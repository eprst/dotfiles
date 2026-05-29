#!/bin/bash
# Hook script to capture Claude Code current activity
# Handles multiple hook events with clean, color-coded status output

STATE_DIR="/tmp/claude-zellij-status"
ZELLIJ_SESSION="${ZELLIJ_SESSION_NAME:-}"
ZELLIJ_PANE="${ZELLIJ_PANE_ID:-0}"
ZELLIJ_BIN="${ZELLIJ_BIN:-zellij}"
ZELLIJ_TIMEOUT_SECONDS="${ZELLIJ_TIMEOUT_SECONDS:-1}"

# Exit if not in Zellij
[ -z "$ZELLIJ_SESSION" ] && exit 0

STATE_FILE="${STATE_DIR}/${ZELLIJ_SESSION}.json"
mkdir -p "$STATE_DIR"

# Read JSON from stdin
INPUT=$(cat)

# Parse hook event and related fields (with error handling)
HOOK_EVENT=$(echo "$INPUT" | jq -r '.hook_event_name // ""' 2>/dev/null || echo "")
TOOL_NAME=$(echo "$INPUT" | jq -r '.tool_name // ""' 2>/dev/null || echo "")
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // "unknown"' 2>/dev/null || echo "unknown")
CWD=$(echo "$INPUT" | jq -r '.cwd // ""' 2>/dev/null || echo "")

# Exit if we couldn't parse the input
[ -z "$HOOK_EVENT" ] && exit 0

# Get short session ID (last 4 chars for compactness)
SHORT_SESSION="${SESSION_ID: -4}"

run_with_timeout() {
    if command -v timeout >/dev/null 2>&1; then
        timeout "$ZELLIJ_TIMEOUT_SECONDS" "$@"
    else
        "$@"
    fi
}

jj_workspace_name() {
    [ -n "$CWD" ] || return 1
    command -v jj >/dev/null 2>&1 || return 1

    run_with_timeout jj --ignore-working-copy -R "$CWD" log -r @ --no-graph \
        -T 'working_copies.map(|w| w.name()).join(" ") ++ "\n"' 2>/dev/null |
        awk 'NF {print $1; exit}'
}

# Project name disambiguation (local override of upstream basename-only logic):
# - jj workspace                  -> use workspace name (default, .wt/<name>, etc.).
# - legacy .wt/<name>/...         -> use <name> if jj cannot answer.
# - anything else                  -> "<basename>:<short_session>" so multiple
#                                     sessions in the same dir are distinguishable.
JJ_WORKSPACE=$(jj_workspace_name)
if [ -n "$JJ_WORKSPACE" ]; then
    PROJECT_NAME="$JJ_WORKSPACE"
elif [[ "$CWD" =~ /\.wt/([^/]+) ]]; then
    PROJECT_NAME="${BASH_REMATCH[1]}"
else
    PROJECT_NAME="$(basename "$CWD" 2>/dev/null || echo "?"):${SHORT_SESSION}"
fi
# Truncate to 18 chars (upstream was 12; widened to fit the :xxxx suffix).
if [ ${#PROJECT_NAME} -gt 18 ]; then
    PROJECT_NAME="${PROJECT_NAME:0:15}..."
fi

# =============================================================================
# COLOR SCHEME (Gruvbox bright — matches the surrounding zjstatus alias)
# =============================================================================
C_GREEN="#b8bb26"   # gruvbox green       - Done/Complete
C_YELLOW="#fabd2f"  # gruvbox yellow      - Active/Working
C_BLUE="#83a598"    # gruvbox blue        - Reading/Searching, project name
C_AQUA="#8ec07c"    # gruvbox aqua        - Writing/Editing
C_RED="#fb4934"     # gruvbox red         - Needs attention
C_ORANGE="#fe8019"  # gruvbox orange      - Bash
C_PURPLE="#d3869b"  # gruvbox purple      - Agent/Skill
C_GRAY="#a89984"    # gruvbox fg4 (gray)  - Thinking/Idle
C_BG="#665c54"      # gruvbox bg3 — pipe_status segment background, baked
                    # into each render directive so terminal default can't
                    # bleed through between fg directives.

zellij_call() {
    run_with_timeout "$ZELLIJ_BIN" "$@"
}

render_sessions() {
    [ -f "$STATE_FILE" ] || return 0

    jq -r '
        to_entries | sort_by(.key)[] |
        (if (.value.short_title // "") != "" then .value.short_title else .value.project end) as $lbl |
        "#[bg=#665c54,fg=\(.value.color)]\(.value.symbol) #[bg=#665c54,fg=#83a598]\($lbl)" +
        (if .value.context_pct then " #[bg=#665c54,fg=\(.value.ctx_color // "#b8bb26")]\(.value.context_pct)%" else "" end)
    ' "$STATE_FILE" 2>/dev/null
}

publish_status() {
    local sessions=""
    local line

    while IFS= read -r line; do
        [ -z "$line" ] && continue
        [ -n "$sessions" ] && sessions="${sessions}  "
        sessions="${sessions}${line}"
    done < <(render_sessions)

    # Mirror the state file into zellij's per-session tmp dir. zellij sandboxes
    # plugins and maps their guest /tmp to the SERVER's ZELLIJ_TMP_DIR
    # (tempdir()/zellij-$UID = /tmp/zellij-$UID by default), so the agent-picker
    # plugin opening /tmp/claude-zellij-status/<session>.json reads it from
    # THERE. Use the literal /tmp, NOT $TMPDIR: this hook runs inside Claude
    # panes whose $TMPDIR is a sandbox dir (e.g. /tmp/claude-$UID) that differs
    # from the zellij server's. Keep that copy current so the picker shows live
    # data.
    local zj_status_dir="/tmp/zellij-${UID}/claude-zellij-status"
    if [ -f "$STATE_FILE" ] && [ -d "/tmp/zellij-${UID}" ]; then
        mkdir -p "$zj_status_dir" 2>/dev/null &&
            cp "$STATE_FILE" "$zj_status_dir/${ZELLIJ_SESSION}.json" 2>/dev/null || true
    fi

    zellij_call -s "$ZELLIJ_SESSION" pipe "zjstatus::pipe::pipe_status::${sessions}" 2>/dev/null || true

    # Nudge the agent-picker plugin to re-read the state file so its list
    # tracks the bar live. `--name` without `--plugin` broadcasts only to
    # already-running plugins, so this is a no-op (discarded by zellij) when
    # the picker is closed. A payload is passed and stdin is /dev/null so
    # `zellij pipe` never blocks waiting on STDIN.
    zellij_call -s "$ZELLIJ_SESSION" pipe --name refresh -- nudge </dev/null 2>/dev/null || true
}

prune_dead_panes() {
    [ -f "$STATE_FILE" ] || return 0

    local live_panes
    live_panes=$(zellij_call -s "$ZELLIJ_SESSION" action list-panes 2>/dev/null | awk '
        NR > 1 {
            pane = $1
            if (pane ~ /^(terminal|plugin)_[0-9]+$/) {
                sub(/^[^_]+_/, "", pane)
                print pane
            }
        }
    ')

    # If Zellij cannot answer, keep the previous state rather than wiping it.
    [ -z "$live_panes" ] && return 0

    local tmp_file
    tmp_file=$(mktemp)
    if jq --arg live_panes "$live_panes" '
        ($live_panes | split("\n") | map(select(length > 0))) as $live |
        with_entries(select(.key as $pane | $live | index($pane)))
    ' "$STATE_FILE" > "$tmp_file" 2>/dev/null; then
        mv "$tmp_file" "$STATE_FILE"
    else
        rm -f "$tmp_file"
    fi
}

# Generate session-title forms via Haiku, mirroring Claude Code's built-in
# feature (which fails for us because output_format requires a beta header
# that CLAUDE_CODE_DISABLE_EXPERIMENTAL_BETAS=1 strips). Hits the same
# gateway Claude Code does, so it inherits the user's auth/billing.
# On success echoes one line of JSON: {"long":"...","short":"..."} where
# `long` is a 3-7 word sentence-case title for the zellij pane title and
# `short` is a <=16-char hyphen-lowercase label for the zjstatus segment.
# On any failure echoes nothing. Bounded by curl --max-time so a slow
# gateway can't stall the user's prompt indefinitely.
generate_session_title() {
    local prompt="$1"
    [ -n "$prompt" ] || return 1

    local base="${ANTHROPIC_BASE_URL:-https://snowhouse.snowflakecomputing.com/api/v2/cortex/anthropic}"
    local token
    token=$(cat "$HOME/.claude/pat" 2>/dev/null) || return 1
    [ -n "$token" ] || return 1

    local sys_prompt='Generate two forms of a session title for this coding-session prompt.

- "long": 3-7 word sentence-case title (e.g. "Add retry logic to Kafka consumer")
- "short": <=16 character abbreviated label, lowercase with hyphens (e.g. "kafka-retry")

Reply with ONLY a single line of compact JSON, nothing else, no markdown fences:
{"long":"<long title>","short":"<short label>"}'

    local body
    body=$(jq -nc --arg p "$prompt" --arg sys "$sys_prompt" '{
        model: "claude-haiku-4-5",
        max_tokens: 100,
        system: $sys,
        messages: [{role: "user", content: $p}]
    }') || return 1

    local response
    response=$(curl -sS --max-time 10 \
        -H "Authorization: Bearer $token" \
        -H "Content-Type: application/json" \
        -H "anthropic-version: 2023-06-01" \
        "$base/v1/messages" -d "$body" 2>/dev/null) || return 1

    local raw
    raw=$(printf '%s' "$response" | jq -r '.content[0].text // ""' 2>/dev/null)
    [ -n "$raw" ] || return 1

    # Extract the first {long,short} object; tolerate prose or fences around it.
    local json
    json=$(printf '%s' "$raw" | tr '\n' ' ' \
        | grep -oE '\{[^{}]*"long"[^{}]*"short"[^{}]*\}' \
        | head -1)
    [ -n "$json" ] || return 1
    printf '%s' "$json" | jq -e . >/dev/null 2>&1 || return 1

    # Sanitize, clamp, and fall back short -> derived-from-long if missing.
    local long short
    long=$(printf '%s' "$json" | jq -r '.long // ""' \
        | tr '\n\r\t' '   ' | tr -s ' ' | sed 's/^ //; s/ $//' \
        | sed -E 's/^["'"'"']//; s/["'"'"']$//' \
        | cut -c 1-60)
    short=$(printf '%s' "$json" | jq -r '.short // ""' \
        | tr '\n\r\t' '   ' | tr -s ' ' | sed 's/^ //; s/ $//' \
        | sed -E 's/^["'"'"']//; s/["'"'"']$//' \
        | cut -c 1-16)
    [ -n "$long" ] || return 1
    if [ -z "$short" ]; then
        short=$(printf '%s' "$long" | tr '[:upper:] ' '[:lower:]-' | cut -c 1-16)
    fi

    jq -nc --arg l "$long" --arg s "$short" '{long: $l, short: $s}'
}

# =============================================================================
# SYMBOLS
# =============================================================================
# Determine activity, color, and symbol based on hook event
case "$HOOK_EVENT" in
    PreToolUse)
        case "$TOOL_NAME" in
            WebSearch|web_search|search_query)
                             ACTIVITY="search"; COLOR="$C_BLUE";   SYMBOL="◍" ;;
            WebFetch|open)   ACTIVITY="fetch";  COLOR="$C_BLUE";   SYMBOL="↓" ;;
            Task|spawn_agent|wait_agent|close_agent)
                             ACTIVITY="agent";  COLOR="$C_PURPLE"; SYMBOL="▶" ;;
            Bash|exec_command|write_stdin|shell|local_shell)
                             ACTIVITY="bash";   COLOR="$C_ORANGE"; SYMBOL="⚡" ;;
            Read|view_image) ACTIVITY="read";   COLOR="$C_BLUE";   SYMBOL="◔" ;;
            Write)           ACTIVITY="write";  COLOR="$C_AQUA";   SYMBOL="✎" ;;
            Edit|apply_patch)
                             ACTIVITY="edit";   COLOR="$C_AQUA";   SYMBOL="✎" ;;
            Glob|Grep|find|rg)
                             ACTIVITY="find";   COLOR="$C_BLUE";   SYMBOL="◎" ;;
            Skill|tool_search_tool)
                             ACTIVITY="skill";  COLOR="$C_PURPLE"; SYMBOL="★" ;;
            TodoWrite|update_plan)
                             ACTIVITY="plan";   COLOR="$C_YELLOW"; SYMBOL="◫" ;;
            AskUserQuestion|request_user_input)
                             ACTIVITY="ask?";   COLOR="$C_RED";    SYMBOL="?" ;;
            multi_tool_use.parallel)
                             ACTIVITY="multi";  COLOR="$C_PURPLE"; SYMBOL="◈" ;;
            mcp__*)          ACTIVITY="mcp";    COLOR="$C_PURPLE"; SYMBOL="◈" ;;
            *)               ACTIVITY="work";   COLOR="$C_YELLOW"; SYMBOL="●" ;;
        esac
        DONE=false
        ;;
    PostToolUse)
        ACTIVITY="think"; COLOR="$C_GRAY"; SYMBOL="◐"; DONE=false ;;
    Notification)
        # Check if session is already done - don't overwrite completion status
        if [ -f "$STATE_FILE" ]; then
            EXISTING_DONE=$(jq -r --arg pane "$ZELLIJ_PANE" '.[$pane].done // false' "$STATE_FILE" 2>/dev/null || echo "false")
            if [ "$EXISTING_DONE" = "true" ]; then
                # Still send zjstatus notification, but preserve done status
                zellij_call -s "$ZELLIJ_SESSION" pipe "zjstatus::notify::${PROJECT_NAME} ! notification" 2>/dev/null || true
                exit 0
            fi
        fi
        ACTIVITY="!"; COLOR="$C_RED"; SYMBOL="!"; DONE=false ;;
    UserPromptSubmit)
        ACTIVITY="start"; COLOR="$C_YELLOW"; SYMBOL="●"; DONE=false ;;
    PermissionRequest)
        ACTIVITY="perm?"; COLOR="$C_RED"; SYMBOL="⚠"; DONE=false ;;
    Stop)
        ACTIVITY="done"; COLOR="$C_GREEN"; SYMBOL="✓"; DONE=true ;;
    SubagentStop)
        ACTIVITY="agent✓"; COLOR="$C_GREEN"; SYMBOL="▷"; DONE=false ;;
    SessionStart)
        ACTIVITY="init"; COLOR="$C_BLUE"; SYMBOL="◆"; DONE=false ;;
    SessionEnd)
        # Session ended - remove from state
        if [ -f "$STATE_FILE" ]; then
            TMP_FILE=$(mktemp)
            jq --arg pane "$ZELLIJ_PANE" 'del(.[$pane])' "$STATE_FILE" > "$TMP_FILE" 2>/dev/null && mv "$TMP_FILE" "$STATE_FILE"
            rm -f "$TMP_FILE"
        fi
        # Update zjstatus with remaining sessions
        prune_dead_panes
        publish_status
        exit 0
        ;;
    *)
        ACTIVITY="..."; COLOR="$C_GRAY"; SYMBOL="○"; DONE=false ;;
esac

# Current time
TIMESTAMP=$(date +%s)
TIME_FMT=$(date +%H:%M)

# Initialize state file if it doesn't exist or is empty
if [ ! -f "$STATE_FILE" ] || [ ! -s "$STATE_FILE" ]; then
    echo "{}" > "$STATE_FILE"
fi

# Read existing state
CURRENT_STATE=$(cat "$STATE_FILE" 2>/dev/null || echo "{}")

# Validate it's proper JSON
if ! echo "$CURRENT_STATE" | jq empty 2>/dev/null; then
    CURRENT_STATE="{}"
    echo "{}" > "$STATE_FILE"
fi

prune_dead_panes
CURRENT_STATE=$(cat "$STATE_FILE" 2>/dev/null || echo "{}")

# Get existing values for this pane (preserve context data from status line script)
EXISTING=$(echo "$CURRENT_STATE" | jq -r --arg pane "$ZELLIJ_PANE" '.[$pane] // {}' 2>/dev/null)
EXISTING_CTX_PCT=$(echo "$EXISTING" | jq -r '.context_pct // null' 2>/dev/null)
EXISTING_CTX_COLOR=$(echo "$EXISTING" | jq -r '.ctx_color // null' 2>/dev/null)
EXISTING_SESSION_ID=$(echo "$EXISTING" | jq -r '.session_id // ""' 2>/dev/null)
# Only carry short_title across hook fires if the session id is unchanged.
# A new session in the same pane (claude/codex relaunched, /clear, etc.)
# starts with no title so the next UserPromptSubmit regenerates one.
if [ "$EXISTING_SESSION_ID" = "$SESSION_ID" ]; then
    EXISTING_SHORT_TITLE=$(echo "$EXISTING" | jq -r '.short_title // null' 2>/dev/null)
else
    EXISTING_SHORT_TITLE=null
fi

# Update state with this pane's activity (preserving context from status line)
TMP_FILE=$(mktemp)
echo "$CURRENT_STATE" | jq \
    --arg pane "$ZELLIJ_PANE" \
    --arg project "$PROJECT_NAME" \
    --arg activity "$ACTIVITY" \
    --arg color "$COLOR" \
    --arg symbol "$SYMBOL" \
    --arg time "$TIME_FMT" \
    --arg ts "$TIMESTAMP" \
    --arg short_session "$SHORT_SESSION" \
    --arg session "$SESSION_ID" \
    --arg ctx_pct "$EXISTING_CTX_PCT" \
    --arg ctx_color "$EXISTING_CTX_COLOR" \
    --arg short_title "$EXISTING_SHORT_TITLE" \
    --argjson done "$DONE" \
    '.[$pane] = {
        project: $project,
        activity: $activity,
        color: $color,
        symbol: $symbol,
        time: $time,
        timestamp: ($ts | tonumber),
        short_session: $short_session,
        session_id: $session,
        context_pct: (if $ctx_pct == "null" then null else $ctx_pct end),
        ctx_color: (if $ctx_color == "null" then null else $ctx_color end),
        short_title: (if $short_title == "null" then null else $short_title end),
        done: $done
    }' > "$TMP_FILE" 2>/dev/null

if [ -s "$TMP_FILE" ]; then
    mv "$TMP_FILE" "$STATE_FILE"
else
    rm -f "$TMP_FILE"
fi

publish_status

# Per-event final dispatch: zjstatus notifications, hook-output JSON, etc.
case "$HOOK_EVENT" in
    Notification|Stop|SubagentStop|AskUserQuestion|PermissionRequest)
        zellij_call -s "$ZELLIJ_SESSION" pipe "zjstatus::notify::${PROJECT_NAME} ${SYMBOL} ${ACTIVITY}" 2>/dev/null || true
        ;;
    UserPromptSubmit)
        # Workaround for broken server-side title generation under
        # CLAUDE_CODE_DISABLE_EXPERIMENTAL_BETAS=1: that env var strips the
        # output_format beta header generateSessionTitle relies on, so the
        # call fails and the title falls through to "Claude Code". We
        # replicate it locally with one Haiku call that returns both a
        # 3-7 word "long" title (claude only: emitted via
        # hookSpecificOutput.sessionTitle, same path as /rename, becomes the
        # zellij pane title) and a <=16-char "short" label (both agents:
        # stashed in STATE_FILE so render_sessions shows it as the zjstatus
        # segment). Codex emits its own OSC 0/2 titles for the pane so we
        # don't override that — only the zjstatus label is updated for it.
        # Synchronous: pays ~1-2s on the first prompt of a session only.
        # Async via `zellij action rename-pane` doesn't work because that
        # command targets the focused pane, not this one.
        # Lock-to-first: skip if Claude already reports a session_title
        # (claude /rename, /resume) OR our state already has a short_title
        # for the current session (covers codex which doesn't expose
        # session_title).
        EXISTING_TITLE=$(printf '%s' "$INPUT" | jq -r '.session_title // ""' 2>/dev/null)
        if [ -z "$EXISTING_TITLE" ] && [ "$EXISTING_SHORT_TITLE" = "null" ]; then
            PROMPT=$(printf '%s' "$INPUT" | jq -r '.prompt // ""' 2>/dev/null)
            if [ -n "$PROMPT" ]; then
                LONG=""
                SHORT=""
                TITLES=$(generate_session_title "$PROMPT")
                if [ -n "$TITLES" ]; then
                    LONG=$(printf '%s' "$TITLES" | jq -r '.long // ""' 2>/dev/null)
                    SHORT=$(printf '%s' "$TITLES" | jq -r '.short // ""' 2>/dev/null)
                fi
                # Fallbacks for Haiku failure (gateway down, auth expired,
                # curl absent, timeout, malformed JSON).
                if [ -z "$LONG" ]; then
                    LONG=$(printf '%s' "$PROMPT" | tr '\n\r\t' '   ' | tr -s ' ' | sed 's/^ //; s/ $//' | cut -c 1-60)
                fi
                if [ -z "$SHORT" ]; then
                    SHORT=$(printf '%s' "$LONG" | tr '[:upper:] ' '[:lower:]-' | cut -c 1-16)
                fi
                # Persist short_title in STATE_FILE so render_sessions picks
                # it up; republish so the zjstatus segment refreshes.
                if [ -n "$SHORT" ]; then
                    TMP_FILE=$(mktemp)
                    if jq --arg pane "$ZELLIJ_PANE" --arg short "$SHORT" \
                        '.[$pane].short_title = $short' \
                        "$STATE_FILE" > "$TMP_FILE" 2>/dev/null; then
                        mv "$TMP_FILE" "$STATE_FILE"
                        publish_status
                    else
                        rm -f "$TMP_FILE"
                    fi
                fi
                # Pane-title emit is claude-only: codex doesn't honor the
                # sessionTitle field on hookSpecificOutput, and codex sets
                # its own pane title via OSC 0/2 anyway.
                if [ -n "$LONG" ] && [ "${CLAUDECODE:-}" = "1" ]; then
                    jq -nc --arg t "$LONG" \
                        '{hookSpecificOutput: {hookEventName: "UserPromptSubmit", sessionTitle: $t}}'
                fi
            fi
        fi
        ;;
esac
