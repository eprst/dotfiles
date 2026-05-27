function _rbwf_preview --description 'Render an rbw entry for rbwf, with the password masked'
    set -l name $argv[1]
    # Empty list -> expands to zero args, so a missing/empty user is omitted cleanly.
    set -l rest
    test (count $argv) -ge 2; and test -n "$argv[2]"; and set rest $argv[2]

    rbw get --raw --full $name $rest 2>/dev/null | jq -r '
        "Name:     \(.name)",
        "Folder:   \(.folder // "-")",
        "Username: \(.data.username // "-")",
        (.data.uris // [] | .[] | "URI:      \(.uri)"),
        "Password: ********  (enter to copy)",
        (if .data.totp then "TOTP:     set  (ctrl-t to copy)" else empty end),
        ((.fields // [])[] | "Field:    \(.name): \(.value)"),
        (if (.notes // "") != "" then "", "Notes:", .notes else empty end)
    '
end
