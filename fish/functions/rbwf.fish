function rbwf --description 'Fuzzy-search rbw (Bitwarden); copy login/password to clipboard'
    # Unlock the vault BEFORE fzf takes over the screen, otherwise pinentry
    # paints on top of the fzf UI. After this, `rbw list` and the preview's
    # `rbw get` calls all run against an already-unlocked agent.
    if not rbw unlocked 2>/dev/null
        rbw unlock; or return
    end

    # name<TAB>user for every entry, fuzzy-pick with fzf.
    # Preview shows the full entry. Keys decide what gets copied.
    set -l out (rbw list --fields name,user | \
        fzf --delimiter \t \
            --with-nth 1,2 \
            --preview '_rbwf_preview {1} {2}' \
            --preview-window 'right,55%,wrap' \
            --expect 'ctrl-u,ctrl-t' \
            --header 'enter: copy password   ctrl-u: copy username   ctrl-t: copy TOTP' \
            --prompt 'bw> ')

    # fzf queries the terminal's background color (OSC 11) on startup; when we
    # exit fast (e.g. Esc), the reply arrives after fzf has restored cooked
    # mode and bytes like `]11;rgb:0d0d/2a2a/3434` leak into the next prompt.
    # Swallow any pending TTY bytes here. begin/end + redirect makes `read`
    # read from /dev/tty regardless of how rbwf was called.
    begin
        while read --timeout 0.05 --nchars 1 --silent --local _
        end
    end </dev/tty 2>/dev/null

    # element 1 = pressed key (empty for plain enter), element 2 = selected row
    test (count $out) -lt 2; and return

    set -l key $out[1]
    set -l fields (string split \t -- $out[2])
    set -l name $fields[1]
    set -l user ""
    test (count $fields) -ge 2; and set user $fields[2]

    switch $key
        case ctrl-u
            printf '%s' $user | pbcopy
            echo "Copied username for '$name': $user"
        case ctrl-t
            if test -n "$user"
                rbw code $name $user | pbcopy
            else
                rbw code $name | pbcopy
            end
            echo "Copied TOTP for '$name'"
        case '*'
            if test -n "$user"
                rbw get $name $user | pbcopy
            else
                rbw get $name | pbcopy
            end
            echo "Copied password for '$name'"
    end
end
