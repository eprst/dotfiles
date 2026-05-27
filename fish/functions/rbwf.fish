function rbwf --description 'Fuzzy-search rbw (Bitwarden); copy login/password to clipboard'
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
