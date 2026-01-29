function _tide_item_jj
    if command -sq jj; and jj root >/dev/null 2>&1
        # Template logic:
        # 1. Grab first 4 chars of change/commit IDs
        # 2. Add bookmarks if they exist
        # 3. Add "(empty)" tag if commit has no changes
        # 4. If description exists, truncate first line to 10 chars + ellipsis
        set -l jj_template '
            change_id.shortest(4)
            ++ if(bookmarks, " " ++ bookmarks.map(|b| b.name()).join(", "))
            ++ if(empty, " (empty)")
            ++ " " ++ if(description, 
                truncate_end(20, description.first_line(), "…"), 
                "(no description)"
            )
        '

        set -l jj_info (jj log -r @ -n 1 --template $jj_template --no-graph --color never 2>/dev/null | string collect)

        if test -n "$jj_info"
            _tide_print_item jj $tide_jj_icon' ' "$jj_info"
        end
    end
end
