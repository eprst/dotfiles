if status is-interactive
    # Commands to run in interactive sessions can go here
    switch (uname)
        case Darwin
            eval "$(brew shellenv)"
    end
end

set -U fish_greeting
set -g tide_right_prompt_items status cmd_duration context jobs direnv node python rustc java php pulumi ruby go gcloud distrobox toolbox terraform aws nix_shell crystal elixir time
set -gx EDITOR nvim

abbr -a g git
abbr -a st 'git status'
abbr -a gl "git --no-pager log --pretty=format:'%C(yellow)%h %Cred%ad %Cblue%<(20)%an%Cgreen%d %Creset%s' --date=short"
abbr -a br 'git branch -v | cat -'
abbr -a gco 'git checkout'
abbr -a gid 'git diff --no-ext-diff --cached'
abbr -a gwd 'git diff --no-ext-diff'

abbr -a --position anywhere G '| grep'
abbr -a --position anywhere L '| less'
abbr -a --position anywhere psg 'ps auxw | grep'

abbr -a v 'nvim'

# rebind push_line to ctrl-q
push-line_key_bindings_uninstall
bind \cq 'push_line'
# remap fzf bindings to use Ctrl
fzf_configure_bindings --directory=\cF --git_log=\cL --git_status=\cS --processes=\cP

switch (uname)
    case Linux
        abbr -a cal 'TERM="xterm-256color" cal -m -3'
        abbr -a caly 'TERM="xterm-256color" cal -m -y'
    case Darwin
        abbr -a cal 'gcal -m .'
end
