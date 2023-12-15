set -gx EDITOR nvim

if status is-interactive
    # Commands to run in interactive sessions can go here
    switch (uname)
        case Darwin
            eval "$(brew shellenv)"
    end

    # rebind push_line to ctrl-q
    push-line_key_bindings_uninstall
    bind \cq 'push_line'

    # crtl-l for clear
    bind \cl 'clear; commandline -f repaint'

    # remap fzf bindings to use Ctrl
    fzf_configure_bindings --directory=\cF --git_log=\cG --git_status=\cS --processes=\cP
    # also add additional mappings to open vim and scroll preview
    set fzf_directory_opts --bind "ctrl-o:execute($EDITOR {} &> /dev/tty)"
    set fzf_git_log_opts --bind "ctrl-j:preview-down" --bind "ctrl-k:preview-up" --bind "ctrl-d:preview-page-down" --bind "ctrl-u:preview-page-up"
    set fzf_git_status_opts --bind "ctrl-j:preview-down" --bind "ctrl-k:preview-up" --bind "ctrl-d:preview-page-down" --bind "ctrl-u:preview-page-up"
end

set -U fish_greeting
set -g tide_right_prompt_items status cmd_duration context jobs direnv node python rustc java php pulumi ruby go gcloud distrobox toolbox terraform aws nix_shell crystal elixir time

abbr -a g git
abbr -a st 'git status'
abbr -a gl "git --no-pager log --pretty=format:'%C(yellow)%h %Cred%ad %Cblue%<(20)%an%Cgreen%d %Creset%s' --date=short"
abbr -a br 'git branch -v | cat -'
abbr -a gco 'git checkout'
abbr -a gid 'git diff --no-ext-diff --cached'
abbr -a gwd 'git diff --no-ext-diff'
abbr -a v 'nvim'

abbr -a --position anywhere G '| grep'
abbr -a --position anywhere L '| less'
abbr -a --position anywhere psg 'ps auxw | grep'

switch (uname)
    case Linux
        abbr -a cal 'TERM="xterm-256color" cal -m -3'
        abbr -a caly 'TERM="xterm-256color" cal -m -y'
    case Darwin
        abbr -a cal 'gcal -m .'
end
