set -gx EDITOR nvim
set -gx VISUAL nvim

if status is-interactive
    # Commands to run in interactive sessions can go here
    switch (uname)
        case Linux
            # rebind alt-arrows to ctrl-arrows
            bind \e\[1\;5D prevd-or-backward-word
            bind \e\[1\;5C nextd-or-forward-word
            # abbrevs to change Konsole theme
            abbr kl konsoleprofile colors=SolarizedLight
            abbr kd konsoleprofile colors=Solarized
        case Darwin
            eval "$(brew shellenv)"
    end

    # rebind push_line to ctrl-q
    push-line_key_bindings_uninstall
    bind \cq 'push_line'

    # crtl-l for clear
    bind \cl 'clear; commandline -f repaint'

    # remap fzf bindings to use Ctrl
    # fzf_configure_bindings --directory=\cF --git_log=\cG --git_status=\cS --processes=\cP
    fzf_configure_bindings --directory=\cF --git_status=\cS --processes=\cP
    # Ctrl-g for lazygit
    bind \cg lazygit
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
abbr -a gt 'git log --graph --decorate --oneline $(git rev-list -g --all)'
abbr -a br 'git branch -v | cat -'
abbr -a gco 'git checkout'
abbr -a gid 'git diff --no-ext-diff --cached'
abbr -a gwd 'git diff --no-ext-diff'
abbr -a gpr 'git push review'
abbr -a gfr 'git pull --rebase'
abbr -a v 'nvim'

abbr -a --position anywhere G '| grep'
abbr -a --position anywhere L '| less'
abbr -a --position anywhere psg 'ps auxw | grep'

alias gs="git branch | fzf --preview 'git log -p master..{-1} --color=always {-1}' | cut -c 3- | xargs git switch"
alias gbd="git branch | fzf -m --preview 'git log -p master..{-1} --color=always {-1}' | cut -c 3- | xargs git branch -D"

function ftr
    find . -type f -iregex ".*$argv[1].*"
end

switch (uname)
    case Linux
        abbr -a cal 'TERM="xterm-256color" cal -m -3'
        abbr -a caly 'TERM="xterm-256color" cal -m -y'
    case Darwin
        abbr -a cal 'gcal -m .'
end

[ -f ~/observe-venv/bin/activate.fish ] && . ~/observe-venv/bin/activate.fish

# Added by Antigravity
fish_add_path /Users/konstantin/.antigravity/antigravity/bin
export PATH="$HOME/.local/bin:$PATH"
