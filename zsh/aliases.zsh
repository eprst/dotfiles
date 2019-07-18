alias reload!='. ~/.zshrc'
# alias ls='ls -GFC'
alias cdd='cd +1'
alias g='git'
alias st='git status'
alias gl='git log --branches --remotes --tags --graph --decorate'
alias -g G='| grep'
alias -g L='| less'
alias -g D='| colordiff | less -R'
alias -g P='| python -m json.tool'
alias -g '...'='../..'
alias -g '....'='../../..'
alias -g '.....'='../../../..'

autoload -U zmv
alias zcp='noglob zmv -W -C'
alias zmvv='noglob zmv -W -M'

if [[ "$OSTYPE" == darwin* ]]; then
  alias cal='gcal -m .'
else
  alias cal='TERM="xterm-256color" cal -m -3'
fi

alias v='vim'
alias m='mutt'
alias oi='offlineimap'
alias socks='ssh -D 8080 -q -C -N -p 2212 kos@sobolev.io'
