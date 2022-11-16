alias reload!='. ~/.zshrc'
# alias ls='ls -GFC'
alias cdd='cd +1'
alias g='git'
alias st='git status'
alias gl='git log --branches --remotes --tags --graph --decorate --stat'
alias br='git branch -v | cat -'
alias gpr='git push review'
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
alias nv='nvim'
# alias m='mutt'
# alias oi='offlineimap'
alias socks='ssh -D 8080 -q -C -N -p 2212 kos@sobolev.io'
alias psg='ps auxw | grep'
alias video='gphoto2 --set-config=/main/actions/autofocus=1 && gphoto2 --stdout --capture-movie | ffmpeg -hwaccel nvdec -c:v mjpeg_cuvid -i - -vcodec rawvideo -pix_fmt yuv420p -threads 0 -f v4l2 /dev/video0'
