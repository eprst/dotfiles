export HISTSIZE=1000
export SAVEHIST=1000
export HISTFILE=$HOME/.zsh_history
# setopt inc_append_history
unsetopt share_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_find_no_dups
