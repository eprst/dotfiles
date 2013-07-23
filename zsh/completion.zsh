# matches case insensitive for lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# pasting with tabs doesn't perform completion
zstyle ':completion:*' insert-tab pending

autoload -U incremental-complete-word
zle -N incremental-complete-word
bindkey "^Xi" incremental-complete-word

autoload -U insert-files
zle -N insert-files
bindkey "^Xf" insert-files

zle -C expand-java-path complete-word _generic
zstyle ':completion:expand-java-path:*' completer _expand_java_path
zstyle ':completion:expand-java-path:*' menu select yes
bindkey '^x^n' expand-java-path

zle -C complete-files complete-word _generic
zstyle ':completion:complete-files:*' completer _files
zstyle ':completion:complete-files:*' menu select yes
bindkey '^x^f' complete-files

bindkey -M menuselect "/" accept-and-infer-next-history
bindkey -M menuselect "," accept-and-menu-complete

# disable git files completion
# __git_files(){}

#{{{1 tmux screen completion
_tmux_pane_words() {
  local expl
  local -a w
  if [[ -z "$TMUX_PANE" ]]; then
    _message "not running inside tmux!"
    return 1
  fi
  w=( ${(u)=$(tmux capture-pane \; show-buffer \; delete-buffer)} )
  _wanted values expl 'words from current tmux pane' compadd -a w
}

zle -C tmux-pane-words-prefix   complete-word _generic
zle -C tmux-pane-words-anywhere complete-word _generic
bindkey '^Xt' tmux-pane-words-prefix
bindkey '^X^X' tmux-pane-words-anywhere
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' completer _tmux_pane_words
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' ignore-line current
zstyle ':completion:tmux-pane-words-anywhere:*' matcher-list 'b:=* m:{A-Za-z}={a-zA-Z}'
#}}}

# vim:ts=4:foldmethod=marker:expandtab:
