zmodload zsh/terminfo

zle -N select-from-cd-stack
select-from-cd-stack() {
  LBUFFER=$LBUFFER"~-"
  zle menu-complete
  if [[ ${LBUFFER[-2,-1]} = "~-" ]]; then
    LBUFFER=${LBUFFER[1,-3]}
  fi
}
#[[ -n "${key[Up]}" ]] && bindkey "${key[Up]}" history-beginning-search-backward-end
#[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" history-beginning-search-forward-end

if [[ "$OSTYPE" == darwin* ]]; then
  bindkey "[5D" backward-word
  bindkey "[5C" forward-word
  bindkey -s '¬' '\eqls\n'
  bindkey '∂' select-from-cd-stack
  bindkey "≤" copy-prev-shell-word
else
#  bindkey "\C-${terminfo[kcub1]}" backward-word
#  bindkey "\C-${terminfo[kcuf2]}" forward-word
  # tmux
  bindkey "[D" backward-word
  bindkey "[C" forward-word

  bindkey -s '\el' '\eqls\n'
  bindkey '\ed' select-from-cd-stack
  bindkey "\e," copy-prev-shell-word
# red hat's gnome terminal
  bindkey '\eOH'    beginning-of-line
  bindkey '\eOF'    end-of-line
fi
# xfce4-terminal and iterm
bindkey "[1;5D" backward-word
bindkey "[1;5C" forward-word

# history search with globs
bindkey "\C-R" history-incremental-pattern-search-backward
bindkey "\C-S" history-incremental-pattern-search-forward

autoload -U url-quote-magic
zle -N self-insert url-quote-magic

