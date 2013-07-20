zmodload zsh/terminfo

bindkey -s '\el' '\eqls\n'
bindkey -s '¬' 'ls\eq'
zle -N select-from-cd-stack
select-from-cd-stack() {
  LBUFFER=$LBUFFER"~-"
  zle menu-complete
  if [[ ${LBUFFER[-2,-1]} = "~-" ]]; then
    LBUFFER=${LBUFFER[1,-3]}
  fi
}
bindkey '\ed' select-from-cd-stack
bindkey '∂' select-from-cd-stack
#[[ -n "${key[Up]}" ]] && bindkey "${key[Up]}" history-beginning-search-backward-end
#[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" history-beginning-search-forward-end

bindkey "\e," copy-prev-shell-word
bindkey "≤" copy-prev-shell-word

if [[ "$OSTYPE" == darwin* ]]; then
  bindkey "[5D" backward-word
  bindkey "[5C" forward-word
else
  bindkey "\C-${terminfo[kcub1]}" backward-word
  bindkey "\C-${terminfo[kcuf1]}" forward-word
fi

# red hat's gnome terminal
bindkey '\eOH'    beginning-of-line
bindkey '\eOF'    end-of-line

# history search with globs
bindkey "\C-R" history-incremental-pattern-search-backward
bindkey "\C-S" history-incremental-pattern-search-forward

autoload -U url-quote-magic
zle -N self-insert url-quote-magic

