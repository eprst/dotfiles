zmodload zsh/terminfo

zle -N select-from-cd-stack
select-from-cd-stack() {
  LBUFFER=$LBUFFER"~+"
  zle menu-complete
  if [[ ${LBUFFER[-2,-1]} = "~+" ]]; then
    LBUFFER=${LBUFFER[1,-3]}
  fi
}

if [[ "$OSTYPE" == darwin* ]]; then
  bindkey "[5D" backward-word
  bindkey "[5C" forward-word
  bindkey -s '¬' '\eqls\n'
  bindkey -s 'l' '\eqls\n'
  bindkey '∂' select-from-cd-stack
  bindkey "≤" copy-prev-shell-word
  bindkey '\ed' select-from-cd-stack # see http://superuser.com/questions/449378/how-to-bindkey-altkey-in-osx
  bindkey "\e," copy-prev-shell-word
else
#  bindkey "\C-${terminfo[kcub1]}" backward-word
#  bindkey "\C-${terminfo[kcuf2]}" forward-word
  bindkey -s '\el' '\eqls\n'
  bindkey '\ed' select-from-cd-stack
  bindkey "\e," copy-prev-shell-word
# red hat's gnome terminal
  bindkey '\eOH'    beginning-of-line
  bindkey '\eOF'    end-of-line
fi
bindkey "[A" history-substring-search-up
bindkey "[B" history-substring-search-down
# xfce4-terminal
bindkey "[1;5D" backward-word
bindkey "[1;5C" forward-word
bindkey "Od" backward-word
bindkey "Oc" forward-word

# tmux
if [[ "$TERM" == screen-256color ]]; then
  bindkey "[D" backward-word
  bindkey "[C" forward-word

  # tmux in xfce terminal (caution, this often breaks!)
  # bindkey "OD" backward-word
  # bindkey "OC" forward-word
fi

# history search with globs
bindkey "\C-R" history-incremental-pattern-search-backward
bindkey "\C-S" history-incremental-pattern-search-forward

autoload -U url-quote-magic
zle -N self-insert url-quote-magic

bindkey '\e.' insert-last-word
