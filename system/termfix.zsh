if [ \( "x$COLORTERM" = "xgnome-terminal" -o "x$COLORTERM" = "xTerminal" -o "x$COLORTERM" = "xxfce4-terminal" \) -a "x$TERM" = "xxterm" ] && infocmp xterm-256color >/dev/null 2>&1; then
  export TERM=xterm-256color
fi
