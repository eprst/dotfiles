source $HOME/.dotfiles/zsh/fasd/fasd
init_args=(zsh-hook)
if zstyle -t ':prezto:module:completion' loaded; then
  init_args+=(zsh-ccomp zsh-ccomp-install zsh-wcomp zsh-wcomp-install)
fi
eval "$(fasd --init $init_args[@])"
unset init_args
