brew install reattach-to-user-namespace --wrap-pbcopy-and-pbpaste
brew install mc vim wget zsh tmux aria2 ag gcal tig htop w-calc ctags dialog
brew cask install macvim --override-system-vim
brew cask install speedcrunch

# nerd fonts
brew tap homebrew/cask-fonts
brew cask install font-hack-nerd-font
brew cask install font-firacode-nerd-font-mono
brew cask install font-firacode-nerd-font

# neovim
brew install https://raw.githubusercontent.com/equalsraf/homebrew-neovim-qt/master/neovim-qt.rb
brew install python3
pip3 install neovim --upgrade
