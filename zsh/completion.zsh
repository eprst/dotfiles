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

# disable git files completion
__git_files(){}
