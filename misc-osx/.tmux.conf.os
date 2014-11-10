set-option -g default-shell /usr/local/bin/zsh
# fix the namespace problem of osx
set-option -g default-command "exec reattach-to-user-namespace zsh"
