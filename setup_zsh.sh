#!/bin/sh

set -e

git clone https://github.com/robbyrussell/oh-my-zsh.git $HOME/.oh-my-zsh
cp $HOME/.oh-my-zsh/templates/zshrc.zsh-template $HOME/.zshrc
echo 'source $HOME/.zshrc.user' >> $HOME/.zshrc
