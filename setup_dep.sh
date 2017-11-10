#!/bin/bash

# this script needs root permission

# basic dependency
apt-get install stow tmux git zsh emacs build-essential consolekit xdotool xclip wmctrl xorg-dev php5-cli pkg-config

# dependency for building awesome
# this is extracted from .travis.yml from awesome-git
sudo apt-get install -y libcairo2-dev gir1.2-gtk-3.0 xmlto asciidoc libpango1.0-dev libxcb-xtest0-dev libxcb-icccm4-dev libxcb-randr0-dev libxcb-keysyms1-dev libxcb-xinerama0-dev libdbus-1-dev libxdg-basedir-dev libstartup-notification0-dev imagemagick libxcb1-dev libxcb-shape0-dev libxcb-util0-dev libx11-xcb-dev libxcb-cursor-dev libxcb-xkb-dev libxkbcommon-dev libxkbcommon-x11-dev
