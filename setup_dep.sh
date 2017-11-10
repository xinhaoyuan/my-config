#!/bin/bash

# this script needs root permission

# basic dependency
apt-get install -y stow tmux git zsh emacs build-essential consolekit xdotool xclip wmctrl xorg-dev php5-cli pkg-config

# dependency for building awesome
apt-get install -y liblua5.3-dev lua-lgi libgdk-pixbuf-2.0-dev libxcb-xrm-dev
# this is extracted from .travis.yml from awesome-git
apt-get install -y \
        libcairo2-dev libxdg-basedir-dev libstartup-notification0-dev imagemagick \
        gir1.2-gtk-3.0 libdbus-1-dev libpango1.0-dev libxkbcommon-dev libxkbcommon-x11-dev \
        libxcb1-dev libx11-xcb-dev libxcb-xtest0-dev libxcb-icccm4-dev libxcb-randr0-dev libxcb-keysyms1-dev libxcb-xinerama0-dev libxcb-shape0-dev libxcb-util0-dev libxcb-cursor-dev libxcb-xkb-dev
