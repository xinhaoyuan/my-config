#!/bin/bash

PKGS_UTILS="xdotool xclip wmctrl xorg-dev rofi rxvt-unicode-256color xcape compton \
cbatticon fortune nitrogen lxappearance"

PKGS_AWESOME_SRC="libcairo2-dev libxdg-basedir-dev libstartup-notification0-dev imagemagick \
gir1.2-gtk-3.0 libdbus-1-dev libpango1.0-dev libxkbcommon-dev libxkbcommon-x11-dev \
libxcb1-dev libx11-xcb-dev libxcb-xtest0-dev libxcb-icccm4-dev libxcb-randr0-dev libxcb-keysyms1-dev libxcb-xinerama0-dev libxcb-shape0-dev libxcb-util0-dev libxcb-cursor-dev libxcb-xkb-dev \
libxcb-xfixes0-dev libxcb-xrm-dev libgdk-pixbuf2.0-dev \
liblua5.3-dev lua5.3 lua-lgi-dev"

sudo apt-get install -y $PKG_UTILS
sudo apt-get install -y $PKG_AWESOME_SRC
