#!/bin/bash

set -e

. libinstdep.sh

DEP_ARRAY=(
    # dev common
    build-essential
    # st dev
    libfontconfig-dev
    libx11-dev
    libxft-dev
    libharfbuzz-dev
    # awesome dev
    cmake
    liblua5.3-dev
    lua-ldoc
    lua-lgi
    imagemagick
    libglib2.0-dev
    libgdk-pixbuf2.0-dev
    libcairo2-dev
    libxcb-cursor-dev
    libxcb-randr0-dev
    libxcb-xtest0-dev
    libxcb-xinerama0-dev
    libxcb-shape0-dev
    libxcb-util0-dev
    libxcb-keysyms1-dev
    libxcb-icccm4-dev
    libxcb-xfixes0-dev
    libxcb-composite0-dev
    libxcb-damage0-dev
    libxcb-xkb-dev
    libxcb-xrm-dev
    libxkbcommon-dev
    libxkbcommon-x11-dev
    libstartup-notification0-dev
    libxdg-basedir-dev
    libdbus-1-dev
    # seems required for awesomewm testing
    dbus-x11
    # oomox
    libgtk-3-dev
    python3-pil
    inkscape
    # luakit
    libwebkit2gtk-4.0-dev
    # Better build local luajit ..
    # libluajit-5.1-dev
    # luajit
)
    
install_dep xy-desktop-dev "${DEP_ARRAY[@]}"
