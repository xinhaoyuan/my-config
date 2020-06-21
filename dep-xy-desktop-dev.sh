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
    libxcb-xkb-dev
    libxcb-xrm-dev
    libxkbcommon-dev
    libxkbcommon-x11-dev
    libstartup-notification0-dev
    libxdg-basedir-dev
)
    
install_dep xy-desktop-dev "${DEP_ARRAY[@]}"
