#!/bin/bash

set -e

function join_by {
    local IFS="$1"
    shift
    echo "$*"
}


DEP_ARRAY=(
    # common
    build-essential
    # st
    libfontconfig-dev
    libx11-dev
    libxft-dev
    # awesome
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
    
DEPS=$(join_by , "${DEP_ARRAY[@]}")

[ -z "$TEMP_DIR" ] && TEMP_DIR=/tmp/xy-desktop-dev-workspace-$$
mkdir $TEMP_DIR

mkdir -p $TEMP_DIR/xy-desktop-dev/DEBIAN
cat >$TEMP_DIR/xy-desktop-dev/DEBIAN/control <<EOF
Package: xy-desktop-dev
Maintainer: Xinhao Yuan
Version: 1.0
Architecture: all
Depends: $DEPS
Description: Development dependency of Xinhao Yuan's desktop configuration. 
EOF
dpkg-deb -b $TEMP_DIR/xy-desktop-dev
sudo dpkg -i $TEMP_DIR/xy-desktop-dev.deb || sudo apt-get --fix-broken install
rm -rf $TEMP_DIR
