#!/bin/bash

set -e

. libinstdep.sh

DEP_ARRAY=(
    # screen
    autorandr
    arandr
    nitrogen
    # hotkeys
    xcape
    # im
    fcitx5
    fcitx5-pinyin
    # launcher
    rofi
    dex
    # fun
    fortune
    # util
    cbatticon
    alsa-utils
    pasystray
    pavucontrol
    pcmanfm
    mousepad
    evtest
    xinput
    xdotool
    network-manager-gnome
    lxappearance
    x11-utils
    xclip
    # awesome needs these
    librsvg2-dev
    fswatch
)

install_dep xy-desktop "${DEP_ARRAY[@]}"
