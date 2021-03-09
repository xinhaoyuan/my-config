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
    pasystray
    pcmanfm
    mousepad
    evtest
    xinput
    # awesome needs this
    librsvg2-dev
)

install_dep xy-desktop "${DEP_ARRAY[@]}"
