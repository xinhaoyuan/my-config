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
)

install_dep xy-desktop "${DEP_ARRAY[@]}"
