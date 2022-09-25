#!/bin/bash

set -e

. libinstdep.sh

DEP_ARRAY=(
    # screen
    autorandr
    arandr
    nitrogen
    picom
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
    blueman
    lxappearance
    qt5ct
    qt5-gtk-platformtheme
    qt5-style-plugins
    x11-utils
    xclip
    brightnessctl
    # awesome needs these
    librsvg2-dev
    fswatch
    # oomox
    sassc
    librsvg2-bin
    gtk2-engines-murrine
)

install_dep xy-desktop "${DEP_ARRAY[@]}"
