#!/bin/bash

unset TMUX

# X basic configure

#xrandr --output DFP2 --right-of DFP1
#xrandr --output DFP2 --primary

eval `dbus-launch --auto-syntax`

xrandr --output DVI-1 --right-of DVI-0
xrandr --output DVI-1 --primary

xrdb ~/.Xdefaults
xset fp+ ~/.fonts
xset m 1 10

fcitx
compton -b

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"

# start WM
export PATH="$HOME/xbin:$PATH"
exec openbox --startup "$HOME/.xdesktoprc"

