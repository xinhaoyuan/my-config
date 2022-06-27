#!/bin/sh

# For some reason fcitx5 freezes openbox.
export NO_FCITX=1

exec $HOME/.xdesktoprc
