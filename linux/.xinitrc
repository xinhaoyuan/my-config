#!/bin/sh

unset TMUX

exec ck-launch-session dbus-launch ${HOME}/.xinitrc.real
