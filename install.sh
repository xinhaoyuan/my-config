#!/bin/sh

command -v stow 2>/dev/null 1>&2 || { echo "stow needed."; exit 0; }
cd $(dirname $0)

stow emacsconf
stow misc

if [ `uname` = 'linux' ]; then
	stow misc-linux
fi
