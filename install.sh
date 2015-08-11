#!/bin/sh

generate_package_names() {
    echo emacsconf
    echo misc
    if [ `uname` = 'Linux' ]; then
	    echo misc-linux
    elif [ `uname` = 'Darwin' ]; then
        echo misc-osx
    elif [ `uname -o` = 'Cygwin' ]; then
        echo misc-cygwin
    fi
}

command -v stow 2>/dev/null 1>&2 || { echo "stow needed."; exit 0; }
cd $(dirname $0)

IFS="$(printf '\n\t')"
for package in `generate_package_names`; do
    if [ -e ${package}/Makefile ]; then
        echo $package
        make -C $package $1 || exit 1
    fi
done
