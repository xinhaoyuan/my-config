#!/bin/sh

generate_package_names() {
    echo emacsconf
    echo misc
    if [ `uname` = 'Linux' ]; then
	    echo misc-linux
    elif [ `uname` = 'Darwin' ]; then
        echo misc-osx
    fi
}

command -v stow 2>/dev/null 1>&2 || { echo "stow needed."; exit 0; }
cd $(dirname $0)

if [ "$1" = 'install' ]; then OP="-S"
elif [ "$1" = 'uninstall' ]; then OP="-D"
else 
    echo "install/uninstall?"
    exit 1
fi

generate_package_names | while read package; do
    echo "$1" $package
    stow $OP $package
done
