#!/bin/sh

if git fetch; then
    git pull
    make uninstall
    make install
fi

