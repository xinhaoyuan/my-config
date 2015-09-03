#!/bin/sh

if git pull; then
    make uninstall
    make install
fi

