#!/bin/bash

set -e

cd /root

(
    cd /tmp
    wget https://github.com/premake/premake-core/releases/download/v5.0.0-alpha15/premake-5.0.0-alpha15-linux.tar.gz
    tar xvf premake-5.0.0-alpha15-linux.tar.gz 
    mv premake5 /usr/bin
)

(
    git clone https://github.com/caryll/otfcc
    cd otfcc
    premake5 gmake
    make -C build/gmake config=release_x64
    mv bin/release-x64/otfcc* /usr/bin
)

(
    git clone https://github.com/xinhaoyuan/iosevka
    npm install
    npm run build -- ttf::iosevka-xy
)

echo "Completed. The output is under /root/iosevka/dist/iosevka-xy."
