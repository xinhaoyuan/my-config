#!/bin/bash

set -euo pipefail

cd /root

(
    cd /tmp
    wget https://github.com/premake/premake-core/releases/download/v5.0.0-alpha15/premake-5.0.0-alpha15-linux.tar.gz
    tar xvf premake-5.0.0-alpha15-linux.tar.gz 
    mv premake5 /usr/bin
)

(
    wget https://github.com/caryll/otfcc/archive/refs/heads/master.zip -O otfcc-master.zip
    busybox unzip otfcc-master.zip
    cd otfcc-master
    premake5 gmake
    make -C build/gmake config=release_x64
    mv bin/release-x64/otfcc* /usr/bin
)

(
    wget https://github.com/xinhaoyuan/Iosevka/archive/refs/heads/master.zip -O Iosevka-master.zip
    busybox unzip Iosevka-master.zip
    cd Iosevka-master
    npm install
    make archive
)

echo "Completed. The output is under /root/Iosevka-master/dist/latest.tgz."
