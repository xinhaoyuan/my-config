#!/bin/bash

set -euo pipefail

cd /root
wget https://github.com/xinhaoyuan/Iosevka/archive/refs/heads/master.zip -O Iosevka-master.zip
busybox unzip Iosevka-master.zip
cd Iosevka-master
npm install
make archive

echo "Completed. The output is under /root/Iosevka-master/dist/latest.tgz."
