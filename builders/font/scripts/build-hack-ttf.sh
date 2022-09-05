#!/bin/bash

set -e

cd /root

(
    git clone https://github.com/source-foundry/Hack
    cd Hack
    # Fix the ttfautohint error inspired by https://github.com/source-foundry/Hack/issues/500#issuecomment-596130056. {
    find postbuild_processing/tt-hinting/ -type f -exec sed -i 's/uni0023/numbersign/g' {} +
    find postbuild_processing/tt-hinting/ -type f -exec sed -i 's/uni0025/percent/g' {} +
    find postbuild_processing/tt-hinting/ -type f -exec sed -i 's/uni002B/plus/g' {} +
    find postbuild_processing/tt-hinting/ -type f -exec sed -i 's/uni0030/zero/g' {} +
    find postbuild_processing/tt-hinting/ -type f -exec sed -i 's/uni0038/eight/g' {} +
    find postbuild_processing/tt-hinting/ -type f -exec sed -i 's/uni0021/exclam/g' {} +
    # }

    make ttf
)

echo "Completed. The output is in /root/Hack/build/ttf."
