#!/bin/bash

set -euo pipefail

[[ -z ${P+x} ]] && P=8

mkdir -p downloads

[[ ! -e downloads/llvm-13.0.0.src ]] && (
    cd downloads
    wget https://github.com/llvm/llvm-project/releases/download/llvmorg-13.0.0/llvm-13.0.0.src.tar.xz -O/tmp/llvm-13.0.0.src.tar.xz
    tar xf /tmp/llvm-13.0.0.src.tar.xz
)

[[ ! -e downloads/llvm-13.0.0.src/tools/clang ]] && (
    cd downloads
    rm -rf clang-13.0.0.src
    wget https://github.com/llvm/llvm-project/releases/download/llvmorg-13.0.0/clang-13.0.0.src.tar.xz -O/tmp/clang-13.0.0.src.tar.xz
    tar xf /tmp/clang-13.0.0.src.tar.xz
    mv clang-13.0.0.src llvm-13.0.0.src/tools/clang
)

mkdir -p install
mkdir -p clang-build

(
    cd clang-build
    cmake ../downloads/llvm-13.0.0.src -DCMAKE_INSTALL_PREFIX=$PWD/../install -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_TERMINFO=OFF -DLLVM_ENABLE_LIBEDIT=OFF -DLLVM_ENABLE_ZLIB=OFF -DLLVM_ENABLE_LIBXML2=OFF -DLLVM_ENABLE_ASSERTIONS=OFF
    make install -j$P
)

mkdir terra-build
(
    cd terra-build
    cmake .. -DCMAKE_INSTALL_PREFIX=$PWD/../install
    make install -j$P
)
