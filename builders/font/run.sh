#!/bin/bash

docker run -it --rm --mount type=bind,source=`pwd`,target=/workspace --user "$(id -u ${USER}):$(id -g ${USER})" --env HOME=/workspace --workdir=/workspace font-builder:latest "$@"
