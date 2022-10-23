FROM debian:testing
RUN apt-get update && apt-get install -y \
  busybox build-essential cmake git wget python3-pip
RUN ln -s /usr/bin/python3 /usr/bin/python
COPY scripts /scripts
