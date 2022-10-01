FROM debian:testing
RUN apt-get update && apt-get install -y \
  busybox nodejs npm meson python3-pip golang git wget ttfautohint
RUN ln -s /usr/bin/python3 /usr/bin/python
RUN pip3 install fontbakery fontmake fonttools font-v fs ufodiff ufolint
RUN go install github.com/chrissimpkins/ink@latest && cp $HOME/go/bin/ink /usr/bin/ink
RUN go install github.com/source-foundry/uni@latest && cp $HOME/go/bin/uni /usr/bin/uni
COPY scripts /scripts
RUN /scripts/build-iosevka-deps.sh
