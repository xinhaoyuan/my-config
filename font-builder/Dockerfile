FROM debian:testing
RUN apt-get update && apt-get install -y \
  nodejs npm meson python3-pip golang git wget \
  ttfautohint
RUN ln -s /usr/bin/python3 /usr/bin/python
RUN pip3 install fontbakery fontmake fonttools font-v fs ufodiff ufolint
RUN go get github.com/chrissimpkins/ink && cp $HOME/go/bin/ink /usr/bin/ink
RUN go get github.com/source-foundry/uni && cp $HOME/go/bin/uni /usr/bin/uni
COPY scripts /scripts
