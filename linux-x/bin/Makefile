.PHONY: all clean

BIN := # xinq osd-box-sel

all: ${BIN}

# .gitignore: Makefile
# 	-rm $@
# 	for i in ${BIN}; do echo $$i >> $@; done

xinq: xinq.c
	${CC} -I/usr/include/X11 -I/usr/X11R6/include $< -o $@ -lXinerama -lX11

osd-box-sel: osd-box-sel.cpp
	${CXX} -o $@ $< `pkg-config --cflags --libs x11 xext`

clean:
	-rm ${BIN}
