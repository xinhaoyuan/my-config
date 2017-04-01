include ../common.mk

prepare:
	${MAKE} -C bin
	${MAKE} -C .config/fontconfig

install:
	${STOW} -S ${PACKAGE}

uninstall:
	${STOW} -D ${PACKAGE}
