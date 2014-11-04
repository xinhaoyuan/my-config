include ../common.mk

STOW := ${STOW} --ignore=".config/iterm2/.*"

prepare:

install:
	@echo Link .config/iterm2 to target
	@(	if [ -e ../../.config/iterm2 ]; then exit 0; fi;\
		LPATH=$(abspath $(shell pwd));\
		LPATH=$${LPATH#$${LPATH%/*/*}/};\
		mkdir -p ../../.config;\
		cd ../../.config;\
		ln -s ../$${LPATH}/.config/iterm2;\
	)
	${STOW} -S ${PACKAGE}

uninstall:
	${STOW} -D ${PACKAGE}
