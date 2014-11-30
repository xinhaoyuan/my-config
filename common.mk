.PHONY: prepare install uninstall

PACKAGE := $(notdir $(abspath $(shell pwd)))
STOW := stow -d .. -t ${HOME} --ignore="^Makefile$$" --no-folding
