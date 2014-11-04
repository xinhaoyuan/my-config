.PHONY: prepare install uninstall

PACKAGE := $(notdir $(abspath $(shell pwd)))
STOW := stow -d .. -t ../.. --ignore="^Makefile$$"
