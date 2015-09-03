.PHONY: all install uninstall sync

all:
	./install.sh prepare

install: all
	./install.sh install

uninstall:
	./install.sh uninstall

sync:
	./sync.sh
