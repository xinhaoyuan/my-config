.PHONY: all install uninstall

all:
	./install.sh prepare

install: all
	./install.sh install

uninstall:
	./install.sh uninstall
