.PHONY: prepare install uninstall cron sync

prepare:
	git submodule update --init --recursive
	./install.sh prepare

install: prepare
	./install.sh install

uninstall:
	./install.sh uninstall

cron:
	@crontab -l | linux/bin/crontab-updater.py linux/.config/cron/tasks | tee /dev/stderr | crontab -

sync:
	./sync.sh
