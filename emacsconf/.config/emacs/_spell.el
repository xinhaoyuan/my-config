(setq ispell-list-command "--list")
(setq ispell-program-name "/usr/local/bin/aspell")
(global-set-key (kbd "C-c ?") 'flyspell-correct-word-before-point)
