(setq ispell-list-command "--list")
(setq ispell-program-name
      (replace-regexp-in-string
       "\r?\n$" "" (shell-command-to-string "command -v aspell")))
(global-set-key (kbd "C-c ?") 'flyspell-correct-word-before-point)
