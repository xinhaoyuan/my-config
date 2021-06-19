(require 'highlight-indent-guides nil 'noerror)

(setq python-check-command "pyflakes")
(setq python-indent-offset 4)
(add-hook 'python-mode-hook
          (lambda ()
            ;; (highlight-indent-guides-mode)
            (indent-guide-mode)
            (turn-on-linum-mode)
            (setq electric-indent-chars (delq ?: electric-indent-chars))))
