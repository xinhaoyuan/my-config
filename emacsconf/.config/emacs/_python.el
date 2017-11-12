(require 'highlight-indent-guides)

(setq python-check-command "pyflakes")
(setq python-indent-offset 4)
(add-hook 'python-mode-hook
          (lambda ()
            (highlight-indent-guides-mode)
            (setq electric-indent-chars (delq ?: electric-indent-chars))))
