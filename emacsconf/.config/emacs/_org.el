(with-eval-after-load "org"
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org/agenda-work.org"))
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))
  (plist-put org-format-latex-options :scale 2)
  )
