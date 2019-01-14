(with-eval-after-load "fstar-mode"
  (progn
    ;; any configuration?
    (setq-default fstar-executable (concat (getenv "HOME") "/opt/fstar/bin/fstar.exe"))
    (setq-default fstar-smt-executable (concat (getenv "HOME") "/opt/fstar/bin/z3"))
    (add-hook 'fstar-mode-hook (lambda () (prettify-symbols-mode -1)))
    )
  )
