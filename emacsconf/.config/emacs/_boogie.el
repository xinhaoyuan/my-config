(if (require 'dafny-mode nil 'noerror)
    (progn
      (setq dafny-verification-backend 'server)
      (add-hook 'dafny-mode-hook
                (lambda ()
                  (setq-local tab-width 4)
                  (setq-local indent-tabs-mode t)
                  (setq-local indent-line-function 'indent-line-simple)
                  (electric-indent-local-mode -1)
                  (fset 'boogie-friends-self-insert-and-indent 'self-insert-command)
                  ))
      (setq flycheck-dafny-executable
	    (concat (getenv "HOME") "/opt/dafny/Dafny.exe"))
      (setq flycheck-inferior-dafny-executable
	    (concat (getenv "HOME") "/opt/dafny/DafnyServer.exe"))
      ))


