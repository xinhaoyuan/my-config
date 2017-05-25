(if (require 'dafny-mode nil 'noerror)
    (progn
      (setq dafny-verification-backend 'server)
      (setq old-parse-errors (symbol-function 'inferior-dafny-parse-errors))
      ;; a workaround for removeing [module-name] and dealing with external file locations 
      (defun inferior-dafny-parse-errors (errors)
        (mapc (lambda (e)
                (let ((fname (flycheck-error-filename e)))
                  (if (eq fname nil)
                      (setq fname buffer-file-name)
                    (if (string-match
                         (rx (group (* (not (in "[]")))) (* anything))
                         fname)
                        (setq fname (match-string 1 fname)))
                    (if (not (string-equal fname buffer-file-name))
                        (let ((old-fname (flycheck-error-filename e))
                              (old-msg (flycheck-error-message e))
                              (old-col (flycheck-error-column e))
                              (old-line (flycheck-error-line e)))
                          (setf (flycheck-error-message e)
                                (concat (format "%s(%d:%d)-"
                                                (file-name-nondirectory fname)
                                                old-line
                                                old-col
                                                )
                                        old-msg
                                        "(" old-fname ")"))
                          (setf (flycheck-error-column e) nil)
                          ;; since line is required ...
                          (setf (flycheck-error-line e) 1)
                          (setq fname buffer-file-name))))
                  (setf (flycheck-error-filename e) fname)
                  ))
              (funcall old-parse-errors errors)))
      (let ((p (assoc "\\(\\_<forall\\_>\\).*?::"
                      dafny-font-lock-keywords)))
        (setcdr p (cdr (cdr p)))
        )
      (add-hook 'dafny-mode-hook
                (lambda ()
                  (setq-local tab-width 4)
                  (setq-local indent-tabs-mode t)
                  (setq-local indent-line-function 'simple-indent-line)
                  (electric-indent-local-mode -1)
                  (fset 'boogie-friends-self-insert-and-indent 'self-insert-command)
                  (modify-syntax-entry ?_ "_")
                  (prettify-symbols-mode -1)
                  (flycheck-mode -1)
                  (simple-indent-mode 1)
                  ))
      (setq flycheck-dafny-executable
	    (concat (getenv "HOME") "/opt/dafny/Dafny.exe"))
      (setq flycheck-inferior-dafny-executable
	    (concat (getenv "HOME") "/opt/dafny/DafnyServer.exe"))
      ))


