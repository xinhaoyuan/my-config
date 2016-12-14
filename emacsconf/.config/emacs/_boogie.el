(if (require 'dafny-mode nil 'noerror)
    (progn
      (setq dafny-verification-backend 'server)
      (add-hook 'boogie-friends-hook (lambda () (setq tab-width 4) (indent-tab-mode 1) (prettify-symbols-mode -1)))
      (setq flycheck-dafny-executable
	    (concat (getenv "HOME") "/opt/dafny-1.9.8/Dafny.exe"))
      (setq flycheck-inferior-dafny-executable
	    (concat (getenv "HOME") "/opt/dafny-1.9.8/DafnyServer.exe"))
      ))

(defun find-indent-ref ()
  (interactive)
  (catch 'stop-skip
    (while (> (point) (point-min))
      (let ((cb (char-before)))
        (if (or (eq cb ?\)) (eq cb ?\]) (eq cb ?}) (eq cb ?\"))
            (backward-sexp)
          (if (or (eq cb ?\n) (eq cb ?\r) (eq cb ?\() (eq cb ?\[) (eq cb ?{))
              (throw 'stop-skip nil)
            (backward-char)))))))

;; Experimental
(defun dafny-indent-alt ()
  (interactive)
  (beginning-of-line)
  (indent-line-to
   (catch 'indent
     ;; Case 0 - the line begins with a closing bracket. Align it with its starting sexp
     ;; 
     ;; Example:
     ;;   {
     ;;   ...
     ;;=> } 
     (if (let ((c (save-excursion (skip-chars-forward " \t") (char-after))))
           (or (eq c ?\)) (eq c ?\]) (eq c ?})))      
         (throw 'indent 
          (save-excursion
            (condition-case nil (backward-up-list) (error (throw 'indent 0)))
            (current-column))))

     (let* ((line-start-point (point))
            (indent-base 0)
            (indent-base-char nil)
            (indent-adjustment 0)
            )
       (save-excursion
         ;; go the last non-empty and non-line-ending char before this line
         (forward-line 0)
         (skip-chars-backward "\r\n\t ")
         
         (cond
          ;; Case 1 - reach the beginning, so no indentation needed
          ((eq line-start-point (point-min)) (throw 'indent 0))
          ;; Case 2.0 - 
          ((eq (char-before) ?\()
           (setq indent-base (- (current-column) 1))
           (setq indent-base-char ?\()
           (setq indent-adjustment 2))
          
          ((eq (char-before) ?\[)
           (setq indent-base (- (current-column) 1))
           (setq indent-base-char ?\[)
           (setq indent-adjustment 2))
          
          ((eq (char-before) ?{)
           (backward-char)
           (find-indent-ref)
           (skip-chars-forward "\t ")
           (setq indent-base (current-column))
           (setq indent-base-char ?{)
           (setq indent-adjustment 4))
          
          (t 
           (find-indent-ref)
           (skip-chars-forward "\t ")
           (setq indent-base (current-column))
           (setq indent-adjustment 0))
          )

         (message "base = %s, adjust = %s" indent-base indent-adjustment)

         (+ indent-base indent-adjustment))))
   ))

