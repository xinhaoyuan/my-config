(defun simple-indent-region (start end offset)
  (interactive)
  (save-excursion
    (goto-char start) (forward-line 0) (setq start (point))
    (goto-char end) (forward-line 0) (setq end (point))
    (let ((loop-flag t))
      (while loop-flag
        (let* ((current-indentation
                (progn (skip-chars-forward "\t ") (current-column)))
               (new-indentation (+ current-indentation offset)))
          (if (<= 0 new-indentation)
              (indent-line-to new-indentation)
            (indent-line-to 0)
            ))
        (forward-line -1)
        (if (or (>= (point) end) (>= start end)) (setq loop-flag nil))
        (setq end (point))
        ))
    ))
        
(defun simple-indent-do-indent ()
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end))
            (deactivate-mark nil)
            )
        (simple-indent-region start end tab-width) 
        )
    (call-interactively 'indent-for-tab-command)
    )
  )

(defun simple-indent-do-back-indent ()
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end))
            (deactivate-mark nil)
            )
        (simple-indent-region start end (- tab-width))
        )
    (save-excursion
      (forward-line 0)
      (if (eq (char-after) ?\t)
          (delete-char 1)))
    )
  )

(defvar simple-indent-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "TAB") 'simple-indent-do-indent)
    (define-key keymap (kbd "<backtab>") 'simple-indent-do-back-indent)
    keymap))

(define-minor-mode simple-indent-mode
  "\
Toggle simple-indent-mode, which changes the behavior of <tab>
and <backtab> to some simple rule.
"
  :global nil
  :init-value nil
  :lighter t
  :keymap simple-indent-mode-map
  )
  
