(defvar simple-indent-width (lambda () tab-width))

(defun simple-indent-get-width (direction)
  (or (and (commandp simple-indent-width)
           (call-interactively simple-indent-width))
      (and (functionp simple-indent-width)
           (funcall simple-indent-width))
      simple-indent-width)
  )

(defun simple-indent-region (start end offset)
  (interactive)
  (save-excursion
    (goto-char start) (forward-line 0) (setq start (point))
    (goto-char end) (forward-line 0) (setq end (point))
    (let ((loop-flag t))
      (while loop-flag
        (if (< end (line-end-position))
            (let* ((current-indentation
                    (progn (skip-chars-forward "\t ") (current-column)))
                   (new-indentation (+ current-indentation offset)))
              (if (<= 0 new-indentation)
                  (indent-line-to new-indentation)
                (indent-line-to 0)
                ))
          )
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
            (width (simple-indent-get-width 1))
            )
        (simple-indent-region start end width)
        )
    (call-interactively 'indent-for-tab-command)
    )
  )

(defun simple-indent-get-current-indentation-unsafe ()
  (interactive)
  (forward-line 0)
  (skip-chars-forward "\t ")
  (current-column))

(defun simple-indent-get-previous-indentation-unsafe ()
  (interactive)
  (forward-line 0)
  (skip-chars-backward "\r\n\t ")
  (if (eq (point) (point-min)) 0
    (simple-indent-get-current-indentation-unsafe)
    ))

(defun simple-indent-line ()
  (interactive)
  (let ((ind -1)
        (ind-point -1)
        (prev-ind (save-excursion (simple-indent-get-previous-indentation-unsafe))))

    (save-excursion
      (forward-line 0)
      (skip-chars-forward "\t ")
      (setq ind (current-column))
      (setq ind-point (point)))

    (if (< (point) ind-point)
        (goto-char ind-point)
      (if (and (= (point) ind-point) (< ind prev-ind))
          (indent-line-to prev-ind)
        (if (= (point) ind-point)
            (indent-line-to (+ ind (simple-indent-get-width 1)))
          (insert-tab)
          )))
    ))

(defun simple-indent-do-back-indent ()
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end))
            (deactivate-mark nil)
            (width (simple-indent-get-width -1))
            )
        (simple-indent-region start end (- width))
        )
    (save-excursion
      (let ((new-indent (- (simple-indent-get-current-indentation-unsafe)
                           (simple-indent-get-width -1))))
        (if (< new-indent 0) (setq new-indent 0))
        (indent-line-to new-indent)
        ))
    ))

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
  :lighter " SI"
  :keymap simple-indent-mode-map
  )
  
