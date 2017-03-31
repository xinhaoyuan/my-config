(defvar simple-indent-width (lambda () tab-width))

(defun simple-indent-calc-delta (direction)
  "In any line, calculate the indentaiton offset according to direction"
  (* (or (and (commandp simple-indent-width)
              (call-interactively simple-indent-width))
         (and (functionp simple-indent-width)
              (funcall simple-indent-width))
         simple-indent-width) direction))

(defun simple-indent-region (start end direction)
  (interactive)
  (save-excursion
    (let ((delta 0)
          (loop-flag t))
      (goto-char start) (forward-line 0) (setq start (point))
      (setq delta (simple-indent-calc-delta direction))
      (goto-char end) (forward-line 0) (setq end (point))
      (while loop-flag
        (if (< end (line-end-position))
            (let* ((current-indentation
                    (progn (skip-chars-forward "\t ") (current-column)))
                   (new-indentation (+ current-indentation delta)))
              (if (<= 0 new-indentation)
                  (indent-line-to new-indentation)
                (indent-line-to 0))
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
        (simple-indent-region start end 1)
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
            (indent-line-to (+ ind (simple-indent-calc-delta 1)))
          (insert-tab)
          )))
    ))

(defun simple-indent-do-back-indent ()
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end))
            (deactivate-mark nil)
            )
        (simple-indent-region start end -1)
        )
    (save-excursion
      (let ((base (simple-indent-get-current-indentation-unsafe))
            (delta (simple-indent-calc-delta -1)))
        (if (<= 0 (+ base delta))
            (indent-line-to (+ base delta))
          (indent-line-to 0))
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
