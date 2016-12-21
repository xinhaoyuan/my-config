;; Some useful elisp routines

(defun get-current-indentation-unsafe ()
  (interactive)
  (forward-line 0)
  (skip-chars-forward "\t ")
  (current-column))

(defun get-previous-indentation-unsafe ()
  (interactive)
  (forward-line 0)
  (skip-chars-backward "\r\n\t ")
  (if (eq (point) (point-min)) 0
    (get-current-indentation-unsafe)
    ))

(defun indent-line-simple ()
  (interactive)
  (let ((ind-point (save-excursion
                     (forward-line 0)
                     (skip-chars-forward "\t ")
                     (point))))
    (if (or (= (char-after ind-point) ?\n)
            (= (char-after ind-point) ?\r))
        (progn
          (indent-line-to (save-excursion (get-previous-indentation-unsafe)))
          (forward-line 0)
          (skip-chars-forward "\t "))
      (if (< (point) ind-point)
          (goto-char ind-point)
        (insert-tab))))
  )
