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
  (let ((ind -1)
        (ind-point -1)
        (prev-ind (save-excursion (get-previous-indentation-unsafe))))

    (save-excursion
      (forward-line 0)
      (skip-chars-forward "\t ")
      (setq ind (current-column))
      (setq ind-point (point)))

    (if (< (point) ind-point)
        (goto-char ind-point)
      (if (and (= (point) ind-point) (< ind prev-ind))
          (indent-line-to prev-ind)
        (insert-tab)))
    ))
