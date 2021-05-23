;;; simple-indent-mode.el --- a minor mode to provide a consistent indentation.

;; Copyright 2021 Xinhao Yuan
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(defcustom si-indent-size 4
  "Number of spaces for a indentation level."
  :type 'integer
  :safe #'integerp)

(defun si-indent-line (levels)
  "Indent the current line by levels."
  (cond
   ((>= levels 0)
    (indent-line-to (* (+ (/ (current-indentation) si-indent-size) levels) si-indent-size)))
   (t
    (indent-line-to (* (+ (/ (+ (current-indentation) si-indent-size -1) si-indent-size) levels) si-indent-size)))
   ))

(defun si-indent-region (start end levels)
  "Indent the region of lines of [start, end] by levels."
  (interactive)
  (save-excursion
    (let ((delta 0)
          (loop-flag t))
      (goto-char start) (forward-line 0) (setq start (point))
      (setq delta (* si-indent-size levels))
      (goto-char end) (forward-line 0) (setq end (point))
      (while loop-flag
        (if (< end (line-end-position))
            (let* ((new-indentation (+ (current-indentation) delta)))
              (if (<= 0 new-indentation)
                  (indent-line-to new-indentation)
                (indent-line-to 0))
              ))
        (forward-line -1)
        (if (or (>= (point) end) (>= start end)) (setq loop-flag nil))
        (setq end (point))
        ))
    ))

(defun si-calculate-indentation ()
  "Calculate the indentation of the current line by looking back to the previous non-empty line."
  (save-excursion
    (forward-line -1)
    (loop
     if (equal (point) (point-min)) return 0
     do (back-to-indentation)
     unless (looking-at "$") return (current-indentation)
     do (forward-line -1)
     )))

(defun si-do-indent (levels repeat)
  "Indent regions/lines based on the context."
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end))
            (deactivate-mark nil)
            )
        (si-indent-region start end levels))
    (let ((new (si-calculate-indentation))
          (old (current-indentation))
          (col (current-column)))
      (cond
       ((and (not (eq old col)) (not (eq levels 0)))
        (back-to-indentation)
        (setq this-command nil))
       ((and (> levels 0) (>= old new))
        (si-indent-line levels))
       ((and (< levels 0) (<= old new))
        (si-indent-line levels))
       (t
        (indent-line-to new))
       ))
    )
  )

(defun si-indent-cmd () (interactive) (si-do-indent 1 (eq last-command 'si-indent-cmd)))
(defun si-back-indent-cmd () (interactive) (si-do-indent -1 (eq last-command 'si-back-indent-cmd)))
(defun si-newline-and-indent () (interactive) (newline) (si-do-indent 0 nil))

(defconst si-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "TAB") 'si-indent-cmd)
    (define-key keymap (kbd "<backtab>") 'si-back-indent-cmd)
    (define-key keymap (kbd "RET") 'si-newline-and-indent)
    (define-key keymap (kbd "<return>") 'si-newline-and-indent)
    keymap))

(define-minor-mode simple-indent-mode
  "Toggle simple-indent-mode, which overrides the behavior of <tab> and <backtab>."
  :global nil
  :init-value nil
  :lighter " SI"
  :keymap si-mode-map
  )

(provide 'simple-indent-mode)
