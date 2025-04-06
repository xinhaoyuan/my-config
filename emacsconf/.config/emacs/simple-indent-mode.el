;;; -*- lexical-binding: t -*-
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
  (let* ((old (current-indentation))
         (new (cond
               ((>= levels 0) (* (+ (/ old si-indent-size) levels) si-indent-size))
               (t (* (max 0 (+ (/ (+ old si-indent-size -1) si-indent-size) levels)) si-indent-size))))
         (col (current-column))
         (new-col (+ col (- new old))))
    (indent-line-to new)
    (move-to-column (max new-col new))
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
            (indent-line-to (max 0 (+ (current-indentation) delta))))
        (forward-line -1)
        (if (or (>= (point) end) (>= start end)) (setq loop-flag nil))
        (setq end (point))
        ))
    ))

(defcustom simple-indent-mode-max-lookup-lines 80 "Number of lines to look up for indenting the current line.")

(defun si-previous-indentation (shrink)
  "Finds the indentation of the cloest previous non-empty line. If `shrink` is t, ignores lines with larger indentation."
  (let ((cur-ind (current-indentation))
        (num-lookup-lines 0)
        )
    (save-excursion
      (cl-loop
       if (< (forward-line -1) 0) return 0
       if (> (setq num-lookup-lines (+ num-lookup-lines 1)) simple-indent-mode-max-lookup-lines) return 0
       do (back-to-indentation)
       for new-ind = (current-indentation)
       unless (or (looking-at "$") (and shrink (>= new-ind cur-ind))) return new-ind
       ))))

(defun si-do-indent (levels repeat)
  "Indent regions/lines based on the context."
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end))
            (deactivate-mark nil)
            )
        (si-indent-region start end levels))
    (let ((new (si-previous-indentation (< levels 0)))
          (old (current-indentation))
          (col (current-column)))
      (cond
       ((and (> levels 0) (>= old new))
        (si-indent-line levels))
       ((and (< levels 0) (<= old new))
        (si-indent-line levels))
       (t
        (indent-line-to new)
        (move-to-column (max new (+ col (- new old))))
        )
       )
      )
    )
  )

(defun si-indent-cmd () (interactive) (si-do-indent 1 (eq last-command 'si-indent-cmd)))
(defun si-back-indent-cmd () (interactive) (si-do-indent -1 (eq last-command 'si-back-indent-cmd)))
(defun si-back-indent-key-cmd (key)
  (lambda () (interactive)
    (let ((cur-ind (current-indentation))
          (cur-col (current-column)))
      (if (and (not (use-region-p)) (> cur-col 0) (= cur-col cur-ind))
          (let* ((prev-ind (si-previous-indentation t))
                 (back-ind (save-excursion
                             (si-indent-line -1)
                             (current-column)))
                 (target-ind (if (< prev-ind cur-ind) (max prev-ind back-ind) back-ind))
                 )
            (indent-line-to target-ind))
        (let* ((simple-indent-mode nil)
               (orig-func (key-binding key)))
          (call-interactively orig-func)
          )))))
(defun si-newline-and-indent () (interactive) (newline) (si-do-indent 0 nil))

(defconst si-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "TAB") 'si-indent-cmd)
    (define-key keymap (kbd "<backtab>") 'si-back-indent-cmd)
    (define-key keymap (kbd "DEL") (si-back-indent-key-cmd (kbd "DEL")))
    ;;;; Does not work when <backspace> is translated to DEL
    ;; (define-key keymap (kbd "<backspace>") (si-back-indent-key-cmd (kbd "")))
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

(defun si-electric-indent-function (_char) (if simple-indent-mode 'no-indent nil))
(add-hook 'electric-indent-functions #'si-electric-indent-function)

(provide 'simple-indent-mode)
