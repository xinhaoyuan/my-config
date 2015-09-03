(defcustom my-configure-dir "~/.config/emacs" "")
(add-to-list 'load-path my-configure-dir)
(load "_package.el")
(load "_theme.el")
(load "_dired.el")
;; (load "_tabbar.el")
(load "_term.el")
(load "_python.el")
(load "_spell.el")
(load "_flyspell.el")
(load "_tty.el")
(load "_tmux.el")
(load "_markdown.el")
(load "_csharp.el")
;; misc {{{

(defvar server-frame nil "The frame of current emacs server")
(if (and (daemonp) (eq (length (frame-list)) 1))
    (setq server-frame (selected-frame)))

(setq eshell-save-history-on-exit nil)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq coding-system-for-write 'utf-8-unix)
(setq initial-scratch-message nil)
(setq default-major-mode 'text-mode)
(add-hook 'before-save-hook 'time-stamp)
(setq backup-inhibited t)
(setq auto-save-timeout 180)
(setq ring-bell-function 'ignore)
(setq require-final-newline t)
(setq visible-bell t)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq next-line-add-newlines nil)
(column-number-mode 1)
(line-number-mode 1)
(size-indication-mode 1)
(blink-cursor-mode -1)
(global-font-lock-mode 1)
(display-time-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)
(setq kill-ring-max 256)
(setq undo-limit 1000000)
(setq enable-recursive-minibuffers t)
(setq default-tab-width 8)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

(require 'subr-x)
(require 'redo)
(require 'psvn)
(require 'htmlize)
(if (require 'visual-fill-column nil 'noerror)
    (add-hook 'visual-line-mode-hook
              (lambda ()
                (if visual-line-mode
                    (visual-fill-column-mode 1)
                  (visual-fill-column-mode -1))
                )))
(require 'nlinum)
 
(setq nlinum-format-function
      (lambda (line width)
        (let ((str (format "%d " line)))
          (when (< (length str) width)
            ;; Left pad to try and right-align the line-numbers.
            (setq str (concat (make-string (- width (length str)) ?\ ) str)))
          (put-text-property 0 width 'face 'linum str)
          str)))

(defun turn-on-linum-mode () ""
  (nlinum-mode 1))
;; display line number for C/C++/JAVA and scheme
(add-hook 'c-mode-common-hook 'turn-on-linum-mode)
(add-hook 'scheme-mode-hook   'turn-on-linum-mode)

(require 'gas-mode)
(add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))
(require 'quack)

;; }}}

;; some procedures {{{
(defun mark-surrounding-blank ()
  "mark all continuous blank chars around current position"
  (interactive)
  (let ((left (point))
        (right (point)))
    (while (let ((char (char-after right)))
             (and char
                  (or (char-equal char ?\t)
                      (char-equal char ?\s))))
      (setq right (+ right 1)))

    (while (let ((char (char-before left)))
             (and char
                  (or (char-equal char ?\t)
                      (char-equal char ?\s))))
      (setq left (- left 1)))

    (set-mark right)
    (push-mark left)
    
    ))

(defun delete-surrounding-blank ()
  "delete all continuous blank chars around current position"
  (interactive)
  (let ((left (point))
        (right (point)))
    (while (let ((char (char-after right)))
             (and char
                  (or (char-equal char ?\t)
                      (char-equal char ?\s))))
      (setq right (+ right 1)))

    (while (let ((char (char-before left)))
             (and char
                  (or (char-equal char ?\t)
                      (char-equal char ?\s))))
      (setq left (- left 1)))

    (delete-region left right)
    ))

(defun wrapped-thing-at-point ()
  (save-excursion
    (let* ((pt (progn (while (and (char-before) (char-equal (char-before) ?\\)) (backward-char))  (point))) 
           (before-r (string-reverse
                      (let ((end pt))
                        (forward-line 0)
                        (buffer-substring-no-properties (point) end)
                       )))
           (after (let ((start pt))
                    (forward-line 1)
                    (buffer-substring-no-properties start (point))
                    )))
      (concat (save-match-data
                (string-match "\\([^][(){}<>\"'[:space:]]*\\([][(){}<>\"'[:space:]]\\\\\\(\\\\\\\\\\)*\\)*\\)+" before-r)
                (string-reverse (match-string 0 before-r)))
              (save-match-data
                (string-match "\\([^][(){}<>\"'[:space:]\n\\\\]*\\(\\\\.\\)*\\)+" after)
                (match-string 0 after)))
      )))

(defun save-wrapped-thing-at-point ()
  (interactive)
  (let ((thing (wrapped-thing-at-point)))
    (kill-new thing)
    (message "String saved: %s" thing))
  )

(defun open-wrapped-thing-at-point ()
  (interactive)
  (let* ((wrapped-thing (wrapped-thing-at-point))
         (raw-file-name 
          (save-match-data
            (string-match "\\([^#?\\\\]*\\(\\\\.\\)*\\)+" wrapped-thing)
            (match-string 0 wrapped-thing)))
         (file-name
          (replace-regexp-in-string
           "\\\\\\(.\\)" "\\1" raw-file-name))
         (raw-file-archor
          (if (> (length wrapped-thing)
                 (+ (length raw-file-name) 1))
              (substring wrapped-thing (length raw-file-name))
            nil))
         (wd (if buffer-file-name
                 (file-name-directory buffer-file-name)
               ""))
         (file-path (concat wd file-name))
         )
    (if (and (or (> (length file-name) 0)
                 (y-or-n-p
                  (format "Open \"%s\"?"
                          file-path)))
             (or (file-exists-p file-path)
                 (y-or-n-p
                  (format "File \"%s\" not exist, open any way?"
                          file-path))))
        (progn
          (find-file file-path)
          
          (if raw-file-archor
              (let ((file-archor
                     (cond
                      ((string-prefix-p "#" raw-file-archor)
                       (concat "[" (substring raw-file-archor 1) "]"))
                      ((string-prefix-p "?" raw-file-archor)
                       (substring raw-file-archor 1)))))
                (if (not (search-forward file-archor nil t))
                    (search-backward file-archor))))
          ))
    ))

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(defun reload-dotemacs ()
  "Reload ~/.emacs"
  (interactive)
  (load "~/.emacs"))

(defun find-Makefile ()
  "find-file \"Makefile\""
  (interactive)
  (find-file-other-window "Makefile"))

(defvar my-last-file-buffer nil
  "last selected file buffer")

(defun my-last-file-buffer-update ()
  (let ((b (current-buffer)))
              (if (buffer-file-name b)
                  (progn
                    ;; (message "last-file-buffer is %s" (buffer-name b))
                    (setq my-last-file-buffer b))
                )))

(add-hook 'buffer-list-update-hook
          'my-last-file-buffer-update)

(defun switch-to-last-file-buffer ()
  "switch to the first file buffer in buffer list"
  (interactive)
  (if (and my-last-file-buffer
           (buffer-live-p my-last-file-buffer))
      (switch-to-buffer my-last-file-buffer)
    ))

(require 'server)
(defun my-exit ()
  ""
  (interactive)
  (if server-process
      (and ;; no asking anymore
           ;; (y-or-n-p "Are you sure to kill this frame?")
           ;; feature <disabled>: if the current frame is the last
           ;; frame, just minimize it.
           (let* ((frame (selected-frame))
                  (nframe (next-frame frame))
                  )
             ;; (if (eq frame nframe)
             ;;     (iconify-frame frame)
             (save-and-kill-local-buffers)
             (delete-frame frame 't)
             ;; (select-frame-set-input-focus nframe)
             ;;  )
             ))
    (and (y-or-n-p "Are you sure to kill emacs?")
         (kill-emacs))
    )
  )

(defun save-and-kill-local-buffers ()
  "interactively save and kill local buffers (which is only opened at current frame)"
  (interactive)
  (let* ((local-frame (selected-frame))
         (buffers (buffer-list local-frame)))
    (cl-block BUFFER-LOOP
      (dolist (buffer buffers)
        (if (and (buffer-live-p buffer) (buffer-file-name buffer))
            (let ((is-local t)
                  (is-remote t)
                  (windows (get-buffer-window-list buffer nil t)))
              (dolist (window windows)
                (if (window-live-p window)
                    (let ((frame (window-frame window)))
                      (if (and (frame-live-p frame) (not (eq frame server-frame))) 
                          (if (eq local-frame frame)
                              (setq is-remote nil)
                            (setq is-local nil))))))
              (if is-local (kill-buffer buffer))
              (if is-remote (cl-return-from BUFFER-LOOP))
              ))
          ))
    )
  )

(defun tabify-buffer ()
  ""
  (interactive)
  (tabify (buffer-end -1) (buffer-end 1))
  )

(defun untabify-buffer ()
  ""
  (interactive)
  (untabify (buffer-end -1) (buffer-end 1))
  )

;; }}}

;; key mapping {{{
(if (require 'mwim nil 'noerror)
    (progn
      (global-set-key (kbd "<end>") 'mwim-end-of-code-or-line) ;;'move-end-of-line)
      (global-set-key (kbd "<home>") 'mwim-beginning-of-code-or-line) ;;'move-beginning-of-line)
      (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
      (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
      ))
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-M-/") 'redo)
(global-set-key (kbd "C-M-_") 'redo)
(global-set-key (kbd "<f10>") 'tmm-menubar)
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "<S-f9>") 'find-Makefile)
(global-set-key (kbd "C-l") 'recenter)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-<delete>") (lambda () (interactive) (mark-word) (delete-region (region-beginning) (region-end))))

(global-set-key (kbd "<f5>") 'eshell)
(global-set-key (kbd "<f6>") 'calendar)

(global-unset-key (kbd "C-z"))
(define-prefix-command 'my-prefix)
(global-set-key (kbd "C-z") 'my-prefix)
(define-key my-prefix "1" 'my-tmux-zoom)
(define-key my-prefix "2" 'my-tmux-split-v)
(define-key my-prefix "3" 'my-tmux-split-h)
(define-key my-prefix ":" 'my-tmux-command)
(define-key my-prefix "c" 'my-tmux-new-window)
(define-key my-prefix (kbd "<C-left>") 'my-tmux-prev-window)
(define-key my-prefix (kbd "<C-right>") 'my-tmux-next-window)
(define-key my-prefix (kbd "a") 'menu-bar-open)
(define-key my-prefix (kbd "<return>") 'newline)
(define-key my-prefix (kbd "C-<tab>") 'indent-region)
(define-key my-prefix (kbd "/") 'auto-insert)
(define-key my-prefix (kbd "t") 'tabify-buffer)
(define-key my-prefix (kbd "T") 'untabify-buffer)
(define-key my-prefix (kbd "p") 'org-publish-current-project)
(define-key my-prefix (kbd "C-z") 'delete-surrounding-blank)
(define-key my-prefix (kbd "DEL") 'delete-surrounding-blank)
(define-key my-prefix (kbd "!") (lambda () (interactive) (term (or (getenv "SHELL") "/bin/sh"))))
(define-key my-prefix (kbd "=") 'toggle-frame-fullscreen)
(define-key my-prefix (kbd "m") 'compile)
(define-key my-prefix (kbd "C-m") 'find-Makefile)
(define-key my-prefix (kbd "C-w") 'save-wrapped-thing-at-point)
(define-key my-prefix (kbd ".") 'open-wrapped-thing-at-point)
(define-key my-prefix (kbd "C-.") 'open-wrapped-thing-at-point)
(define-key my-prefix (kbd "q") 'visual-line-mode)
(define-key my-prefix (kbd "<up>")    'my-move-up)
(define-key my-prefix (kbd "<down>")  'my-move-down)
(define-key my-prefix (kbd "<left>")  'my-move-left)
(define-key my-prefix (kbd "<right>") 'my-move-right)

(define-key my-prefix (kbd "f") 'toggle-big-font)
(define-key my-prefix (kbd "<f11>") 'toggle-fullscreen)

(define-key my-prefix (kbd "v") 'set-variable)

(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-c") 'my-exit)
(global-set-key (kbd "M-<up>") 'scroll-down)
(global-set-key (kbd "M-<down>") 'scroll-up)
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-x c") 'kill-this-buffer)

(require 'sr-speedbar)
(global-set-key (kbd "C-z s") 'sr-speedbar-toggle)

(defun my-move-up ()
  (interactive)
  (windmove-up)
  )

(defun my-move-down ()
  (interactive)
  (windmove-down)
  )

(defun my-move-left ()
  (interactive)
  (windmove-left)
  )

(defun my-move-right ()
  (interactive)
  (windmove-right)
  )

;; Outline mode
(global-set-key (kbd "C-z \\") 'hide-other)
(global-set-key (kbd "C-z |") 'show-all)
(global-set-key (kbd "C-z +") 'show-branches)
(global-set-key (kbd "C-z ]") 'show-children)
(global-set-key (kbd "C-}") 'hide-subtree)
(global-set-key (kbd "C-{") 'show-subtree)
(global-set-key (kbd "C-z <C-return>") 'semantic-ia-complete-symbol)
;; }}}

;; enhence for CC mode {{{
(setq c-file-style "awk")
(add-hook 'c-mode-common-hook
          (lambda ()
            (outline-minor-mode 1)
            (subword-mode 1)
            (c-set-style "awk")))
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "awk")
            ))
(add-hook 'c-initialization-hook
          (lambda ()
            ;; indent format
            (setq c-default-style
                  '((c-mode   . "awk")
                    (c++-mode . "awk"))
                  )
            ;; auto indent
            (let ((map c-mode-base-map))
              (define-key map (kbd "<return>") 'newline-and-indent)
              (define-key map (kbd "RET") 'newline-and-indent)
              (define-key map (kbd "C-c C-r") 'uncomment-region))
            ;; abbrev
            (mapcar
             (lambda (arg)
               (apply 'define-abbrev c-mode-abbrev-table arg))
             '(("forl" "" abbrev-c-for)
               ("ifth" "" abbrev-c-ifth)
               ("ifel" "" abbrev-c-ifel)
               ("nil" "NULL")
               ))
            (mapcar
             (lambda (arg)
               (apply 'define-abbrev c++-mode-abbrev-table arg))
             '(("forl" "" abbrev-c-for)
               ("ifth" "" abbrev-c-ifth)
               ("ifel" "" abbrev-c-ifel)
               ("tplt" "template")
               ("nspc" "namespace")
               ("nil" "NULL")
               ))
            ))
;; }}}

;; enhence for GUD {{{
(defun gud-goto-line()
  (interactive)
  (gud-tbreak nil)
  (gud-go nil))

(defun gud-quit ()
  (interactive)
  (gud-call "q"))

(define-prefix-command 'my-gud-prefix)
(define-key my-gud-prefix (kbd "r") 'gdb-restore-windows)
(define-key my-gud-prefix (kbd "g") 'gdb-display-gdb-buffer)
(define-key my-gud-prefix (kbd "p") 'gud-print)
(define-key my-gud-prefix (kbd "b") 'gud-tbreak)

(setq gdb-many-windows 't)
(global-set-key (kbd "M-<f9>") 'gdb)
(add-hook 'gdb-mode-hook 
          (lambda ()
            (let ((map gud-minor-mode-map))
              ;; Reset windows
              (define-key map (kbd "C-<f2>") 'gud-quit)
              (define-key map (kbd "C-<f9>") 'gud-go)
              (define-key map (kbd "C-r") 'my-gud-prefix)
              ;; Breakpoints
              (define-key map (kbd "C-<f8>") 'gud-break)
              (define-key map (kbd "<f4>") 'gud-jump)
              (define-key map (kbd "C-<f4>") 'gud-goto-line)
              ;; Watch
              (define-key map (kbd "C-M-<f7>") 'gud-print)
              (define-key map (kbd "C-<f7>") 'gud-watch)
              ;; Nextline
              (define-key map (kbd "<f8>") 'gud-next)
              (define-key map (kbd "<f7>") 'gud-step)
              (define-key map (kbd "<f5>") 'gud-refresh) 
              )))
;; }}}

;; enhence for lisp/scheme {{{
;; Lisp & Scheme mode
(let ((map lisp-mode-shared-map))
  (define-key map (kbd "<return>") 'newline-and-indent) 
  (define-key map (kbd "RET") 'newline-and-indent)
  )
;; }}}

;; imaxima {{{
;; display configure of imaxima
(add-hook 'imaxima-startup-hook
          (lambda ()
            (setq imaxima-scale-factor 1.5)
            (setq imaxima-equation-color "black")))
;; }}}


;; no tabs by default. modes that really need tabs should enable
;; indent-tabs-mode explicitly. makefile-mode already does that, for
;; example.
(setq-default indent-tabs-mode nil)

;; if indent-tabs-mode is off, untabify before saving
;; (add-hook 'before-save-hook
;;           (lambda () (if (not indent-tabs-mode)
;;                          (untabify (point-min) (point-max)))))

(setq debug-on-error nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
