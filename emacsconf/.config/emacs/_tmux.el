;; TMUX integration

;; Disabled for now

;; (defun trim-string (string)
;;   "Remove white spaces in beginning and ending of STRING.
;; White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
;;   (replace-regexp-in-string
;;    "\\`[ \t\n]*" ""
;;    (replace-regexp-in-string "[ \t\n]*\\'" "" string))
;;   )

;; (defun tmux-command (command)
;;   (let ((r (trim-string
;;             (shell-command-to-string (format "tmux %s" command)))))
;;     ;; (message "%s" (format "tmux-command [%s] => [%s]" command r))
;;     (if (> (length r) 0) (message "TMUX: %s" r))
;;     r
;;     ))

;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (select-frame frame)
;;             (if (and (frame-terminal) (getenv "TMUX"))
;;                 (progn
;;                   (set-frame-parameter
;;                    nil 'tmux-pane-id
;;                    (trim-string
;;                     (tmux-command "display -p \"#{pane_id}\"")))
;;                   (tmux-command 
;;                    (format "setp -t %s passthrough-keys on"
;;                            (frame-parameter nil 'tmux-pane-id)))
;;                   ))
;;             ))

;; (add-hook 'suspend-resume-hook
;;           (lambda ()
;;             (if (and (frame-terminal) (getenv "TMUX"))
;;                 (tmux-command 
;;                  (format "setp -t %s passthrough-keys on"
;;                          (frame-parameter nil 'tmux-pane-id)))
;;               )))

;; (add-hook 'suspend-hook
;;           (lambda ()
;;             (if (and (frame-terminal) (getenv "TMUX"))
;;                 (tmux-command 
;;                  (format "setp -t %s passthrough-keys off"
;;                          (frame-parameter nil 'tmux-pane-id)))
;;               )))

;; (add-hook 'delete-frame-functions
;;           (lambda (f)
;;             (if (and (frame-live-p f) (frame-terminal f) (getenv "TMUX" f))
;;                 (tmux-command 
;;                  (format "setp -t %s passthrough-keys off"
;;                          (frame-parameter f 'tmux-pane-id)))
;;               )))

;; (add-hook 'kill-emacs-hook
;;           (lambda ()
;;             (mapc
;;              (lambda (f)
;;                (if (and (frame-live-p f) (frame-terminal f) (getenv "TMUX" f))
;;                    (tmux-command 
;;                     (format "setp -t %s passthrough-keys off"
;;                             (frame-parameter f 'tmux-pane-id)))
;;                  ))
;;              (frame-list))))

;; (add-hook 'tty-setup-hook
;;           (lambda ()
;;             (if (getenv "TMUX" (selected-frame))
;;                 (progn
;;                   (set-terminal-parameter
;;                    nil 'tmux-pane-id
;;                    (tmux-command "display -p \"#{pane_id}\""))
;;                   (tmux-command 
;;                    (format "setp -t %s passthrough-keys on"
;;                            (terminal-parameter nil 'tmux-pane-id)))
;;                   ))))
                 
;; (add-hook 'resume-tty-functions
;;           (lambda (tty)
;;             (tmux-command 
;;              (format "setp -t %s passthrough-keys on"
;;                      (terminal-parameter tty 'tmux-pane-id)))
;;             ))

;; (add-hook 'suspend-tty-functions
;;           (lambda (tty)
;;             (tmux-command 
;;              (format "setp -t %s passthrough-keys off"
;;                      (terminal-parameter tty 'tmux-pane-id)))
;;             ))

;; (add-hook 'delete-terminal-functions
;;           (lambda (tty)
;;             (tmux-command 
;;              (format "setp -t %s passthrough-keys off"
;;                      (terminal-parameter tty 'tmux-pane-id)))
;;             ))

;; (defun my-move-up ()
;;   (interactive)
;;   (condition-case nil
;;       (windmove-up)
;;     (error (if (and (not (display-graphic-p))
;;                     (getenv "TMUX" (selected-frame)))
;;                (tmux-command "select-pane -U")
;;                )))
;;   )

;; (defun my-move-down ()
;;   (interactive)
;;   (condition-case nil
;;       (windmove-down)
;;     (error (if (and (not (display-graphic-p))
;;                     (getenv "TMUX" (selected-frame)))
;;                (tmux-command "select-pane -D"))))
;;   )

;; (defun my-move-left ()
;;   (interactive)
;;   (condition-case nil
;;       (windmove-left)
;;     (error (if (and (not (display-graphic-p))
;;                     (getenv "TMUX" (selected-frame)))
;;                (tmux-command "select-pane -L"))))
;;   )

;; (defun my-move-right ()
;;   (interactive)
;;   (condition-case nil
;;       (windmove-right)
;;     (error (if (and (not (display-graphic-p))
;;                     (getenv "TMUX" (selected-frame)))
;;                (tmux-command "select-pane -R"))))
;;   )

;; (defun my-tmux-split-h ()
;;   (interactive)
;;   (tmux-command "split-window -h"))

;; (defun my-tmux-split-v ()
;;   (interactive)
;;   (tmux-command "split-window -v"))

;; (defun my-tmux-break-pane ()
;;   (interactive)
;;   (tmux-command "break-pane"))

;; (defun my-tmux-command ()
;;   (interactive)
;;   (tmux-command "command-prompt"))

;; (defun my-tmux-prev-window ()
;;   (interactive)
;;   (tmux-command "select-window -p"))

;; (defun my-tmux-next-window ()
;;   (interactive)
;;   (tmux-command "select-window -n"))

;; (defun my-tmux-new-window ()
;;   (interactive)
;;   (tmux-command "new-window"))

;; (defun my-tmux-key-passthrough ()
;;   (interactive)
;;   (let ((key (elt (this-command-keys-vector) 1)))
;;     (if (and (not (display-graphic-p))
;;              (getenv "TMUX" (selected-frame)))
;;         (tmux-command
;;          (format "send -U %s %c" "C-z" key)))
;;     ))

;; (defun my-tmux-zoom ()
;;   (interactive)
;;   (tmux-command "resize-pane -Z"))
