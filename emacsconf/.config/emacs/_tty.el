(require 'xt-mouse)

;; (defun turn-on-focus-event-on-terminal (terminal)
;;   (send-string-to-terminal "\e[?1004h" terminal))

;; (defun turn-off-focus-event-on-terminal (terminal)
;;   (send-string-to-terminal "\e[?1004l" terminal))

;; (defun send-apc-to-terminal (terminal message)
;;   (send-string-to-terminal (format "\e_%s\e\\" message) terminal))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string "[ \t\n]*\\'" "" string))
  )

(defun execute-tmux-command (command)
  (let ((r (trim-string
            (shell-command-to-string (format "tmux %s" command)))))
    ;; (message "%s" (format "tmux-command [%s] => [%s]" command r))
    r
    ))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((tty (frame-terminal frame)))
              (select-frame frame)
              (if tty
                  (progn
                    (if (or (string-match "^screen" (getenv "TERM"))
                            (string-match "^xterm" (getenv "TERM"))
                            (string-match "^rxvt" (getenv "TERM"))
                            )
                        (let ((m function-key-map))
                          (let ((function-key-map local-function-key-map))
                            (require 'xterm-extras)
                            (xterm-extra-keys)
                            (setq local-function-key-map function-key-map)
                            )
                          (setq function-key-map m)))

                    (set-frame-parameter
                     frame 'tmux-pane-id
                     (trim-string
                      (execute-tmux-command "display -p \"#{pane_id}\"")))
                    (execute-tmux-command 
                     (format "setp -t %s passthrough-keys on"
                             (frame-parameter frame 'tmux-pane-id)))
                    )))
            ))

(add-hook 'suspend-resume-hook
          (lambda ()
            (if (and (frame-terminal) (getenv "TMUX"))
                (execute-tmux-command 
                 (format "setp -t %s passthrough-keys on"
                         (frame-parameter nil 'tmux-pane-id)))
              )))

(add-hook 'suspend-hook
          (lambda ()
            (if (and (frame-terminal) (getenv "TMUX"))
                (execute-tmux-command 
                 (format "setp -t %s passthrough-keys off"
                         (frame-parameter nil 'tmux-pane-id)))
              )))

(add-hook 'delete-frame-functions
          (lambda (f)
            (if (and (frame-terminal) (getenv "TMUX"))
                (execute-tmux-command 
                 (format "setp -t %s passthrough-keys off"
                         (frame-parameter nil 'tmux-pane-id)))
              )))


(add-hook 'tty-setup-hook
          (lambda ()
            (if (getenv "TMUX" (selected-frame))
                (progn
                  (set-terminal-parameter
                   (frame-terminal) 'tmux-pane-id
                   (trim-string
                    (execute-tmux-command "display -p \"#{pane_id}\"")))
                  (execute-tmux-command 
                   (format "setp -t %s passthrough-keys on"
                           (frame-parameter frame 'tmux-pane-id)))
                  ))))
                 
(add-hook 'resume-tty-functions
          (lambda (tty)
            (execute-tmux-command 
             (format "setp -t %s passthrough-keys on"
                     (terminal-parameter tty 'tmux-pane-id)))
            ))

(add-hook 'suspend-tty-functions
          (lambda (tty)
            (execute-tmux-command 
             (format "setp -t %s passthrough-keys off"
                     (terminal-parameter tty 'tmux-pane-id)))
            ))

(add-hook 'delete-terminal-functions
          (lambda (tty)
            (execute-tmux-command 
             (format "setp -t %s passthrough-keys off"
                     (terminal-parameter tty 'tmux-pane-id)))
            ))

(add-hook 'kill-emacs-hook
          (lambda ()
            (mapc
             (lambda (f)
               (run-hook-with-args 'delete-frame-functions f))
             (frame-list))))

;; (defun terminal-gain-focus (tty)
;;   (if (not (terminal-parameter tty 'has-focus))
;;       (progn
;;         (set-terminal-parameter tty 'has-focus t)
;;         (if (getenv "TMUX" (selected-frame))
;;             (send-apc-to-terminal tty "enter-emacs"))
;;     )))

;; (defun terminal-lose-focus (tty)
;;     (if (terminal-parameter tty 'has-focus)
;;         (progn
;;           (set-terminal-parameter tty 'has-focus nil)
;;           (if (getenv "TMUX" (selected-frame))
;;               (send-apc-to-terminal tty "leave-emacs"))
;;           )))

(defun my-move-up ()
  (interactive)
  (condition-case nil
      (windmove-up)
    (error (if (and (not (display-graphic-p))
                    (getenv "TMUX" (selected-frame)))
               (execute-tmux-command "select-pane -U")
               )))
  )

(defun my-move-down ()
  (interactive)
  (condition-case nil
      (windmove-down)
    (error (if (and (not (display-graphic-p))
                    (getenv "TMUX" (selected-frame)))
               (execute-tmux-command "select-pane -D"))))
  )

(defun my-move-left ()
  (interactive)
  (condition-case nil
      (windmove-left)
    (error (if (and (not (display-graphic-p))
                    (getenv "TMUX" (selected-frame)))
               (execute-tmux-command "select-pane -L"))))
  )

(defun my-move-right ()
  (interactive)
  (condition-case nil
      (windmove-right)
    (error (if (and (not (display-graphic-p))
                    (getenv "TMUX" (selected-frame)))
               (execute-tmux-command "select-pane -R"))))
  )

(defun my-tmux-split-h ()
  (interactive)
  (execute-tmux-command "split-window -h"))

(defun my-tmux-split-v ()
  (interactive)
  (execute-tmux-command "split-window -v"))

(defun my-tmux-break-pane ()
  (interactive)
  (execute-tmux-command "break-pane"))

(defun my-tmux-command ()
  (interactive)
  (execute-tmux-command "command-prompt"))

(defun my-tmux-prev-window ()
  (interactive)
  (execute-tmux-command "select-window -p"))

(defun my-tmux-next-window ()
  (interactive)
  (execute-tmux-command "select-window -n"))

(defun my-tmux-new-window ()
  (interactive)
  (execute-tmux-command "new-window"))

(defun my-tmux-key-passthrough ()
  (interactive)
  (let ((key (elt (this-command-keys-vector) 1)))
    (if (and (not (display-graphic-p))
             (getenv "TMUX" (selected-frame)))
        (execute-tmux-command
         (format "send -U %s %c" "C-z" key)))
    ))

(defun my-tmux-zoom ()
  (interactive)
  (execute-tmux-command "resize-pane -Z"))
