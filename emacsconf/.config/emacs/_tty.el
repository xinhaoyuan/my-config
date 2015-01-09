(require 'xt-mouse)

(defun turn-on-focus-event-on-terminal (terminal)
  (send-string-to-terminal "\e[?1004h" terminal)
  )

(defun turn-off-focus-event-on-terminal (terminal)
  (send-string-to-terminal "\e[?1004l" terminal)
  )

(defun send-apc-to-terminal (terminal message)
  (send-string-to-terminal (format "\e_%s\e\\" message) terminal))

(add-hook 'after-make-frame-functions
          (lambda (f)
            (message "after-make-frame")
            (with-selected-frame f
              (if (not (window-system))
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
                    
                    (define-key input-decode-map
                      "\e[I"
                      (lambda (_event)
                        (terminal-gain-focus (frame-terminal (selected-frame))) ""))
                    (define-key input-decode-map
                      "\e[O"
                      (lambda (_event)
                        (terminal-lose-focus (frame-terminal (selected-frame))) ""))

                    ;; let the terminal to resend the focus signal
                    (turn-off-focus-event-on-terminal (frame-terminal))
                    (turn-on-focus-event-on-terminal (frame-terminal))
                    )))))

(add-hook 'suspend-resume-hook
          (lambda () (if (not (display-graphic-p))
                         (progn
                           (turn-on-focus-event-on-terminal (frame-terminal))
                           (terminal-gain-focus (frame-terminal))))))

(add-hook 'resume-tty-functions
          (lambda (tty) (if (not (display-graphic-p))
                         (progn
                           (turn-on-focus-event-on-terminal tty)
                           (terminal-gain-focus tty)))))


(add-hook 'suspend-hook
          (lambda () (if (not (display-graphic-p))
                         (progn
                           (turn-off-focus-event-on-terminal (frame-terminal))
                           (terminal-lose-focus (frame-terminal))))))

(add-hook 'suspend-tty-functions
          (lambda (tty)
            (turn-off-focus-event-on-terminal tty)
            (terminal-lose-focus tty)))

(add-hook 'delete-frame-functions
          (lambda (f) (if (and (frame-live-p f) (not (display-graphic-p f)))
                          (with-selected-frame f
                            (turn-off-focus-event-on-terminal (frame-terminal))
                            (terminal-lose-focus (frame-terminal))
                            ))))

(add-hook 'delete-terminal-functions
          (lambda (tty)
            (turn-off-focus-event-on-terminal tty)
            (terminal-lose-focus tty)))
            
(add-hook 'kill-emacs-hook
          (lambda ()
            (mapc
             (lambda (f)
               (run-hook-with-args 'delete-frame-functions f))
             (frame-list))))

(defun terminal-gain-focus (tty)
  (if (not (terminal-parameter tty 'has-focus))
      (progn
        (set-terminal-parameter tty 'has-focus t)
        (if (getenv "TMUX" (selected-frame))
            (send-apc-to-terminal tty "enter-emacs"))
    )))

(defun terminal-lose-focus (tty)
    (if (terminal-parameter tty 'has-focus)
        (progn
          (set-terminal-parameter tty 'has-focus nil)
          (if (getenv "TMUX" (selected-frame))
              (send-apc-to-terminal tty "leave-emacs"))
          )))

(defun my-move-up ()
  (interactive)
  (condition-case nil
      (windmove-up)
    (error (if (and (not (display-graphic-p))
                    (getenv "TMUX" (selected-frame)))
               (send-apc-to-terminal (frame-terminal) "cmd,select-pane -U"))))
  )

(defun my-move-down ()
  (interactive)
  (condition-case nil
      (windmove-down)
    (error (if (and (not (display-graphic-p))
                    (getenv "TMUX" (selected-frame)))
               (send-apc-to-terminal (frame-terminal) "cmd,select-pane -D"))))
  )

(defun my-move-left ()
  (interactive)
  (condition-case nil
      (windmove-left)
    (error (if (and (not (display-graphic-p))
                    (getenv "TMUX" (selected-frame)))
               (send-apc-to-terminal (frame-terminal) "cmd,select-pane -L"))))
  )

(defun my-move-right ()
  (interactive)
  (condition-case nil
      (windmove-right)
    (error (if (and (not (display-graphic-p))
                    (getenv "TMUX" (selected-frame)))
               (send-apc-to-terminal (frame-terminal) "cmd,select-pane -R"))))
  )

(defun send-tmux-prefix ()
  (interactive)
  (condition-case nil
      (windmove-right)
    (error (if (and (not (display-graphic-p))
                    (getenv "TMUX" (selected-frame)))
               (send-apc-to-terminal (frame-terminal) "cmd,send-prefix -U"))))
  )

(add-hook 'my-prefix-undefined-hook
          (lambda (keys)
            (if (and (not (display-graphic-p))
                     (getenv "TMUX" (selected-frame)))
                (let ((line (format "tmux send -U %s %c" "C-F1" (elt keys 1))))
                  (shell-command line)
                  ))))
