(require 'xt-mouse)

;; (defun turn-on-focus-event-on-terminal (terminal)
;;   (send-string-to-terminal "\e[?1004h" terminal))

;; (defun turn-off-focus-event-on-terminal (terminal)
;;   (send-string-to-terminal "\e[?1004l" terminal))

;; (defun send-apc-to-terminal (terminal message)
;;   (send-string-to-terminal (format "\e_%s\e\\" message) terminal))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((tty (frame-terminal frame)))
              (select-frame frame)
              (if (and tty (eq window-system nil))
                  (progn
                    (if (and (stringp (getenv "TERM"))
                             (or (string-match "^dvtm" (getenv "TERM"))
                                 (string-match "^screen" (getenv "TERM"))
                                 (string-match "^xterm" (getenv "TERM"))
                                 (string-match "^rxvt" (getenv "TERM"))
                                 ))
                        (let ((m function-key-map))
                          (let ((function-key-map local-function-key-map))
                            (require 'xterm-extras)
                            (xterm-extra-keys)
                            (setq local-function-key-map function-key-map)
                            )
                          (setq function-key-map m)))
                    ))
              )
            ))

(defun terminal-init-dvtm ()
  "Terminal initialization function for dvtm."
  ;; Use the xterm color initialization code.
  (let ((file (locate-library (concat term-file-prefix "xterm"))))
    (and file
         (or (assoc file load-history)
             (load file t t))))
  ;; (tty-run-terminal-initialization (selected-frame) "xterm")
  ;; (terminal-init-xterm)
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces)

  ;; Don't know why this keymaps haven't been made in xterm init
  (define-key input-decode-map "\e[H" [home])
  (define-key input-decode-map "\e[F" [end])

  ;; More special keys listed here
  (define-key input-decode-map "\e[13;2u" [S-RET])
  (define-key input-decode-map "\e[13;4u" [M-S-RET])
  (define-key input-decode-map "\e[13;5u" [C-RET])
  (define-key input-decode-map "\e[13;6u" [C-S-RET])
  (define-key input-decode-map "\e[13;7u" [C-M-RET])
  )

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

