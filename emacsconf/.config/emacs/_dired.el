(setq dired-recursive-deletes t)
(setq dired-recursive-copies t)

(require 'dired-x)
(setq dired-omit-files "^\\(#\\|\\.[^\\.]+\\)")
(setq dired-guess-shell-alist-user
      '(;; video/audio files
        ("\\(\\.mp4\\|\\.mkv\\|\\.avi\\|\\.rmvb\\|\\.flac\\)$" "mplayer")
        ;; music files
        ("\\.mp3$" "mpg123 * &")
        ;; documents
        ("\\(\\.ps\\|\\.ps.gz\\|\\.pdf\\)$" "evince ? &")
        ))
(define-key dired-mode-map (kbd "M-m") 'wdired-change-to-wdired-mode)
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (dired-omit-mode 1)
            ))
