;; use utf-8
(set-locale-environment "zh_CN.UTF-8")
(setq display-time-format "%Y%m%d-%w:%H%M")
(display-time)

;; set CJK font
(defcustom cjk-font-name-candidate-list
  (list "WenQuanYi Zen Hei"
	"STHeiti"
	"Vera Sans YuanTi Mono"
        "SimHei"
	"SimSun"
	"UniSun")
  "The list of possible fonts to display CJK characters")

(font-family-list)
;; toggle small/big fonts
(defcustom my-font-name "Hack" "undocumented")
(defcustom my-font-toggle-flag 0 "undocumented")
(defcustom my-font-big-size 16 "undocumented")
(defcustom my-font-small-size 10 "undocumented")
(defcustom my-font-big (concat my-font-name "-" (number-to-string my-font-big-size)) "undocumented")
(defcustom my-font-small (concat my-font-name "-" (number-to-string my-font-small-size)) "undocumented")
(defun toggle-big-font ()
  (interactive)
  (if (eq my-font-toggle-flag 0)
      (progn
	(setq my-font-toggle-flag 1)
	(set-frame-font my-font-big 't))
    (progn
      (setq my-font-toggle-flag 0)
      (set-frame-font my-font-small 't))))

(let ((init-new-frame
       (lambda (f)
	 ;; set font/color
	 (select-frame f)
	 (if (display-graphic-p f)
	     ;; window
	     (progn
	       (setq x-select-enable-clipboard t)
	       (setq frame-title-format "%b %n %p")
	       (set-scroll-bar-mode nil)
	       (tool-bar-mode -1)
	       ;; (menu-bar-mode -1)
	       (let ((try-font
		      (lambda (cur)
			(if (eq cur nil)
			    (error "Cannot find suitable CJK font")
			  (let ((font-name (car cur)))
			    (if (find-font (font-spec :name font-name))
				(set-fontset-font "fontset-default" 'gb18030
						  (cons font-name "unicode-bmp"))
			      (funcall try-font (cdr cur))))
			  ))))
		 (funcall try-font cjk-font-name-candidate-list)
		 )
	       (let ((frame-alist default-frame-alist))
		 (mapc (lambda (ele) 
			 (setq frame-alist
			       (cons ele
				     frame-alist
				     )))
		       (list
			'(left-fringe . 0)
			'(right-fringe . 0)
			'(alpha . 100)
			'(width . 100)
			'(height . 35)
			
			;; '(cursor-color	   . "#f0f0f0")
			;; '(foreground-color . "#f0f0f0")
			;; '(background-color . "#2d2d2d")
			
			(cons 'font my-font-small)
			))
		 (modify-frame-parameters f frame-alist)
		 )
	       )
	   ;; TTY
           ;; For rxvt-unicode-256color, the current theme detection does not work because COLORFGBG returns "default".
           ;; We have to use the xterm way but modified to make it fast
           (let ((term (getenv-internal "TERM" initial-environment)))
             (if (string-prefix-p "rxvt" term)
                 (progn
                   ;; for xterm-query
                   (load "term/xterm.el")
                   (flet ((my-xterm-maybe-set-dark-background-mode
                           (redc greenc bluec)
                           (message "!!! %d %d %d" redc greenc bluec)
                           ;; Use the heuristic in `frame-set-background-mode' to decide if a
                           ;; frame is dark.
                           (if (< (+ redc greenc bluec) (* .6 (+ 65535 65535 65535)))
                               (setq frame-background-mode 'dark)
                             (setq frame-background-mode 'light)
                             )
                           (frame-set-background-mode f)
                           )
                          (my-xterm--report-background-handler
                           ()
                           (let ((str "")
                                 chr)
                             ;; The reply should be: \e ] 11 ; rgb: NUMBER1 / NUMBER2 / NUMBER3 \e \\
                             (while (and (setq chr (read-event nil nil 0.1)) (not (equal chr ?\\)))
                               (setq str (concat str (string chr))))
                             (when (string-match
                                    "rgb:\\([a-f0-9]+\\)/\\([a-f0-9]+\\)/\\([a-f0-9]+\\)" str)
                               (let ((recompute-faces
                                      (my-xterm-maybe-set-dark-background-mode
                                       (string-to-number (match-string 1 str) 16)
                                       (string-to-number (match-string 2 str) 16)
                                       (string-to-number (match-string 3 str) 16))))

                                 ;; Recompute faces here in case the background mode was
                                 ;; set to dark.  We used to call
                                 ;; `tty-set-up-initial-frame-faces' only once, but that
                                 ;; caused the light background faces to be computed
                                 ;; incorrectly.  See:
                                 ;; http://permalink.gmane.org/gmane.emacs.devel/119627
                                 (when recompute-faces
                                   (tty-set-up-initial-frame-faces)))))))
                     (xterm--query "\e]11;?\e\\" '(("\e]11;" .  my-xterm--report-background-handler)))
	             ))))
	     )
	 )))
  (add-hook 'after-make-frame-functions init-new-frame)
  )

(add-hook 'after-init-hook
	  (lambda ()
	    (mapc
	     (lambda (f)
	       (run-hook-with-args 'after-make-frame-functions f))
	     (frame-list))))
