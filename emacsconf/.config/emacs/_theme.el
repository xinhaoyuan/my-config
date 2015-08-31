;; use utf-8
(set-locale-environment "zh_CN.UTF-8")
(setq display-time-format "%Y%m%d-%w:%H%M")
(display-time)

;; set CJK font
(defcustom cjk-font-name-candidate-list
  (list "WenQuanYi Zen Hei"
	"STHeiti"
	"Vera Sans YuanTi Mono"
	"SimSun"
	"UniSun")
  "The list of possible fonts to display CJK characters")

;; toggle small/big fonts
(defcustom my-font-name "Consolas" "undocumented")
(defcustom my-font-toggle-flag 0 "undocumented")
(defcustom my-font-big-size 18 "undocumented")
(defcustom my-font-small-size 11 "undocumented")
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
			
			'(cursor-color	   . "#f0f0f0")
			'(foreground-color . "#f0f0f0")
			'(background-color . "#2d2d2d")
			
			(cons 'font my-font-small)
			))
		 (modify-frame-parameters f frame-alist)
		 )
	       (setq frame-background-mode 'dark)
	       )
	   ;; console
	   (progn
	     (setq frame-background-mode 'dark)
	     )
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
