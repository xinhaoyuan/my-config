
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defcustom my-configure-dir (concat (getenv "HOME") "/.config/emacs") "")
(defcustom my-org-dir (concat (getenv "HOME") "/org") "")

(add-to-list 'load-path my-configure-dir)
(load "_init.el")

(setq debug-on-error nil)
