(if (require 'package nil 'noerror)
    (progn
      ;; (add-to-list 'package-archives
      ;;              '("elpa" . "http://tromey.com/elpa/"))
      ;; (add-to-list 'package-archives
      ;;              '("gnu" . "http://elpa.gnu.org/packages"))
      (add-to-list 'package-archives
                   '("marmalade" . "http://marmalade-repo.org/packages/"))
      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.milkbox.net/packages/"))

      (package-initialize)
      
      (defun ensure-package-installed (&rest packages)
        "Assure every package s installed, ask for initiallation if it's not.

Return a list of installed packages or nil for every skipped package."
        (mapcar
         (lambda (package)
           (if (package-installed-p package)
               nil
             (if (y-or-n-p (format "Package %s is missing. Install it? " package))
                 (package-install package)
               package)))
         packages))
      
      (defun ensure-packages ()
        (interactive)
        (ignore-errors (package-refresh-contents))
        (ensure-package-installed
         'mwim 'popup 'nlinum                        ;; Basic enhancements
         'visual-fill-column                         ;; Text editing
         'company-mode                               ;; General completion
         'markdown-mode 'csharp-mode 'omnisharp      ;; Extra modes
         'irony-mode 'company-irony                  ;; C/C++ completion
         ))
      ))
