(if (and (require 'csharp-mode nil 'noerror)
         (require 'omnisharp nil 'noerror))
    (progn
      (add-hook 'csharp-mode-hook
                (lambda ()
                  (omnisharp-mode)
                  (company-mode)
                  (flycheck-mode)
                  ))
      (define-key omnisharp-mode-map (kbd "C-c ?") 'omnisharp-go-to-definition)
      (define-key omnisharp-mode-map (kbd "C-c /") 'omnisharp-find-usages)
      (define-key omnisharp-mode-map (kbd "C-c C-s") 'omnisharp-helm-find-symbols)
      (if (require 'backward-forward nil 'noerror)
          (progn
            (define-key omnisharp-mode-map (kbd "C-c <left>") 'backward-forward-previous-location)
            (define-key omnisharp-mode-map (kbd "C-c <right>") 'backward-forward-next-location)
            ))
      (eval-after-load
          'company
        '(add-to-list 'company-backends #'company-omnisharp))
      )
  )
