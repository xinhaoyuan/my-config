(if (and (require 'csharp-mode nil 'noerror)
         (require 'omnisharp nil 'noerror))
    (add-hook 'csharp-mode-hook 'omnisharp-mode)
  )
