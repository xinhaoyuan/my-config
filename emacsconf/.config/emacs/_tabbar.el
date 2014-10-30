(if (require 'tabbar-ruler nil 'noerror)
    (progn
      (setq tabbar-ruler-global-ruler t)
      (global-set-key (kbd "C-<") 'tabbar-backward)
      (global-set-key (kbd "C->") 'tabbar-forward)
      (global-set-key (kbd "C-=") 'tabbar-ruler-move)
      (add-hook 'after-init-hook
		(lambda ()
		  (global-set-key (kbd "C-z =") 'tabbar-ruler-move)
		  ))
      (tabbar-mode 1)
      )
  )

(add-hook 'window-setup-hook
          (lambda ()
            (tabbar-install-faces)
            ))
