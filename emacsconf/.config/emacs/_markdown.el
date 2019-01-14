(with-eval-after-load "markdown-mode"
  (progn
    (let ((func (symbol-function 'markdown-follow-wiki-link)))
      (if (not (symbolp func))
          (setq original-markdown-follow-wiki-link-func func)))
    ;; Add-on to enable navigating to specific text in wiki links
    (defun my-markdown-follow-wiki-link (name &optional other)
      (save-match-data
        (string-match "\\([^#]+\\)\\(#\\(.+\\)\\)?" name)
        (let ((raw-filename (match-string 1 name))
              (archor (match-string 3 name)))
          (funcall original-markdown-follow-wiki-link-func raw-filename other)
          (if archor
              (if (not (search-forward archor nil t))
                  (search-backward archor)))
          )))
    (fset 'markdown-follow-wiki-link 'my-markdown-follow-wiki-link)
    )
  )
