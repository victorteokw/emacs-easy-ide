;;; Syntax checking

(add-hook 'sass-mode-hook 'flycheck-mode)

;;; Rainbow color

(add-hook 'sass-mode-hook 'rainbow-mode)

;;; Code folding

(add-hook 'sass-mode-hook 'origami-mode)

(add-to-list 'origami-parser-alist '(sass-mode . origami-indent-parser))

(provide 'eide-sass)
