;;; 2 space for indentation

(setq css-indent-offset 2)

;;; rainbow

(add-hook 'css-mode-hook 'rainbow-mode)

;;; Code folding

(add-hook 'css-mode-hook 'origami-mode)

(add-to-list 'origami-parser-alist '(css-mode . origami-c-style-parser))

(provide 'eide-css)
