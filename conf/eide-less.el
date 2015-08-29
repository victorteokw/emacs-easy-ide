;; Flycheck less syntax
(add-hook 'less-css-mode 'flycheck-mode)

;; Code folding

(add-hook 'less-css-mode-hook 'origami-mode)

(add-to-list 'origami-parser-alist '(less-css-mode . origami-c-style-parser))

(provide 'eide-less)
