;; yaml
(require 'yaml-mode)

;; Code folding

(add-hook 'yaml-mode-hook 'origami-mode)

(add-to-list 'origami-parser-alist '(yaml-mode . origami-indent-parser))

(provide 'eide-yaml)
