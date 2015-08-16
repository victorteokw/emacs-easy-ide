;; Check JSON syntax
(add-hook 'json-mode-hook 'flycheck-mode)

;; Electric pair
(add-hook 'json-mode-hook 'electric-pair-mode)

(provide 'eide-json)
