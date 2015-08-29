(require 'json-mode)

;; Check JSON syntax
(add-hook 'json-mode-hook 'flycheck-mode)

;; Electric pair
(add-hook 'json-mode-hook 'electric-pair-mode)

;; Code folding
(add-hook 'json-mode-hook 'hs-minor-mode)

(provide 'eide-json)
