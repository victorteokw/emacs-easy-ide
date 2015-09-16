(require 'json-mode)

;;; Electric pair

(add-hook 'json-mode-hook 'electric-pair-mode)

;;; Syntax checking

(add-hook 'json-mode-hook 'flycheck-mode)

;; Code folding

(add-hook 'json-mode-hook 'hs-minor-mode)

(provide 'eide-json)
