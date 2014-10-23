(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Column Marker
(require-package 'column-marker)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 79)))

;; Expand region
(require-package 'expand-region)
(global-set-key (kbd "C-z") 'er/expand-region)
(global-set-key (kbd "C-M-z") 'er/contract-region)
(setq expand-region-contract-fast-key (kbd "Z"))

(provide 'editing-additional)
