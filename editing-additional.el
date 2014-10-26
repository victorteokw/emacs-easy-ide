;;; editing-additional.el

;; electric indent
(electric-indent-mode 1)

;; electric pair
(electric-pair-mode 1)

;; (global-linum-mode 1)
;; whitespace mode for 80 column rule
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

;; Dash at point
(require-package 'dash-at-point)
(global-set-key (kbd "C-h D") 'dash-at-point)

(provide 'editing-additional)
