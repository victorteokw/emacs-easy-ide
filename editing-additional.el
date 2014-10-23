(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
;; Column Marker
(require-package 'column-marker)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 79)))

(provide 'editing-additional)
