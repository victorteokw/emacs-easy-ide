;;; editing-additional.el

;; electric indent
(electric-indent-mode 1)
(require-package 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

;;(global-aggressive-indent-mode)

;; electric pair
(electric-pair-mode 1)

;; show paren mode
(show-paren-mode 1)
;; (global-linum-mode 1)
;; whitespace mode for 80 column rule
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(require-package 'diminish)
(diminish 'whitespace-mode)
;; Column Marker
(require-package 'column-marker)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 79)))

;; Expand region
(require-package 'expand-region)
(global-set-key (kbd "C-z") 'er/expand-region)
(global-set-key (kbd "C-M-z") 'er/contract-region)
(setq expand-region-contract-fast-key (kbd "Z"))

;; Highlight symbol
(require-package 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(eval-after-load 'highlight-symbol
  '(diminish 'highlight-symbol-mode))

;; kill back to indentation
(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

;; Dash at point
(require-package 'dash-at-point)
(global-set-key (kbd "C-h D") 'dash-at-point)

(provide 'editing-additional)
