(require 'auto-complete)

;; Completion for slim mode
(defun hy-lang-slim-setup-auto-completion ()
  "You know what it means."
  (require 'ac-slim)
  (setq ac-sources '())
  (add-to-list 'ac-sources 'ac-source-slim-tag)
  (add-to-list 'ac-sources 'ac-source-slim-attribute)
  (add-to-list 'ac-sources 'ac-source-slim-attribute-value)
  (auto-complete-mode))

(add-hook 'slim-mode-hook 'hy-lang-slim-setup-auto-completion)

(add-hook 'slim-mode-hook 'flycheck-mode)

(provide 'hy-lang-slim)
