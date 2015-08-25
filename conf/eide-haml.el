(defun hy-lang-haml-setup-auto-completion ()
  "You know"
  (require 'ac-haml)
  (setq ac-sources '(ac-source-haml-tag
                     ac-source-haml-attribute
                     ac-source-haml-attribute-value))
  (auto-complete-mode))

;; Auto complete
;;(add-hook 'haml-mode-hook 'hy-lang-haml-setup-auto-completion)

;; Check haml syntax
(add-hook 'haml-mode-hook 'flycheck-mode)

(provide 'eide-haml)
