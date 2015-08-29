(defun hy-lang-haml-setup-auto-completion ()
  "You know"
  (require 'ac-haml)
  (require 'ac-html-default-data-provider)
  (ac-html-enable-data-provider 'ac-html-default-data-provider)
  (ac-haml-setup)
  (setq ac-sources '(ac-source-haml-tag
                     ac-source-haml-attr
                     ac-source-haml-attrv))
  (auto-complete-mode))

;; Auto complete
(add-hook 'haml-mode-hook 'hy-lang-haml-setup-auto-completion)

;; Check haml syntax
;;(add-hook 'haml-mode-hook 'flycheck-mode)

(provide 'eide-haml)
