(defun setup-objc ()
  "Setup Objective C development."
  ;; Yasnippet
  (require-package 'yasnippet)
  (yas-global-mode 1)

  ;; Auto-complete
  (require-package 'auto-complete)
  (require 'auto-complete-config)
  (setq-default ac-sources '(ac-source-yasnippet
			     ac-source-abbrev
			     ac-source-dictionary
			     ac-source-words-in-same-mode-buffers))
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)

  (add-to-list 'ac-modes 'objc-mode)
  )

(add-hook 'c-mode-common-hook 'setup-objc)


(provide 'init-xcode)
