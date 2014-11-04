(defun setup-ruby-auto-complete ()
  "Set up ruby development auto complete."
  (require-package 'auto-complete)
  (add-to-list 'ac-modes 'ruby-mode)
  (ac-config-default)
  (setq ac-sources '(ac-source-yasnippet))
  (add-to-list 'ac-sources 'ac-source-dictionary)
  (auto-complete-mode t)
  (setq-default ac-auto-start t)
  (setq-default ac-dwim t)
  )

(add-hook 'ruby-mode-hook 'setup-ruby-auto-complete)
