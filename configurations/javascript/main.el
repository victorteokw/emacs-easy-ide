(require-package 'js2-mode)

(defun auto-complete-javascript ()
  (require-package 'auto-complete)
  (require-package 'ac-js2)
  (add-to-list 'ac-modes 'js2-mode)
  (ac-config-default)
  (setq ac-sources '(ac-source-yasnippet))
  (auto-complete-mode t)
  (setq-default ac-auto-start t)
  (setq-default ac-dwim t)
  (ac-js2-mode)
  )

(add-hook 'js2-mode-hook '(lambda ()
                            (js2-imenu-extras-mode)

                            (auto-complete-javascript)
                            ))
(auto-major-mode 'js2-mode "\\.js\\'")
(require-package 'json-mode)



;; Javascript nests {} and () a lot, so I find this helpful
(require-package 'rainbow-delimiters)
(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))
