(require-package 'js2-mode)
(require-package 'helm)
(add-hook 'js2-mode-hook '(lambda ()
                            (js2-imenu-extras-mode)
                            ))
(auto-major-mode 'js2-mode "\\.js\\'")
(require-package 'json-mode)

(require-package 'ac-js2)

;; Javascript nests {} and () a lot, so I find this helpful
(require-package 'rainbow-delimiters)
(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))
