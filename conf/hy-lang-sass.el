;; Disable free variable warning
(eval-when-compile
  (require 'scss-mode))

;; Do not compile at save
(setq scss-compile-at-save nil)

;; Flycheck syntax
(add-hook 'scss-mode-hook 'flycheck-mode)
(add-hook 'sass-mode-hook 'flycheck-mode)

(provide 'hy-lang-sass)
