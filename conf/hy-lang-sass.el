;; Disable free variable warning
(eval-when-compile
  (require 'scss-mode))

;; Do not compile at save
(setq scss-compile-at-save nil)

;;; Flycheck syntax

;;(add-hook 'scss-mode-hook 'flycheck-mode)
;;(add-hook 'sass-mode-hook 'flycheck-mode)

;;; Rainbow

(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'sass-mode-hook 'rainbow-mode)

;; Electric pair

(add-hook 'scss-mode-hook 'electric-pair-mode)

;;
(defun hy-lang-sass-insert-colon-pair ()
  ""
  (interactive)
  (insert ": ;")
  (backward-char 1))
(define-key scss-mode-map (kbd ":") 'hy-lang-sass-insert-colon-pair)

(defun hy-lang-sass-insert-block ()
  ""
  (interactive)
  (insert " {}")
  (backward-char 1)
  (newline-and-indent))
(define-key scss-mode-map (kbd "{") 'hy-lang-sass-insert-block)

(provide 'hy-lang-sass)
