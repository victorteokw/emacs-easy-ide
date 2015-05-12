(require-package 'web-mode)
(require-package 'php-mode)

;; (defun setup-php ()
;;   (web-mode)
;;   (make-local-variable 'web-mode-code-indent-offset)
;;   (make-local-variable 'web-mode-markup-indent-offset)
;;   (make-local-variable 'web-mode-css-indent-offset)

;;   (setq web-mode-code-indent-offset 4)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-markup-indent-offset 2))

(add-hook 'php-mode-hook
          (lambda ()
            (auto-complete-mode)))

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))


(provide 'init-php)
