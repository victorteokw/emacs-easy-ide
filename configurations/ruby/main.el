;; Basic ruby setup
(require-package 'ruby-mode)
(require-package 'ruby-hash-syntax)

(auto-major-mode 'ruby-mode
                 "\\.rxml\\'" "Kirkfile\\'"
                 "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'"
                 )

(setq ruby-use-encoding-map nil)

;; Ruby compilation
(require-package 'ruby-compilation)

;; yaml
(require-package 'yaml-mode)

(eval-after-load "ruby-mode"
  '(lambda ()
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)
     (add-hook 'ruby-mode-hook
               (lambda ()
                 (unless (derived-mode-p 'prog-mode)
                   (run-hooks 'prog-mode-hook))))))

(add-hook 'ruby-mode-hook (lambda ()
                            (ruby-end-mode t)
                            (require 'yasnippet)
                            (yas-minor-mode-on)))

;; Flycheck
(require-package 'flycheck)
;;(add-hook 'ruby-mode-hook 'flycheck-mode)
(provide 'init-ruby-mode)
