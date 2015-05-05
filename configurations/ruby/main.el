;; Basic ruby setup
(require-package 'ruby-mode)

(require-package 'ruby-hash-syntax)

(auto-major-mode 'ruby-mode
                 "\\.rxml\\'" "Kirkfile\\'"
                 "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "Beanfile\\'")

(setq ruby-use-encoding-map nil)

;; Ruby compilation
(require-package 'ruby-compilation)

(eval-after-load "ruby-mode"
  '(lambda ()
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)
     (add-hook 'ruby-mode-hook
               (lambda ()
                 (unless (derived-mode-p 'prog-mode)
                   (run-hooks 'prog-mode-hook))))))

(add-hook 'ruby-mode-hook (lambda ()
                            (yas-minor-mode-on)))

;; Flycheck
;;(require-package 'flycheck)
;;(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-hook 'ruby-mode-hook
          (lambda ()
            (require 'diminish)
(diminish 'robe-mode)
(diminish 'auto-complete-mode)
(diminish 'whitespace-mode)
(diminish 'yas-minor-mode)
;;(diminish 'flycheck-mode)
(diminish 'eldoc-mode) ;; Fix this fucking thing.
))

(provide 'init-ruby-mode)
