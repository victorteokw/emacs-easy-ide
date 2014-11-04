;; Basic ruby setup
(require-package 'ruby-mode)
(require-package 'ruby-hash-syntax)

(auto-major-mode 'ruby-mode
                 "\\.rxml\\'" "Kirkfile\\'"
                 "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'"
                 )

(setq ruby-use-encoding-map nil)

;; Inferior ruby
(require-package 'inf-ruby)
(require-package 'ac-inf-ruby)
(after-load 'auto-complete
  (add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)
(after-load 'inf-ruby
  (define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete))

;; Ruby compilation
(require-package 'ruby-compilation)

;; ri
(require-package 'yari)
(defalias 'ri 'yari)

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
                            (yas-minor-mode-on)
                            (substitute-key-definition
			     'ruby-end-return
			     'ruby-end-of-line-and-return ruby-end-mode-map)
                            ))

(provide 'init-ruby-mode)
