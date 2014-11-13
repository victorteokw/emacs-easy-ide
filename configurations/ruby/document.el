;;; Robe
(require-package 'robe)
(after-load 'ruby-mode
  (add-hook 'ruby-mode-hook 'robe-mode))
(after-load 'robe
  (add-hook 'robe-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-robe))))

;; ri
(require-package 'yari)
(defalias 'ri 'yari)
(eval-after-load 'ruby-mode '(define-key ruby-mode-map (kbd "C-c C-y") 'yari-helm))



