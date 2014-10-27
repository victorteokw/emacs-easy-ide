;;; Robe
(require-package 'robe)
(after-load 'ruby-mode
  (add-hook 'ruby-mode-hook 'robe-mode))
(after-load 'robe
  (add-hook 'robe-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-robe))))
