(require-package 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . web-mode))
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)
(provide 'init-html)
