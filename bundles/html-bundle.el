;; web
(require 'web-mode)
(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-sql-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . web-mode))
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)


;; completion
(require 'ac-html)
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-words-in-buffer ac-source-css-property))
        ("html" . (ac-source-html-tag
                   ac-source-html-attribute
                   ac-source-html-attribute-value
                   ))
        ("php" . (ac-source-words-in-buffer
                  ac-source-words-in-same-mode-buffers
                  ac-source-dictionary))))
(setq web-mode-enable-auto-quoting nil)
(add-hook 'web-mode-hook 'auto-complete-mode)


(add-hook 'slim-mode-hook
          (lambda ()
            (require 'ac-slim)
            (setq ac-sources '(ac-source-slim-attribute))
            (add-to-list 'ac-sources 'ac-source-slim-tag)
            (add-to-list 'ac-sources 'ac-source-slim-attribute-value)
            (auto-complete-mode)
            ))

(add-hook 'haml-mode-hook
          (lambda ()
            (require 'ac-haml)
            (setq ac-sources '(ac-source-haml-attribute))
            (add-to-list 'ac-sources 'ac-source-haml-tag)
            (add-to-list 'ac-sources 'ac-source-haml-attribute-value)
            (auto-complete-mode)
            ))


(setq css-indent-offset 2)
(setq scss-compile-at-save nil)

;; elnode is a super engine
(require 'elnode)

;; restclient
(require 'restclient)


(provide 'html-bundle)
