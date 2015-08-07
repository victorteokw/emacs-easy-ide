;; Use web mode for html files and erb files
(require 'web-mode)
(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-sql-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)

;; completion for web mode
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

;; elnode is a super engine
;;(require 'elnode)

;; restclient
;;(require 'restclient)


(provide 'hy-lang-html)
