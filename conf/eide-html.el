;;; Open html with web mode

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;; Handy settings

(require 'web-mode)

(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-sql-indent-offset 2
              web-mode-enable-auto-pairing t
              web-mode-enable-auto-closing t
              web-mode-enable-auto-quoting nil)

(eval-after-load "web-mode"
  '(progn
     (setq web-mode-ac-sources-alist
           '(("css" . (ac-source-words-in-buffer ac-source-css-property))
             ("html" . (ac-source-html-tag
                        ac-source-html-attr
                        ac-source-html-attrv))
             ("php" . (ac-source-words-in-buffer
                       ac-source-words-in-same-mode-buffers
                       ac-source-dictionary))))))

(defun eide-html-auto-completion ()
  (require 'ac-html)
  (require 'ac-html-default-data-provider)
  (ac-html-enable-data-provider 'ac-html-default-data-provider)
  (ac-html-setup)
  (auto-complete-mode))

(add-hook 'web-mode-hook 'eide-html-auto-completion)

;;; Emmet edit

(add-hook 'web-mode-hook 'emmet-mode)

;;; elnode is a super engine

;;(require 'elnode)

;;; restclient

;;(require 'restclient)


(provide 'eide-html)
