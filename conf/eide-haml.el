;;; Auto complete

(defun eide-haml-auto-complete ()
  (require 'ac-haml)
  (require 'ac-html-default-data-provider)
  (ac-html-enable-data-provider 'ac-html-default-data-provider)
  (ac-haml-setup)
  (setq ac-sources '(ac-source-haml-tag
                     ac-source-haml-attr
                     ac-source-haml-attrv))
  (auto-complete-mode))

(add-hook 'haml-mode-hook 'eide-haml-auto-complete)

;;; Check haml syntax

(add-hook 'haml-mode-hook 'flycheck-mode)

;;; Code folding

(add-hook 'haml-mode-hook 'origami-mode)

(add-to-list 'origami-parser-alist '(haml-mode . origami-indent-parser))

;;; Indent
(require 'smart-indent-mode)
(add-hook 'haml-mode-hook 'smart-indent-mode)

(provide 'eide-haml)
