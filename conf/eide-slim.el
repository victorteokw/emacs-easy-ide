;;; Auto complete

(defun eide-slim-auto-complete ()
  "You know what it means."
  (require 'ac-slim)
  (require 'auto-complete)
  (require 'ac-html-default-data-provider)
  (ac-html-enable-data-provider 'ac-html-default-data-provider)
  (ac-slim-setup)
  (setq ac-sources '())
  (add-to-list 'ac-sources 'ac-source-slim-tag)
  (add-to-list 'ac-sources 'ac-source-slim-attr)
  (add-to-list 'ac-sources 'ac-source-slim-attrv)
  (add-to-list 'ac-sources 'ac-source-slim-class)
  (add-to-list 'ac-sources 'ac-source-slim-id)
  (auto-complete-mode))

(add-hook 'slim-mode-hook 'eide-slim-auto-complete)

;;; Code folding

(add-hook 'slim-mode-hook 'origami-mode)

(add-to-list 'origami-parser-alist '(slim-mode . origami-indent-parser))

;;; Indent

(add-hook 'slim-mode-hook 'smart-indent-mode)

(provide 'eide-slim)
