;; Syntax checking

(add-hook 'less-css-mode 'flycheck-mode)

;;; Electric pair

(add-hook 'less-css-mode-hook 'electric-pair-mode)

;;; Rainbow color

(add-hook 'less-css-mode-hook 'rainbow-mode)

;; Code folding

(add-hook 'less-css-mode-hook 'origami-mode)

(add-to-list 'origami-parser-alist '(less-css-mode . origami-c-style-parser))

;;; Auto complete

(defun eide-less-auto-complete ()
  (require 'auto-complete)
  (setq ac-sources '(ac-source-css-property))
  (auto-complete-mode))

(add-hook 'less-css-mode-hook 'eide-scss-auto-complete)


(provide 'eide-less)
