;;; Electric pair

(add-hook 'c++-mode-hook 'electric-pair-mode)

;;; irony

(add-hook 'c++-mode-hook 'irony-mode)

;;; Auto complete

(add-hook 'c++-mode-hook 'eide-c-auto-complete)

;;; Snippets

(add-hook 'c++-mode-hook 'yas-minor-mode-on)

;;; Syntax checking

(add-hook 'c++-mode-hook 'flycheck-mode)

;;; Man page

(defun eide-c-man-page ()
  (local-set-key (kbd "s-M") 'helm-man-woman))

(add-hook 'c++-mode-hook 'eide-c-man-page)

;;; iedit

(defun eide-iedit ()
  (local-set-key (kbd "C-c ;") 'iedit-mode))

;;; code folding

(add-hook 'c++-mode-hook 'origami-mode)

(provide 'eide-c++)
