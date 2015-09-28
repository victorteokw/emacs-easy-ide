;;; Electric pair

(add-hook 'objc-mode-hook 'electric-pair-mode)

;;; irony

(add-hook 'objc-mode-hook 'irony-mode)

;;; Auto complete

(add-hook 'objc-mode-hook 'eide-c-auto-complete)

;;; Snippets

(add-hook 'objc-mode-hook 'yas-minor-mode-on)

;;; Syntax checking

(add-hook 'objc-mode-hook 'flycheck-mode)

;;; Man page

(defun eide-c-man-page ()
  (local-set-key (kbd "s-M") 'helm-man-woman))

(add-hook 'objc-mode-hook 'eide-c-man-page)

;;; iedit

(defun eide-iedit ()
  (local-set-key (kbd "C-c ;") 'iedit-mode))

;;; code folding

(add-hook 'objc-mode-hook 'origami-mode)

(provide 'eide-objc)
