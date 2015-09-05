;; coffeeScript

(require 'coffee-mode)

;; highlight in documentation

(add-hook 'coffee-mode-hook 'yard-mode)

;; syntax checking

(add-hook 'coffee-mode-hook 'flycheck-mode)

;; JST

(if (fboundp 'jst-enable-appropriate-mode)
    (add-hook 'coffee-mode-hook 'jst-enable-appropriate-mode))

;; clear whitespace

(add-hook 'coffee-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'whitespace-cleanup)))

(defun ky/coffee-new-line-at-end-and-indent ()
  "Move to the end of line and indent like coffee."
  (interactive)
  (move-end-of-line 1)
  (coffee-newline-and-indent))

(define-key coffee-mode-map [s-return] 'ky/coffee-new-line-at-end-and-indent)

;; Code folding

(add-hook 'coffee-mode-hook 'origami-mode)

(add-to-list 'origami-parser-alist '(coffee-mode . origami-indent-parser))

(add-hook 'coffee-mode-hook 'electric-operator-mode)

(provide 'eide-coffeescript)
