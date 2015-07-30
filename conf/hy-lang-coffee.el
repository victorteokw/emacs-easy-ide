;; coffeeScript

(require 'coffee-mode)

(defun ky/coffee-new-line-at-end-and-indent ()
  "Move to the end of line and indent like coffee."
  (interactive)
  (move-end-of-line 1)
  (coffee-newline-and-indent))

(define-key coffee-mode-map [s-return] 'ky/coffee-new-line-at-end-and-indent)

(require 'coffee-mode)


(add-hook 'coffee-mode-hook
          (lambda ()
            ;; use yard mode for highlight documentation
            (yard-mode)
            ;; Clean whitespace
            (add-hook 'before-save-hook 'whitespace-cleanup)
            ;; use snippets
            (yas-minor-mode-on)
            ;; syntax checking
            (setq flycheck-checker 'coffee)
            (flycheck-mode)
            ;; JST mode
            (if (fboundp 'jst-enable-appropriate-mode)
                (jst-enable-appropriate-mode))
            ))

(provide 'hy-lang-coffee)
