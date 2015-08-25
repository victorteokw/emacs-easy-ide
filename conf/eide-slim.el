(require 'auto-complete)
(require 'slim-mode)

;; Completion for slim mode
;; (defun hy-lang-slim-setup-auto-completion ()
;;   "You know what it means."
;;   (require 'ac-slim)
;;   (setq ac-sources '())
;;   (add-to-list 'ac-sources 'ac-source-slim-tag)
;;   (add-to-list 'ac-sources 'ac-source-slim-attribute)
;;   (add-to-list 'ac-sources 'ac-source-slim-attribute-value)
;;   (auto-complete-mode))

;; (add-hook 'slim-mode-hook 'hy-lang-slim-setup-auto-completion)

(add-hook 'slim-mode-hook 'flycheck-mode)

(defun hy-lang-slim-new-line ()
  "Slim new line."
  (interactive)
  (newline-and-indent)
  (slim-electric-backspace 1))
(define-key slim-mode-map (kbd "RET") 'hy-lang-slim-new-line)

(defun hy-lang-slim-new-line-not-indent ()
  "Slim new line."
  (interactive)
  (newline))
(define-key slim-mode-map [C-return] 'hy-lang-slim-new-line-not-indent)

(defun hy-lang-slim-indent-right ()
  "Slim indent right."
  (interactive)
  (insert "  "))
(define-key slim-mode-map [C-tab] 'hy-lang-slim-indent-right)

(defun eide-slim-indent-guide ()
  "Not documented yet."
  (require 'indent-guide)
  (setq indent-guide-recursive t)
  (indent-guide-mode))

(add-hook 'slim-mode-hook 'eide-slim-indent-guide)

(provide 'eide-slim)
