(require 'auto-complete)
(require 'slim-mode)

;; Completion for slim mode
(defun eide-slim-setup-auto-completion ()
  "You know what it means."
  (require 'ac-slim)
  (require 'ac-html-default-data-provider)
  (require 'ac-html-testing-data-provider)

  (ac-html-enable-data-provider 'ac-html-default-data-provider)
  (ac-html-enable-data-provider 'ac-html-testing-data-provider)
  (ac-slim-setup)

  (setq ac-sources '())
  (add-to-list 'ac-sources 'ac-source-slim-tag)
  (add-to-list 'ac-sources 'ac-source-slim-attr)
  (add-to-list 'ac-sources 'ac-source-slim-attrv)
  (add-to-list 'ac-sources 'ac-source-slim-class)
  (add-to-list 'ac-sources 'ac-source-slim-id)

  (auto-complete-mode))

(add-hook 'slim-mode-hook 'eide-slim-setup-auto-completion)

;; Code folding

(add-hook 'slim-mode-hook 'origami-mode)

(add-to-list 'origami-parser-alist '(slim-mode . origami-indent-parser))

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

;; (defun eide-slim-indent-guide ()
;;   "Not documented yet."
;;   (require 'indent-guide)
;;   (setq indent-guide-recursive t)
;;   (indent-guide-mode))

;; (add-hook 'slim-mode-hook 'eide-slim-indent-guide)


(provide 'eide-slim)
