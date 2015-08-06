;; Emacs lisp

;; .el file loading
(setq load-prefer-newer t)

;; open Cask file in elisp-mode
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))

(defun hy-lang-elisp-click-to-jump (event)
  "you know"
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (goto-char (posn-point (event-start event)))
  (elisp-slime-nav-find-elisp-thing-at-point (elisp-slime-nav--read-symbol-at-point)))

(defun ky/elisp-common-setup ()
  "Elisp common setup."
  ;; jump
  (turn-on-elisp-slime-nav-mode)
  (define-key elisp-slime-nav-mode-map [M-mouse-1] 'hy-lang-elisp-click-to-jump)
  ;; rainbow parens
  (rainbow-delimiters-mode)
  ;; editing
  (paredit-mode)
  (define-key paredit-mode-map (kbd "M-<left>") 'paredit-wrap-sexp)
  ;; identation
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  ;; highlight sexp
  ;; (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
  ;; short documentation
  (eldoc-mode)
  ;; show parens
  (show-paren-mode)
  ;; yasnippet
  (yas-minor-mode)

  ;; auto complete
  (require 'auto-complete)
  (setq ac-ignore-case nil)
  (setq ac-sources '(ac-source-yasnippet
                     ac-source-dictionary
                     ac-source-features
                     ac-source-functions
                     ac-source-symbols
                     ac-source-variables
                     ac-source-words-in-same-mode-buffers
                     ))
  (auto-complete-mode t))

(defun ky/elisp-spec-setup ()
  "Elisp editing setup."
  ;; clean whitespace
  (add-hook 'before-save-hook 'whitespace-cleanup)
  ;; check parents when saving
  (add-hook 'after-save-hook #'check-parens nil t))

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'ky/elisp-common-setup))

(add-hook 'emacs-lisp-mode-hook 'ky/elisp-spec-setup)

;; Flycheck elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (flycheck-mode)
            (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
            ))

(provide 'hy-lang-elisp)
