;; ruby editing features

;; auto insert end
(require-package 'ruby-end)

;; auto cleanup trailing whitespaces
(require-package 'whitespace-cleanup-mode)

;; End of line and return
(defun ruby-end-of-line-and-return ()
  "Move to the end of line and return."
  (interactive)
  (end-of-line)
  (ruby-end-return))

(setq ruby-end-insert-newline nil)

(add-hook 'ruby-mode-hook
          '(lambda ()
             (whitespace-cleanup-mode t)
             (yas-reload-all)
             (add-hook 'before-save-hook 'whitespace-cleanup)
             (require 'diminish)
             (diminish 'ruby-end-mode)
             (diminish 'robe-mode)
             (diminish 'auto-complete-mode)
             (diminish 'whitespace-mode)
             (diminish 'yas-minor-mode)
             (diminish 'whitespace-cleanup-mode)
             (diminish 'flycheck-mode)
             (diminish 'eldoc-mode) ;; Fix this fucking thing.
             ))
