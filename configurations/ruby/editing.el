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
             ))
