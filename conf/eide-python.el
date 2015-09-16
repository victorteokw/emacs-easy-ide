;;; REPL

(require 'python)

(defvar eide-python-repl-last-buffer nil)

(defadvice python-shell-switch-to-shell
    (before eide-remember-last-file activate)
  (setq eide-python-repl-last-buffer (current-buffer)))

(defun eide-python-back-to-file ()
  (interactive)
  (switch-to-buffer-other-window eide-python-repl-last-buffer))

(define-key inferior-python-mode-map (kbd "C-c C-z") 'eide-python-back-to-file)

(provide 'eide-python)
