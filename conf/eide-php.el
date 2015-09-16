;;; REPL

(defvar eide-php-repl-last-buffer ()
  nil)

(defadvice inf-php (before eide-remember-last-file activate)
  (setq eide-php-repl-last-buffer (current-buffer)))

(defun eide-php-back-to-file ()
  (interactive)
  (switch-to-buffer-other-window eide-php-repl-last-buffer))

(eval-after-load "php-mode"
  '(define-key php-mode-map (kbd "C-c C-z") 'inf-php))

(eval-after-load "inf-php"
  '(define-key inf-php-mode-map (kbd "C-c C-z") 'eide-php-back-to-file))

(provide 'eide-php)
