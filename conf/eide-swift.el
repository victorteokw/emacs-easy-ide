;; Syntax Checking

;; (add-hook 'swift-mode-hook 'flycheck-mode)

;; REPL

(defvar eide-swift-repl-last-buffer nil)

(defadvice swift-mode-run-repl
    (before eide-remember-last-file activate)
  (setq eide-swift-repl-last-buffer (current-buffer)))

(defun eide-swift-back-to-file ()
  (interactive)
  (switch-to-buffer-other-window eide-swift-repl-last-buffer))

(eval-after-load "swift-mode"
  '(define-key swift-repl-mode-map (kbd "C-c C-z") 'eide-swift-back-to-file))



(provide 'eide-swift)
