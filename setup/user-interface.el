;; Disable tool bar
;;(if (functionp 'tool-bar-mode) (tool-bar-mode 0))

;; Using meta-backtip to open menu bar
(define-key global-map (kbd "M-`") 'menu-bar-open)

(setq-default
 inhibit-splash-screen t ;; disable splash screen
 visible-bell t ;; do not beep
 indent-tabs-mode nil ;; use whitespace to indent
 line-number-mode t ;; show line numbers
 column-number-mode t ;; and column numbers
 make-backup-files nil ;; do not backup files
 initial-scratch-message ""
 tab-width 2
 ;; initial-major-mode 'ruby-mode
 )

(if (display-graphic-p)
    (progn
      (require-package 'solarized-theme)
      (load-theme 'solarized-dark t)
      (setenv "PATH" (concat  "/usr/local/bin:" (getenv "PATH")))))

(provide 'user-interface)
