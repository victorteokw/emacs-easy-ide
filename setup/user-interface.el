;; Disable tool bar
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))

;; Disable splash screen
(setq inhibit-splash-screen t)

(setq-default
 visible-bell t ;; do not beep
 indent-tabs-mode nil ;; use whitespace to indent
 line-number-mode t ;; Show line numbers
 column-number-mode t ;; and column numbers
 show-trailing-whitespace t ;; show trailing whitespace
 )

;; Require my theme
(require 'aqua-theme)

;; Require more themes
(require-package 'color-theme-sanityinc-solarized)

;; Do not backup files
(setq make-backup-files nil)

;; Initial scratch message
(setq initial-scratch-message "#I'm happy to live a ruby life.\n\n")

;; Initial major mode
(setq initial-major-mode 'ruby-mode)
(provide 'user-interface)
