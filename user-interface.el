;; Disable tool bar
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))

;; Disable splash screen
(setq inhibit-splash-screen t)

;; Disable beep
(setq visible-bell t)

;; Show line numbers and column numbers
(line-number-mode 1)
(column-number-mode 1)

;; Require the theme
(require 'aqua-theme)

;; Do not backup files
(setq make-backup-files nil)


(provide 'user-interface)
