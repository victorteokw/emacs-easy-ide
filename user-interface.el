;; Disable tool bar
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))

;; Disable splash screen
(setq inhibit-splash-screen t)

;; Disable beep
(setq visible-bell t)

;; Require the theme
(require 'aqua-theme)

(provide 'user-interface)
