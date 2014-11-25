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

;; Using meta-backtip to open menu bar
(global-set-key (kbd "M-`") 'menu-bar-open)

;; Require my theme
;;(require 'aqua-theme)

;; Require more themes
;;(require-package 'color-theme-sanityinc-solarized)
;;(load-theme 'sanityinc-solarized-light t)
;; Do not backup files
(setq make-backup-files nil)

;; Initial scratch message
(setq initial-scratch-message ";;Have Fun!\n\n")

;; Initial major mode
;;(setq initial-major-mode 'ruby-mode)

;; auto complete for shell
(add-hook 'shell-mode-hook 'setup-shell-ac)
(defun setup-shell-ac ()
  (require-package 'readline-complete)
  (require-package 'auto-complete)
  (add-to-list 'ac-modes 'shell-mode)
  (setq explicit-shell-file-name "bash")
  (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
  (setq comint-process-echoes t)
  (ac-config-default)

  (require 'readline-complete)
  (ac-rlc-setup-sources)
  )

(provide 'user-interface)
