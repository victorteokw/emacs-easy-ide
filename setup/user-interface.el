;; Disable tool bar
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))

;; Using meta-backtip to open menu bar
(global-set-key (kbd "M-`") 'menu-bar-open)

(setq-default
 inhibit-splash-screen t ;; disable splash screen
 visible-bell t ;; do not beep
 indent-tabs-mode nil ;; use whitespace to indent
 line-number-mode t ;; show line numbers
 column-number-mode t ;; and column numbers
 make-backup-files nil ;; do not backup files
 initial-scratch-message ";;Life is fun."
 ;; initial-major-mode 'ruby-mode
 )

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
