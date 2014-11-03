;;; ido/main.el

(defun setup-ido ()
  "This function setup ido everywhere."
  ;; configure ido
  (require 'ido)
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length 0)
  (setq ido-use-virtual-buffers t)

  ;; flx-ido
  (require-package 'flx-ido)
  (flx-ido-mode t)
  (setq ido-use-faces nil)

  ;; configure ido-ubiquitous
  (require-package 'ido-ubiquitous)
  (ido-ubiquitous-mode t)

  ;; Use smex to handle M-x
  (require-package 'smex)
  ;; Change path for ~/.smex-items
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  ;; Remap M-x to smex
  (global-set-key [remap execute-extended-command] 'smex)

  ;; Configure idomenu
  (require-package 'idomenu)

  ;; Allow the same buffer to be open in different frames
  (setq ido-default-buffer-method 'selected-window)

  ;; http://www.reddit.com/r/emacs/comments/21a4p9/use_recentf_and_ido_together/cgbprem
  (add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element))))

(add-hook 'after-init-hook 'setup-ido)

;; Provide it
(provide 'init-ido)
