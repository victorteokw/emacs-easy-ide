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
  (add-hook 'ido-setup-hook (lambda ()
                              (define-key ido-completion-map [up]
                                'previous-history-element))))

(defun setup-projectile ()
  (require-package 'projectile)
  (projectile-global-mode)
  (diminish 'projectile-mode))

(defun setup-helm ()
  (require-package 'helm)
  (require 'helm-config))

(defun setup-helm-projectile ()
  (require-package 'helm-projectile))

(defun setup-grizzl ()
  (require-package 'grizzl)
  (setq projectile-completion-system 'grizzl))

(defun setup-diminish ()
  (require-package 'diminish))

(defun setup-auto-complete ()
  (require-package 'auto-complete)
  (setq ac-ignore-case nil))

(defun setup-yasnippet ()
  (require-package 'yasnippet))

(defun setup-magit ()
  (require-package 'magit)
  (global-set-key (kbd "M-/") 'magit-status))

(mapc (lambda (f) (add-hook 'after-init-hook f))
      (list 'setup-ido 'setup-projectile 'setup-helm 'setup-helm-projectile
            'setup-diminish 'setup-auto-complete 'setup-yasnippet
            'setup-magit))

;; Provide it
(provide 'engines)
