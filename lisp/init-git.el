(defun import-and-merge-git-config ()
  "Merge current logged in unix user's '.gitconfig' to current emacs version."
  (interactive)
  (let* ((git-config-file-emacs
          (expand-file-name "gitconfig.txt" user-emacs-directory))
         (git-config-file-user
          (substitute-in-file-name "$HOME/.gitconfig")))

    (unless (file-exists-p git-config-file-user)
      (error "User .gitconfig not exist."))

    (with-current-buffer (find-file-noselect git-config-file-user)
      (if (file-exists-p git-config-file-emacs)
          (progn
            (append-to-file "## Last Import\n" nil git-config-file-emacs)
            (append-to-file nil nil git-config-file-emacs)
            )
        (progn
          (write-file git-config-file-emacs nil))))))

(defun git-config ()
  "Visit git config file in emacs dir."
  (interactive)
  (let* ((git-config-file-emacs
          (expand-file-name "gitconfig.txt" user-emacs-directory)))
    (find-file git-config-file-emacs)))

(defun setup-git-config ()
  "Duplicate git config to '~/.gitconfig'."
  (interactive)
  (let* ((git-config-file-emacs
          (expand-file-name "gitconfig.txt" user-emacs-directory))
         (git-config-file-user
          (substitute-in-file-name "$HOME/.gitconfig")))
    (unless (file-exists-p git-config-file-emacs)
      (error "You are not import git config."))
    (if (yes-or-no-p "DANGEROUS: this may rewrite your ~/.gitconfig file. \
Really do this?")
        ;; copy file
        (with-current-buffer (find-file-noselect git-config-file-emacs)
          (write-file git-config-file-user t)))))

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
  (ac-rlc-setup-sources))

(provide 'init-git)
