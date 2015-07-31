(defconst hy-core-conf-user-conf-dir
  (expand-file-name "conf/" user-emacs-directory)
  "User configuration directory.")

(defun hy-core-conf-goto-user-init-file ()
  "Go to user init file."
  (interactive)
  (let ((current-file-name (buffer-file-name (current-buffer))))
    (cond ((string= current-file-name user-init-file)
           (dired hy-core-conf-user-conf-dir))

          ((and (string= (symbol-name major-mode) "dired-mode")
                (string= (dired-current-directory) hy-core-conf-user-conf-dir))
           (ido-find-file-in-dir hy-core-conf-user-conf-dir))

          (t (find-file user-init-file)))))
(global-set-key [C-f11] 'hy-core-conf-goto-user-init-file)

(defun hy-core-conf-goto-readme ()
  "Go to user readme file."
  (interactive)
  (find-file (expand-file-name "README.org" user-emacs-directory)))
(global-set-key [M-f11] 'hy-core-conf-goto-readme)

(provide 'hy-core-conf)
