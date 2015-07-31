(defun hy-core-conf-goto-user-init-file ()
  "Go to user init file."
  (interactive)
  (let ((current-file-name (buffer-file-name (current-buffer))))
    (if (string= current-file-name user-init-file)
        (dired (expand-file-name "conf" user-emacs-directory))
      (find-file user-init-file))))
(global-set-key [C-f11] 'hy-core-conf-goto-user-init-file)


(provide 'hy-core-conf)
