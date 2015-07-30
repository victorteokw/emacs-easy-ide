(defun hy-core-splash-page-buffer ()
  "show the initial screen for the easy omni ide"
  (let ((ide-buffer (get-buffer-create "*welcome back*")))
    (with-current-buffer ide-buffer
      (insert "Ensogo projects:\n")
      (hy-core-splash-page-insert-button-for-dir "/Users/1/Developer/Ensogo")
      (insert "\nMy projects:\n")
      (hy-core-splash-page-insert-button-for-dir "/Users/1/Developer/")
      (insert "\nEmacs config:\n")
      (hy-core-splash-page-insert-button-for-file "/Users/1/.emacs.d/init.el")
      (switch-to-buffer ide-buffer)
      (read-only-mode))))

(add-hook 'after-init-hook 'hy-core-splash-page-buffer)

(defun hy-core-splash-page-insert-button-for-dir (dir-name)
  (f-directories dir-name 'hy-core-splash-page-insert-button-for-file nil))

(defun hy-core-splash-page-insert-button-for-file (file-name)
  (insert-button file-name 'action 'hy-core-splash-page-goto
                 'mouse-action 'hy-core-splash-page-goto)
  (insert "\n"))

(defun hy-core-splash-page-goto (button)
  "go to the proj."
  (ffap))

(provide 'hy-core-splash-page)
