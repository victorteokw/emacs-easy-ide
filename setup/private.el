(defconst user-private-config-directory
  (expand-file-name "private" user-emacs-directory)
  "This directory is under `user-emacs-directory' and is the directory that 
contains all users' private configuration files. These normally include 
corporation project configuration, corporation code, personal project code 
etc.")

(defun rq--load-directory (dir-name &optional error-string)
  "Load all files from a directory."
  (or (file-directory-p dir-name)
      (error (or error-string "'%s' is neither a directory nor exist.")
             dir-name))
  (dolist (file (directory-files dir-name t "^[^.]"))
    (if (file-directory-p file)
        (rq--load-directory file)
      (load-file file))))

(defun require-private ()
  "Require private configurations which located at '~/.emacs.d/private'.
If the private folder does not exist, create by default."
  (interactive)
  (if (and (file-exists-p user-private-config-directory)
           (file-directory-p user-private-config-directory))
      (rq--load-directory user-private-config-directory)
    (if (not (file-exists-p user-private-config-directory))
        (make-directory user-private-config-directory)
      (error "You may delete ~/.emacs.d/private by hand in order 
to require private package."))))

(defun backup-private ()
  "Backup all private files."
  (interactive)) ;; TODO

(defun recover-private ()
  "Recover all private files."
  (interactive)) ;; TODO

(provide 'private)
;;; private.el ends here
