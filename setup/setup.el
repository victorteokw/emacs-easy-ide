;;; setup.el

;;; This file setup the environment this configuration needs

;;; Code:

;; This directory is the parent directory for all the language supports
(defconst user-configuration-directory
  (expand-file-name "configurations" user-emacs-directory)
  "The directory is under `user-emacs-directory' and is the directory that
contains all the programming language support configuration files.")

(defconst old-packages-directory
  (expand-file-name "old-packages" user-emacs-directory)
  "This directory contains old and obsolete packages. But they are powerful.")

(defun require-old-package (package)
  (let* ((package-string (symbol-name package))
	 (package-dir (expand-file-name package-string
					old-packages-directory)))
    
    (if (file-directory-p package-dir)
	(add-to-list 'load-path package-dir)
      (message "Cannot load old package '%s'" package))
    (require package)))

;; Configure the package system
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
;; Initialize the package system
(setq package-enable-at-startup nil)
(package-initialize)

;; Use this function to lazily require package
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun require-set (feature-set)
  "Require a feature set. It is always a programming language name.
The FEATURE-SET is a directory under `user-configuration-directory' "
  ;; create user-configuration-directory if not exists.
  (unless (file-directory-p user-configuration-directory)
    (if (file-exists-p user-configuration-directory)
        (error "Unexpected file exist at '%s'" user-configuration-directory))
    (make-directory user-configuration-directory))
  ;; require files
  (let* ((feature-dir-name (symbol-name feature-set))
         (feature-dir-full-name
          (expand-file-name feature-dir-name user-configuration-directory)))
    (load-directory
     feature-dir-full-name
     "Feature directory '%s' is neither a directory nor exist.")))

(defun load-directory (dir-name &optional error-string)
  "Load all files from a directory"
  (or (file-directory-p dir-name)
      (error (or error-string "'%s' is neither a directory nor exist.")
             dir-name))
  (dolist (file (directory-files dir-name t "^[^.]"))
    (if (file-directory-p file)
        (load-directory file)
      (load-file file)
      (message "File loaded: %s" file))))

(add-to-list 'load-path user-configuration-directory)

;; Set custom file
(setq custom-file (expand-file-name "setup/custo-mpreferences.el"
				    user-emacs-directory))

(provide 'setup)
;;; setup.el ends here
