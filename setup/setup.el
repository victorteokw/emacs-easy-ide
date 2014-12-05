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

(defun require-old-package (package &optional do-not-require)
  (let* ((package-string (symbol-name package))
	 (package-dir (expand-file-name package-string
					old-packages-directory)))

    (if (file-directory-p package-dir)
	(add-to-list 'load-path package-dir)
      (message "Cannot load old package '%s'" package))
    (unless do-not-require (require package))))

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
(setq custom-file (expand-file-name "setup/custom-preferences.el"
				    user-emacs-directory))

;; Handier way to add modes to auto-mode-alist.
(defun auto-major-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE'
for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; return all matches from string
(defun string-all-matches (regex str &optional group)
  "Find all match for `REGEX' within `STR', returning the full
match string or group `GROUP'."
  (let ((result nil) (pos 0) (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

;; This variable alias is for compatibility.
;; Some ruby package may use this.
(unless (boundp 'last-command-char)
  (defvaralias 'last-command-char 'last-command-event))

;; Use this constant to determine if this computer is a mac.
(defconst this-computer-is-mac (eq system-type 'darwin)
  "Use this constant to determine if this computer is a mac")
(defvaralias 'this-computer-is-mac 'current-operating-system-is-os-x)
(defvaralias 'this-computer-is-mac 'operating-system-is-os-x)

(provide 'setup)
;;; setup.el ends here
