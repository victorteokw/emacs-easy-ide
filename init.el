;;; This config is designed to be an easy IDE.

;;: This config is currently only support emacs 23.
;;; Code:

(let ((min_ver 24))
  (unless (>= emacs-major-version min_ver)
    (error "This config requires v%s or higher" min_ver)))

(add-to-list 'load-path (expand-file-name "user" user-emacs-directory))

(defconst *is-a-mac* (eq system-type 'darwin))

(require 'init-basic)
(require 'init-my-tutorial)
(require 'init-splash)

(provide 'init)
;;; init.el ends here
