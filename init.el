;;; init.el

;;; This file bootstraps the configuration, which is divided into a number of
;;; other files.
;;: This config is currently only support Emacs 24.

;;; Code:

;; Prepare to load setup file
(add-to-list 'load-path user-emacs-directory)

;; Load setup file
(require 'setup)

(require 'init-lisp)
(require 'init-javascript)
(require 'init-html)
(require 'init-ruby)
(require 'init-php)
(require 'init-md)

(require 'init-xcode)
(require 'init-osx)

(require 'init-apache)
(require 'init-nginx)

;; Load user preferences
(require 'custom)

;; Load private configurations
;; (require-private)

(provide 'init)
;;; init.el ends here
