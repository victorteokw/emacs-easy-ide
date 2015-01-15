;;; init.el

;;; This file bootstraps the configuration, which is divided into a number of
;;; other files.
;;: This config is currently only support Emacs 24.

;;; Code:

;; Prepare to load setup file
(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))

;; Load setup file
(require 'setup)
(require 'engines)
(require 'language)
(require 'buffer)
(require 'editing)
(require 'private)

;; Load user interface
(require 'user-interface)
;; (require-set 'version-control) TODO: Remove this

(require-set 'lisp)
(require-set 'javascript)
(require-set 'html)
(require-set 'ruby)
(require-set 'php)
(require-set 'markdown)
;;(require-language 'python)

(require-set 'xcode)
(require-set 'osx)

(require-set 'apache)

;; Load user preferences
(require 'custom-preferences)

;; Load private configurations
(require-private)

(provide 'init)
;;; init.el ends here
