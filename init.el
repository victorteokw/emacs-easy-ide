;;; init.el

;;; This file bootstraps the configuration, which is divided into a number of
;;; other files.
;;: This config is currently only support Emacs 24.

;;; Code:

;; Prepare to load setup file
(add-to-list 'load-path user-emacs-directory)

;; Load setup file
(require 'setup)

;; Load my own core extension to "standard library"
(require 'core-extension)

;; Load user interface
(require 'user-interface)

;;(require 'project-additional)
(require 'buffer-additional)
(require 'editing-additional)

;; Require better emacs basic functionality
(require-set 'ido)

;; Require lisp so that I can develop this config with this config
(require-set 'lisp)

;; Require simulating xcode
(require-set 'xcode)

;;(require-set 'basic-editing-additional)
;;(require-language 'ruby)
;;(require-set 'python)

;; Load user preferences
(require 'custom-preferences)

(provide 'init)

;;; init.el ends here
