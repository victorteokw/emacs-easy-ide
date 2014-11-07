;;; init.el

;;; This file bootstraps the configuration, which is divided into a number of
;;; other files.
;;: This config is currently only support Emacs 24.

;;; Code:

;; Prepare to load setup file
(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))

;; Load setup file
(require 'setup)

;; Load my own core extension to "standard library"
(require 'core-extension)

;; Load user interface
(require 'user-interface)

(require 'project-additional)
(require 'buffer-additional)
(require 'editing-additional)
(require 'language-additional)

(require-set 'version-control)

;; Require better emacs basic functionality
(require-set 'ido)

;; Require lisp so that I can develop this config with this config
(require-set 'lisp)

;; Require browser
(require-set 'browser)

;; Require simulating xcode
(require-set 'xcode)

;; Require html
(require-set 'html)

;; Require javaScript
(require-set 'javascript)

;; Require ruby
(require-set 'ruby)

;; Require php
(require-set 'php)

;; Require markdown
(require-set 'md)

;;(require-language 'python)

;; Load user preferences
(require 'custom-preferences)

(provide 'init)

;;; init.el ends here
