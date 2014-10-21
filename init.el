;;; init.el

;;; This file bootstraps the configuration, which is divided into a number of
;;; other files.
;;: This config is currently only support Emacs 24.

;;; Code:

;; Load setup file
(require 'setup)

;; Load my own core extension to "standard library"
(require 'core-extension)

;; Load user interface
(require 'user-interface)

;;(require 'project-additional)
(require 'buffer-additional)

;;(require-set 'basic-editing-additional)
;;(require-language 'ruby)
;;(require-set 'python)

;; Load user preferences
(require 'custom-preferences)

(provide 'init)

;;; init.el ends here
