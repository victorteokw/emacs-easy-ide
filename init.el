;;; init.el --- bootstrap file  -*- lexical-binding: t -*-

;; Maintainer: Zhang Kai Yu
;; Keywords: ide

;;; Commentary:

;;; This config is designed to be an easy IDE.
;;; This file bootstraps the configuration, which is divided into a number of
;;; other files
;;: This config is currently only support Emacs 24.

;;; Code:

(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "This config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "boot" user-emacs-directory))

(defconst *is-a-mac* (eq system-type 'darwin))

(require 'init-basic)
(require 'init-my-tutorial)
(require 'init-splash)
(require 'init-scroll)

(provide 'init)

;;; init.el ends here
