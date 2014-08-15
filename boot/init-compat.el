;;; init-compat.el --- compatibility initialization -*- lexical-binding: t -*-

;; Maintainer: Zhang Kai Yu
;; Keywords: ide

;;; Commentary:

;;; Compatibility code is here

;;; Code:

;; Restore removed var alias, used by ruby-electric-brace and others
(unless (boundp 'last-command-char)
  (defvaralias 'last-command-char 'last-command-event))

(provide 'init-compat)

;;; init-compat.el ends here
