;; Initialize cask and pallet
(require 'cask (expand-file-name "$HOME/.cask/cask.el"))
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

(defun require-package (arg1 &optional arg2 arg3)
  (require arg1))

(defun maybe-require-package (package &optional min-version no-refresh)
  (require-package package min-version no-refresh))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Copied from Purcell's config
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp)
(require 'init-pkg)
(require 'init-exec-path)

;; This directory is the parent directory for all the language supports
(defconst user-configuration-directory
  (expand-file-name "conf" user-emacs-directory)
  "The directory is under `user-emacs-directory' and is the directory that
contains all the programming language support configuration files.")



(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)
(require 'init-gui-frames)
(require 'init-proxies)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)

(require 'init-recentf)
(require 'init-ido)
(require 'init-hippie-expand)
(require 'init-auto-complete)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)
(require 'init-mmm)

(require 'init-editing-utils)

(require 'init-vc)
(require 'init-git)
(require 'init-github)

(require 'init-compile)
(require 'init-crontab)
(require 'init-textile)
(require 'init-markdown)
(require 'init-csv)
(require 'init-javascript)
(require 'init-php)
(require 'init-org)
(require 'init-nxml)
(require 'init-html)
(require 'init-css)
(require 'init-haml)
(require 'init-python-mode)
(require 'init-ruby-mode)
(require 'init-rails)
(require 'init-sql)

(require 'init-paredit)
(require 'init-lisp)
(require 'init-slime)
(require 'init-common-lisp)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)

(require 'init-dash)
(require 'init-ledger)
;; Extra packages which don't require any configuration

(require 'gnuplot)
(require 'lua-mode)
(require 'htmlize)
(require 'dsvn)
(when *is-a-mac*
  (require 'osx-location))
(require 'regex-tool)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)



;; (require 'init-lisp)
;; (require 'init-javascript)
;; (require 'init-html)
;; (require 'init-ruby)
;; (require 'init-php)
;; (require 'init-md)

;; (require 'init-xcode)
;; (require 'init-osx)

;; (require 'init-apache)
;; (require 'init-nginx)

;; Load user preferences
;; (require 'custom)

;; Load private configurations
;; (require-private)

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
