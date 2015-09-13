;;; eide boot file

;;; Use cask and pallet for package management

(require 'cask (expand-file-name "$HOME/.cask/cask.el"))
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;;; Setup directory structure

(require 'f)

(eval-when-compile
  (defconst eide-conf-dir (f-expand "conf/" user-emacs-directory)
    "Eide conf directory.")
  (defconst eide-etc-dir (f-expand "etc/" user-emacs-directory)
    "Eide etc directory.")
  (add-to-list 'load-path eide-conf-dir))

;;; Editor core

(require 'eide-environment)          ;; gnu-emacs or emacs-mac
(require 'eide-extension)            ;; eide library
(require 'eide-theme)                ;; theme
(require 'eide-ui)                   ;; User interface configuration
(require 'eide-editor)               ;; Editor configuration
(require 'eide-enable)               ;; For enable emacs command
(require 'eide-cvs)                  ;; git configuration
(require 'eide-key-bindings)         ;; for key bindings

;;; Programming Language

(require 'eide-prog)                 ;; All programming languages
(require 'eide-text)                 ;; All text languages

(require 'eide-elisp)                ;; elisp
(require 'eide-c)                    ;; C, C++, Objective-C
(require 'eide-ruby)                 ;; ruby
(require 'eide-php)                  ;; PHP

(require 'eide-javascript)           ;; javaScript
(require 'eide-typescript)           ;; typeScript
(require 'eide-coffeescript)         ;; coffeeScript

(require 'eide-html)                 ;; HTML
(require 'eide-slim)                 ;; slim
(require 'eide-haml)                 ;; haml
(require 'eide-erb)                  ;; erb

(require 'eide-css)                  ;; CSS
(require 'eide-sass)                 ;; Sass
(require 'eide-scss)                 ;; SCSS syntax Sass
(require 'eide-less)                 ;; Less

(require 'eide-yaml)                 ;; yaml
(require 'eide-json)                 ;; JSON

;;; Applicaiton Framework

(require 'eide-rails)                ;; For rails framework

;;; Operating System

(require 'eide-shell)                ;; bash

;;; GTD

(require 'eide-org)                   ;; Org

;;; Life and health

(require 'eide-health)               ;; Health

;;; Fixed up

(require 'eide-fixup)             ;; Fix the playsound feature of emacs

;;; Load custom file
;;; This file is generated by emacs automatically

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
