;;; User emacs configuration boot file
;;; All the configuration files resides in ~/.emacs.d/conf

;; Use cask and pallet for package management
(require 'cask (expand-file-name "$HOME/.cask/cask.el"))
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;;; Setup directory structure

(require 'f)

(defconst eide-conf-dir (f-expand "conf/" user-emacs-directory)
  "Eide conf directory.")

(defconst eide-etc-dir (f-expand "etc/" user-emacs-directory)
  "Eide etc directory.")

(eval-when-compile (add-to-list 'load-path eide-conf-dir))

;;; Editor core

(require 'eide-extension)            ;; eide library
(require 'eide-ui)                   ;; User interface configuration
(require 'eide-editor)               ;; Editor configuration
(require 'eide-enable)               ;; For enable emacs command
(require 'eide-cvs)                  ;; git configuration
(require 'eide-key-bindings)         ;; for key bindings

;;; Programming Language

(require 'hy-lang-prog)                 ;; All programming languages
(require 'hy-lang-text)                 ;; All text languages

(require 'hy-lang-elisp)                ;; elisp
(require 'hy-lang-c)                    ;; C, C++, Objective-C
(require 'hy-lang-ruby)                 ;; ruby
(require 'hy-lang-php)                  ;; PHP

(require 'hy-lang-javascript)           ;; javaScript
(require 'hy-lang-coffee)               ;; coffeeScript

(require 'hy-lang-html)                 ;; HTML
(require 'hy-lang-slim)                 ;; slim
(require 'hy-lang-haml)                 ;; haml
(require 'hy-lang-erb)                  ;; erb

(require 'hy-lang-css)                  ;; CSS
(require 'hy-lang-sass)                 ;; Sass
(require 'hy-lang-scss)                 ;; SCSS syntax Sass
(require 'hy-lang-less)                 ;; Less

(require 'hy-lang-yaml)                 ;; yaml
(require 'hy-lang-json)                 ;; JSON

;;; Applicaiton Framework

(require 'hy-frwk-rails)                ;; For rails framework

;;; Operating System

(require 'hy-proc-shell)                ;; bash

;;; GTD

(require 'eide-org)                   ;; Org

;;; Life and health

(require 'hy-life-health)               ;; Health

;;; Fixed up

(require 'hy-fix-playsound)             ;; Fix the playsound feature of emacs

;;; Load custom file
;;; This file is generated by emacs automatically

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
