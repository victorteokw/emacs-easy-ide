;; Use cask and pallet
(require 'cask (expand-file-name "$HOME/.cask/cask.el"))
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(add-to-list 'load-path (expand-file-name "bundles" user-emacs-directory))

(require 'ui-bundle)                    ;; User interface configuration
(require 'editor-bundle)                ;; Editor configuration
(require 'cvs-bundle)                   ;; git configuration
(require 'prog-bundle)                  ;; setup for all programming languages
(require 'text-bundle)                  ;; setup for all non programming langs

(require 'elisp-bundle)                 ;; Elisp
(require 'ruby-bundle)                   ;; Ruby
(require 'html-bundle)                  ;; HTML
(require 'js-bundle)                    ;; JavaScript and CoffeeScript
(require 'php-bundle)                   ;; PHP
(require 'c-bundle)                     ;; C, C++, Objective-C

(require 'shell-bundle)                 ;; bash

(require 'health-bundle)                ;; Health
(require 'org-bundle)                   ;; Org

;; custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
