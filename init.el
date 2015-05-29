;; Initialize cask and pallet
(require 'cask (expand-file-name "$HOME/.cask/cask.el"))
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; Using meta-backtip to open menu bar on terminal
(define-key global-map (kbd "M-`") 'menu-bar-open)

(setq-default
 ;; inhibit-splash-screen t ;; disable splash screen
 visibel-bell t ;; do not beep
 indent-tabs-mode nil ;; use whitespace to indent
 tab-width 2
 initial-scratch-message ";; Life and health, family and oxygen.\n\n"
 make-backup-files nil ;; do not backup files
 ;; initial-major-mode 'ruby-mode
 fill-column 80
 linum-format "%2d "
 )

;; Nice fringe
(set-fringe-mode '(10 . 10))
(add-to-list 'default-frame-alist '(width . 83))

;; exec path
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; default font
(set-face-attribute 'default nil :height 140)

;; pretty symbol
(global-prettify-symbols-mode)

;; project
(require 'projectile)
(projectile-global-mode)

;; git client
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key [(meta f12)] 'magit-status)
(require 'git-timemachine)
(require 'gitignore-mode)
(require 'gitconfig-mode)
(require 'git-messenger)
(require 'git-commit-mode)
(require 'git-rebase-mode)
(require 'git-blame)
(require 'diff-hl)
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

;; github
(require 'yagist)
(require 'github-browse-file)
(require 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)
(require 'github-clone)

;; ido interface
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-file-extensions-order '(".rb" ".coffee" ".js" ".erb" ".md" ".html"))
(setq ido-ignore-files '(".DS_Store"))
(setq ido-use-filename-at-point 'guess)
(ido-mode t)
;; (require 'ido-vertical-mode)
;; (ido-vertical-mode 1)
;; Make functions such as `C-h f' using ido
(require 'ido-ubiquitous)
(ido-ubiquitous-mode)

;; smex
(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(global-set-key [remap execute-extended-command] 'smex)

;; guide key
(require 'guide-key)
(setq guide-key/guide-key-sequence
      '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r"))
(guide-key-mode 1)
(diminish 'guide-key-mode)

;; discover
(require 'discover)
(global-discover-mode)

;; global auto revert
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; helm
(require 'helm-config)
(require 'helm-projectile)
(helm-autoresize-mode)
(global-set-key (kbd "s-i") 'helm-mini)
(global-set-key (kbd "s-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-imenu-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(require 'helm-swoop)
(global-set-key (kbd "C-z") 'helm-swoop)

(require 'sublimity)
(require 'sublimity-map)

;; undo
(require 'undo-tree)
(global-undo-tree-mode)

;; searching
(require 'anzu)
(global-anzu-mode t)
(global-set-key (kbd "M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-M-%") 'anzu-query-replace)

;; jumping
(require 'ace-jump-mode)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(global-unset-key (kbd "C-;"))
(global-unset-key (kbd "M-;"))
(define-key global-map (kbd "C-;") 'ace-jump-char-mode)
(define-key global-map (kbd "C-:") 'ace-jump-word-mode)
(define-key global-map (kbd "C-M-;") 'ace-jump-line-mode)

;; dash
(global-set-key (kbd "C-h D") 'dash-at-point)

;; themes
(require 'base16-ocean-dark-theme)

;; global line number
(global-linum-mode)

;; global hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; expand region
(defalias 'save-mark-and-excursion 'save-excursion)
(defmacro save-mark-and-excursion (&rest body)
  `(save-excursion ,@body)) ;; fixed a bug
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Mark
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

;; kill back to indentation
(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "s-M-<backspace>") 'kill-back-to-indentation)

;; move line up and down
(require 'move-dup)
(global-set-key [s-down] 'md/move-lines-down)
(global-set-key [s-up] 'md/move-lines-up)
(global-set-key [M-s-down] 'md/duplicate-down)
(global-set-key [M-s-up] 'md/duplicate-up)

;; whole line or region
(require 'whole-line-or-region)
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
(make-variable-buffer-local 'whole-line-or-region-mode)

;; highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)

;; ignore and next line
(defun ignore-this-line-and-move-to-next-line ()
  "Ignore this line and move to next line."
  (interactive)
  (next-line)
  (move-end-of-line 1))
(global-set-key [M-s-return] 'ignore-this-line-and-move-to-next-line)

(defun ignore-this-line-and-open-new-line ()
  "Ignore this line and open new line below."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key [s-return] 'ignore-this-line-and-open-new-line)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c c c") 'mc/edit-lines)
(global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)

;; All programming language setup

(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; All non programming language setup
(add-hook 'text-mode-hook 'goto-address-mode)

;; Emacs lisp

;; emacs loading
(setq load-prefer-newer t)

;; jump
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; litable
          ; (require 'litable)
          ; (add-hook 'emacs-lisp-mode-hook 'litable-mode)

;; highlight sexp
;; (require 'hl-sexp)
;; (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)

;; rainbow parens
(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; editing
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; indentation
(require 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

;; short documentation
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; check parens when saving
(add-hook 'emacs-lisp-mode-hook
    (lambda ()
      (add-hook 'after-save-hook #'check-parens nil t)
      ))

;; open Cask file in elisp-mode
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))

;; expand macro
(require 'macrostep)

;; snippets
(require 'yasnippet)
(setq yas-snippet-dirs (remove 'yas-installed-snippets-dir yas-snippet-dirs))
(yas-reload-all)

;; auto complete
(require 'auto-complete)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq ac-ignore-case nil)
            (setq ac-sources '(ac-source-dictionary
                               ac-source-features
                               ac-source-functions
                               ac-source-symbols
                               ac-source-variables
                               ac-source-words-in-same-mode-buffers
                               ))
            (auto-complete-mode t)
            ))

;; bash

;; (add-to-list 'ac-modes 'shell-mode)
;; (require 'readline-complete)
;; (add-hook 'shell-mode-hook 'ac-rlc-setup-sources)

;; Ruby

(require 'ruby-mode)

;; ruby file extensions
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Beanfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.irbrc\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.pryrc\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Podfile\\'" . ruby-mode))

;; compilation
(require 'ruby-compilation)

;; inferior ruby
(require 'inf-ruby)
(require 'ac-inf-ruby)
(add-hook 'inf-ruby-mode-hook (lambda ()
        (ac-inf-ruby-enable)
        (auto-complete-mode)))

;; ri documentation
(require 'yari)
(global-set-key (kbd "C-h y") 'yari-helm)

;; yaml
(require 'yaml-mode)

;; rails
(require 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(require 'rails-new)

;; mini test
(require 'minitest)
(setq minitest-keymap-prefix (kbd "C-c t"))
(eval-after-load 'minitest
  '(minitest-install-snippets))

;; rspec
(require 'rspec-mode)
(setq rspec-key-command-prefix (kbd "C-c t"))
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

;; yasnippet
(add-hook 'ruby-mode-hook 'yas-minor-mode-on)

;; syntax
(add-hook 'ruby-mode-hook
          '(lambda ()
             (modify-syntax-entry ?$ "w")
             (modify-syntax-entry ?@ "w")
             (modify-syntax-entry ?: ".")

             ;; indent guide
             (indent-guide-mode)
             ;; (setq indent-guide-recursive t)

             ))

;; support yard syntax
(require 'yard-mode)
(add-hook 'ruby-mode-hook 'yard-mode)

;; whitespace cleaning
(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq-local whitespace-style '(empty indentation::space
                                                  space-befure-tab::space
                                                  trailing
                                                  whitespace-style::space))
       (add-hook 'before-save-hook 'whitespace-cleanup)))

;; do not deep indent
(setq ruby-deep-indent-paren nil)

;; guard
;; (require 'ruby-guard)

;; web
(require 'web-mode)
(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-sql-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . web-mode))
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)

;; completion
(require 'ac-html)
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-words-in-buffer ac-source-css-property))
  ("html" . (ac-source-html-tag
       ac-source-html-attribute
       ac-source-html-attribute-value
       ))
  ("php" . (ac-source-words-in-buffer
      ac-source-words-in-same-mode-buffers
      ac-source-dictionary))))
(setq web-mode-enable-auto-quoting nil)
(add-hook 'web-mode-hook 'auto-complete-mode)

(add-hook 'slim-mode-hook
          (lambda ()
            (require 'ac-slim)
            (setq ac-sources '(ac-source-slim-attribute))
            (add-to-list 'ac-sources 'ac-source-slim-tag)
            (add-to-list 'ac-sources 'ac-source-slim-attribute-value)
            (auto-complete-mode)
            ))

(add-hook 'haml-mode-hook
          (lambda ()
            (require 'ac-haml)
            (setq ac-sources '(ac-source-haml-attribute))
            (add-to-list 'ac-sources 'ac-source-haml-tag)
            (add-to-list 'ac-sources 'ac-source-haml-attribute-value)
            (auto-complete-mode)
            ))

(setq css-indent-offset 2)
(setq scss-compile-at-save nil)


;; elnode is a super engine
(require 'elnode)

;; restclient
(require 'restclient)

;; javascript
(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js2-mode))
(setq js2-basic-offset 2)
(push '("function" . ?ƒ) prettify-symbols-alist)
(require 'js2-mode)
(add-hook 'js2-mode-hook
          (lambda ()
            (js2-imenu-extras-mode)
            ;; Clean whitespace
            (add-hook 'before-save-hook 'whitespace-cleanup)

            ))

;; Jasmine
(setq-default js2-global-externs
              '("angular" "inject" "describe" "expect" "it" "beforeEach"
                "afterEach" "$" "_" "JSON" "jasmine" "spyOn" "module" "breeze"
                "moment"))

;; rainbow delimiters mode
(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))

;; coffeeScript

(defun ky/coffee-new-line-at-end-and-indent ()
  "Move to the end of line and indent like coffee."
  (interactive)
  (move-end-of-line 1)
  (coffee-newline-and-indent))

(require 'coffee-mode)
(define-key coffee-mode-map [s-return] 'ky/coffee-new-line-at-end-and-indent)

(add-hook 'coffee-mode-hook
          (lambda ()
            ;; use yard mode for highlight documentation
            (yard-mode)
            ;; Clean whitespace
            (add-hook 'before-save-hook 'whitespace-cleanup)
            ;; use snippets
            (yas-minor-mode-on)
            ;; syntax checking
            (setq flycheck-checker 'coffee)
            (flycheck-mode)
            ))

;; php
(require 'php-mode)

;; c
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Auto complete main
            (require 'auto-complete-clang-async)
            (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
            (setq ac-sources '(ac-source-clang-async))
            (ac-clang-launch-completion-process)

            ;; Auto complete c headers
            (require 'auto-complete-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (setq achead:include-directories
                  (list "/usr/include" "/usr/local/include"
                        "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/6.1.0/include"
                        "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"))
            (auto-complete-mode)

            ;; Snippets
            (yas-minor-mode-on)

            ;; Syntax checking
            (flycheck-mode)
            ))

;; packages
(global-set-key [f9] 'package-install)
(global-set-key [M-f9] 'package-list-packages)

;; custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-sexp-face ((t (:background "SeaGreen4")))))

;; Health

(require 'health (expand-file-name "health.el" user-emacs-directory))
(require 'play-sound (expand-file-name "playsound.el" user-emacs-directory))
;; mode line format
(setq default-mode-line-format
      (list "%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification mode-line-position mode-line-end-spaces
            '(:eval (health-time-mode-line))))

(setq health-session-length 10)
(setq health-time-over-sound (expand-file-name "sounds/short.wav" user-emacs-directory))
;; Start health counting
(health-go-to-work)
