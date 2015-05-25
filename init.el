;; Initialize cask and pallet
(require 'cask (expand-file-name "$HOME/.cask/cask.el"))
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; Using meta-backtip to open menu bar on terminal
(define-key global-map (kbd "M-`") 'menu-bar-open)

(setq-default
 ;; inhibit-splash-screen t ;; disable sp;ash screen
 visibel-bell t ;; do not beep
 indent-tabs-mode nil ;; use whitespace to indent
 tab-width 2
 initial-scratch-message ";; Life and health, family and oxygen."
 make-backup-files nil ;; do not backup files
 ;; initial-major-mode 'ruby-mode
 )

;; exec path
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

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

;; github
(require 'yagist)
(require 'github-browse-file)
(require 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)
(require 'github-clone)

;; ido interface
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-file-extensions-order '(".rb" ".js" ".erb" ".md" ".html"))
(setq ido-ignore-files '(".DS_Store"))
(setq ido-use-filename-at-point 'guess)
(ido-mode t)

;; smex
(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(global-set-key [remap execute-extended-command] 'smex)

;; helm
(require 'helm-config)
(require 'helm-projectile)

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

;; themes
(require 'base16-eighties-dark-theme)

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
(require 'hl-sexp)
(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
(set-variable 'hl-sexp-background-color "#efefef")

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

;; completion
;; use hippie-expand

;; yasnippet
(add-hook 'ruby-mode-hook 'yas-minor-mode-on)

;; syntax
(add-hook 'ruby-mode-hook
          '(lambda ()
             (modify-syntax-entry ?$ "w")
             (modify-syntax-entry ?@ "w")
             (modify-syntax-entry ?: ".")))

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
(add-hook 'web-mode-hook
	  'auto-complete-mode)

(require 'slim-mode)
(require 'haml-mode)

;; elnode is a super engine
(require 'elnode)

;; restclient
(require 'restclient)

;; javascript
(require 'js2-mode)

;; php
(require 'php-mode)


