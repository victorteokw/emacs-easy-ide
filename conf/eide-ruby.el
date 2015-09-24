;;; Use ruby mode to open

(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))   ; Rakefile
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))    ; rake
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))    ; Gemfile
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode)) ; gemspec
(add-to-list 'auto-mode-alist '("\\.irbrc\\'" . ruby-mode))   ; irb
(add-to-list 'auto-mode-alist '("\\.pryrc\\'" . ruby-mode))   ; pry
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode)); jbuilder
(add-to-list 'auto-mode-alist '("\\.rabl\\'" . ruby-mode))    ; rabl
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))      ; Rack config
(add-to-list 'auto-mode-alist '("\\Podfile\\'" . ruby-mode))  ; CocoaPods
(add-to-list 'auto-mode-alist '("Beanfile\\'" . ruby-mode))   ; CocoaBean

(require 'ruby-mode)

;;; Compilation and run ruby

(require 'ruby-compilation)

(define-key ruby-mode-map (kbd "s-r") 'ruby-compilation-this-buffer)

;;; REPL

(require 'inf-ruby)
(require 'ac-inf-ruby)

(defun eide-ruby-repl-auto-complete ()
  (ac-inf-ruby-enable)
  (auto-complete-mode))

(add-hook 'inf-ruby-mode-hook 'eide-ruby-repl-auto-complete)

;; C-c C-z to switch to REPL

(define-key inf-ruby-minor-mode-map (kbd "C-c C-z") 'inf-ruby)

;;; Electric pair

(add-hook 'ruby-mode-hook 'electric-pair-mode)

;;; ri Documentation

(define-key ruby-mode-map (kbd "C-h y") 'yari-helm)

;;; Snippets

(add-hook 'ruby-mode-hook 'yas-minor-mode-on)

;;; Syntax

(defun eide-ruby-syntax ()
  (modify-syntax-entry ?$ "w")
  (modify-syntax-entry ?@ "w")
  (modify-syntax-entry ?: "."))

(add-hook 'ruby-mode-hook 'eide-ruby-syntax)

;;; Yard in comment

(add-hook 'ruby-mode-hook 'yard-mode)

;;; do not deep indent

(setq ruby-deep-indent-paren nil)

;;; guard

;; (require 'ruby-guard)

;;; rake

(eval-after-load "rake"
  '(setq rake-cache-file (f-expand "rake.cache" eide-etc-dir)))

;;; code folding

(add-hook 'ruby-mode-hook 'origami-mode)

(provide 'eide-ruby)
