;; Basic ruby setup
(require-package 'ruby-mode)

(require-package 'ruby-hash-syntax)

(auto-major-mode 'ruby-mode
                 "\\.rxml\\'" "Kirkfile\\'"
                 "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "Beanfile\\'")

(setq ruby-use-encoding-map nil)

;; Ruby compilation
(require-package 'ruby-compilation)

(eval-after-load "ruby-mode"
  '(lambda ()
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)
     (add-hook 'ruby-mode-hook
               (lambda ()
                 (unless (derived-mode-p 'prog-mode)
                   (run-hooks 'prog-mode-hook))))))

(add-hook 'ruby-mode-hook (lambda ()
                            (yas-minor-mode-on)))

;; Flycheck
;;(require-package 'flycheck)
;;(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-hook 'ruby-mode-hook
          (lambda ()
            (require 'diminish)
(diminish 'robe-mode)
(diminish 'auto-complete-mode)
(diminish 'whitespace-mode)
(diminish 'yas-minor-mode)
;;(diminish 'flycheck-mode)
(diminish 'eldoc-mode) ;; Fix this fucking thing.
))

(provide 'init-ruby-mode)

(defun setup-ruby-auto-complete ()
  "Set up ruby development auto complete."
  (add-to-list 'ac-modes 'ruby-mode)
  (ac-config-default)
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (add-to-list 'ac-sources 'ac-source-dictionary)
  (auto-complete-mode t)
  (setq-default ac-auto-start t)
  (setq-default ac-dwim t))

(add-hook 'ruby-mode-hook 'setup-ruby-auto-complete)

(require-package 'bundler)

;; Fix bundle-bug
(defun bundle-gem-location (gem-name)
  "Returns the location of the given gem, or 'no-gemfile if the
Gemfile could not be found, or nil if the Gem could not be
found."
  (let ((bundler-stdout
         (shell-command-to-string
          (format "bundle show %s" (shell-quote-argument gem-name)))))
    (cond
     ((string-match "Could not locate Gemfile" bundler-stdout)
      'no-gemfile)
     ((string-match "Could not find " bundler-stdout)
      nil)
     (t
      (concat (replace-regexp-in-string
               "Resolving dependencies...\\|\n" ""
               bundler-stdout)
              "/")))))

(auto-major-mode 'ruby-mode
                 "\\Podfile\\'")
(require-package 'feature-mode)
;;; Robe
(require-package 'robe)
(after-load 'ruby-mode
  (add-hook 'ruby-mode-hook 'robe-mode))
(after-load 'robe
  (add-hook 'robe-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-robe))))

;; ri
(require-package 'yari)
(defalias 'ri 'yari)
(eval-after-load 'ruby-mode '(define-key ruby-mode-map (kbd "C-c C-y") 'yari-helm))



;; ruby editing features

;; See http://hbin.me/blog/2012/08/24/emacs-user-tips-for-rails/
(defun insert-arrow ()
  (interactive)
  (delete-horizontal-space t)
  (insert " => "))
;;(define-key ruby-mode-map (kbd "C-c .") 'insert-arrow)

(add-hook 'ruby-mode-hook
          '(lambda ()
             (modify-syntax-entry ?$ "w")
             (modify-syntax-entry ?@ "w")
             (modify-syntax-entry ?: ".")))


;; Ruby tools minor mode
;; Which is a collection of handy functions

(defun ruby-tools-looking-around (back at)
  "Check if looking backwards at BACK and forward at AT."
  (and (looking-at-p at) (looking-back back)))

(defun ruby-tools-symbol-at-point-p ()
  "Check if cursor is at a symbol or not."
  (ruby-tools-looking-around ":[A-Za-z0-9_]*" "[A-Za-z0-9_]*"))

(defun ruby-tools-string-at-point-p ()
  "Check if cursor is at a string or not."
  (ruby-tools-string-region))

(defun ruby-tools-symbol-region ()
  "Return region for symbol at point."
  (list
   (save-excursion
     (search-backward ":" (line-beginning-position) t))
   (save-excursion
     (if (re-search-forward "[^A-Za-z0-9_]" (line-end-position) t)
         (1- (point))
       (line-end-position)))))

(defun ruby-tools-string-region ()
  "Return region for string at point."
  (let ((orig-point (point))
        (regex "'\\(\\(\\\\'\\)\\|[^']\\)*'\\|\"\\(\\(\\\\\"\\)\\|[^\"]\\)*\"")
        beg end)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (and (re-search-forward regex (line-end-position) t)
                  (not (and beg end)))
        (let ((match-beg (match-beginning 0)) (match-end (match-end 0)))
          (when (and
                 (> orig-point match-beg)
                 (< orig-point match-end))
            (setq beg match-beg)
            (setq end match-end))))
      (and beg end (list beg end)))))

(defun ruby-tools-interpolate ()
  "Interpolate with #{} in some places."
  (interactive)
  (if (and mark-active (equal (point) (region-end)))
      (exchange-point-and-mark))
  (insert ?#)
  (when (or
         (ruby-tools-looking-around "\"[^\"\n]*" "[^\"\n]*\"")
         (ruby-tools-looking-around "`[^`\n]*" "[^`\n]*`")
         (ruby-tools-looking-around "%([^(\n]*" "[^)\n]*)"))
    (cond (mark-active
           (goto-char (region-beginning))
           (insert ?\{)
           (goto-char (region-end))
           (insert ?\}))
          (t
           (insert "{}")
           (forward-char -1)))))

(defun ruby-tools-to-symbol ()
  "Turn string at point to symbol."
  (interactive)
  (if (ruby-tools-string-at-point-p)
      (let* ((region (ruby-tools-string-region))
             (min (nth 0 region))
             (max (nth 1 region))
             (content (buffer-substring-no-properties (1+ min) (1- max))))
        (when (string-match-p "^\\([a-zA-Z_][a-zA-Z0-9_]*\\)?$" content)
          (let ((orig-point (point)))
            (delete-region min max)
            (insert (concat ":" content))
            (goto-char orig-point))))))

(defun ruby-tools-to-single-quote-string ()
  (interactive)
  (ruby-tools-to-string "'"))

(defun ruby-tools-to-double-quote-string ()
  (interactive)
  (ruby-tools-to-string "\""))

(defun ruby-tools-to-string (string-quote)
  "Convert symbol or string at point to string."
  (let* ((at-string (ruby-tools-string-at-point-p))
         (at-symbol (and (not at-string) (ruby-tools-symbol-at-point-p))))
    (when (or at-string at-symbol)
      (let* ((region (or
                      (and at-symbol (ruby-tools-symbol-region))
                      (and at-string (ruby-tools-string-region))))
             (min (nth 0 region))
             (max (nth 1 region))
             (content
              (buffer-substring-no-properties (1+ min)
                                              (if at-symbol max (1- max)))))
        (setq content
              (if (equal string-quote "'")
                  (replace-regexp-in-string
                   "\\\\\"" "\""
                   (replace-regexp-in-string "\\([^\\\\]\\)'" "\\1\\\\'"
                                             content))
                (replace-regexp-in-string
                 "\\\\\'" "'"
                 (replace-regexp-in-string "\\([^\\\\]\\)\"" "\\1\\\\\""
                                           content))))
        (let ((orig-point (point)))
          (delete-region min max)
          (insert
           (format "%s%s%s" string-quote content string-quote))
          (goto-char orig-point))))))

(defun ruby-tools-clear-string ()
  "Clear string at point."
  (interactive)
  ;; Will this go into kill ring?
  )

(defun ruby-tools-to-regexp ()
  "Turn a string or a symbol into Regexp."
  (interactive)
  (ruby-tools-to-string "/")
  )
(defvar ruby-tools-mode-keymap-prefix (kbd "C-c")
  "`ruby-tools-mode' keymap prefix.")

(defvar ruby-tools-mode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ":") 'ruby-tools-to-symbol)
    (define-key map (kbd "\"") 'ruby-tools-to-double-quote-string)
    (define-key map (kbd "'") 'ruby-tools-to-single-quote-string)
    (define-key map (kbd "#") 'ruby-tools-interpolate)
    (define-key map (kbd ";") 'ruby-tools-clear-string)
    (define-key map (kbd "/") 'ruby-tools-to-regexp)
    map
    )
  "Key map after `ruby-tools-mode-keymap-prefix'.")

(defvar ruby-tools-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ruby-tools-mode-keymap-prefix ruby-tools-mode-command-map)
    map)
  "Key map for ruby tools mode.")

(define-minor-mode ruby-tools-mode
  "Collection of handy functions for ruby-mode."
  :init-value nil
  :lighter nil
  :keymap ruby-tools-mode-map)

(add-hook 'ruby-mode-hook 'ruby-tools-mode)
;; Ruby tools mode ends here.


(add-hook 'ruby-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup)
             (setq-local whitespace-style '(empty indentation::space
                                                  space-befure-tab::space
                                                  trailing
                                                  whitespace-style::space))
             ;; Whitespace showing and cleaning

             ;; Do not deep indent
             (setq ruby-deep-indent-paren nil)

             ))
(auto-major-mode 'ruby-mode
                 "Gemfile\\'" "\\.gemspec\\'")
(auto-major-mode 'ruby-mode
                 "\\Guardfile\\'")
(require-package 'ruby-guard)
;; Inferior ruby
(require-package 'inf-ruby)
(require-package 'ac-inf-ruby)
(after-load 'auto-complete
  (add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)
(after-load 'inf-ruby
  (define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete))
(auto-major-mode 'ruby-mode
                 "\\.jbuilder\\'")
(require-package 'minitest)
(setq minitest-keymap-prefix (kbd "C-c t"))
(eval-after-load 'minitest
  '(minitest-install-snippets))
(require 'minitest)
(auto-major-mode 'ruby-mode
                 "\\.ru\\'")
(require-package 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(require 'ruby-mode)

;; Use C-c f for projectile-rails
(setq projectile-rails-keymap-prefix (kbd "C-c f"))

(require-package 'rails-new)

;; This is just a tutorial
;; Cannot be really used.
(defun select-inside-quotes ()
  ;;  (interactive)
  (let (p1 p2)
    (skip-chars-backward "^\"")
    (setq p1 (point))
    (skip-chars-forward "^\"")
    (setq p2 (point))

    (goto-char p1)
    (push-mark p2)
    (setq mark-active t)))


;;; Rails guide minor mode

(defconst rails-guide-versions-list
  (list "default" "4.2.0" "4.1.8" "4.0.12" "3.2.21" "2.3.11")
  "Rails guide versions.")

(defconst rails-guide-pages-alist
  '(
    ("index" . "Index Page")
    ("getting_started" . "Start Here: Getting Started with Rails")
    ("migrations" . "Models: Rails Database Migrations")
    ("active_record_basics" . "Models: Active Record Basics")
    ("active_record_validations" . "Models: Active Record Validations")
    ("active_record_validations_callbacks" . "Models: Active Record V\
alidations and Callbacks")
    ("association_basics" . "Models: Active Record Associations")
    ("active_record_querying" . "Models: Active Record Query Interface")
    ("active_model_basics" . "Models: Active Model Basics")
    ("action_view_overview" . "Views: Action View Overview")
    ("layouts_and_rendering" . "Views: Layouts and Rendering in Rails")
    ("form_helpers" . "Views: Form Helpers")
    ("action_controller_overview" . "Controllers: Action Controller Ov\
erview")
    ("routing" . "Controllers: Rails Routing from the Outside In")
    ("active_support_core_extensions" . "Digging Deeper: Active Support\
 Core Extensions")
    ("i18n" . "Digging Deeper: Rails Internationalization (I18n) API")
    ("action_mailer_basics" . "Digging Deeper: Action Mailer Basics")
    ("active_job_basics" . "Digging Deeper: Active Job Basics")
    ("testing" . "Digging Deeper: Testing Rails Applications")
    ("security" . "Digging Deeper: Ruby on Rails Security Guide")
    ("debugging_rails_applications" . "Digging Deeper: Debugging Rails\
 Applications")
    ("configuring" . "Digging Deeper: Configuring Rails Applications")
    ("command_line" . "Digging Deeper: The Rails Command Line and Rake\
 Tasks")
    ("asset_pipeline" . "Digging Deeper: The Asset Pipeline")
    ("working_with_javascript_in_rails" . "Digging Deeper: Working with\
 JavaScript in Rails")
    ("engines" . "Digging Deeper: Getting Started with Engines")
    ("initialization" . "Digging Deeper: The Rails Initialization Process")
    ("constant_autoloading_and_reloading" . "Digging Deeper: Constant \
Autoloading and Reloading")
    ("plugins" . "Extending Rails: The Basics of Creating Rails Plugins")
    ("rails_on_rack" . "Extending Rails: Rails on Rack")
    ("generators" . "Extending Rails: Creating and Customizing Rails G\
enerators & Templates")
    ("contributing_to_ruby_on_rails" . "Contributing: Contributing to R\
uby on Rails")
    ("api_documentation_guidelines" . "Contributing: API Documentation \
Guidelines")
    ("ruby_on_rails_guides_guidelines" . "Contributing: Ruby on Rails Gu\
ides Guidelines")
    ("maintenance_policy" . "Maintenance Policy: Maintenance Policy for R\
uby on Rails")
    ("upgrading_ruby_on_rails" . "Upgrade: Upgrading Ruby on Rails")
    ))

(defun rails-guide-visit (version page)
  "Visit rails guide."
  (interactive (list
                (helm-comp-read
                 "Select a rails version: "
                 rails-guide-versions-list
                 :buffer "*Rails Version Select*"
                 :name "Rails Version"
                 :default "default"
                 :must-match t)
                (helm-comp-read
                 "Select a page: "
                 (mapcar 'cdr rails-guide-pages-alist)
                 :buffer "*Rails Guide Select*"
                 :name "*Rails Guide*"
                 :default "Index Page"
                 :must-match t)))

  (let ((url "http://guides.rubyonrails.org"))
    (or (and (string= version "default") (string= page "Index Page") t)
        (and (string= version "default")
             (setq url (concat url "/" (car (rassoc page rails-guide-pages-alist))
                               ".html")) t)
        (and (setq url (concat url "/v" version))
             (or (string= "Index Page" page)
                 (setq url (concat url "/"
                                   (car (rassoc page rails-guide-pages-alist))
                                   ".html")))))
    (browse-url url)))

(defvar rails-guide-mode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c G") 'rails-guide-visit)
    map
    )
  "Key map for`rails-guide-mode'.")

(define-minor-mode rails-guide-mode
  "Convinience way to read rails guide."
  :init-value nil
  :lighter nil
  :keymap rails-guide-mode-command-map
  )

(add-hook 'projectile-rails-mode-hook
          (lambda ()
            (if (bound-and-true-p projectile-rails-mode)
                (progn
                  (rails-guide-mode +1)
                  )
              (rails-guide-mode -1)
              )
            ))

(auto-major-mode 'ruby-mode
                 "Rakefile\\'" "\\.rake\\'")
(require-package 'rspec-mode)
;; load rspec-mode snippets for yasnippet
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(setq rspec-key-command-prefix (kbd "C-c r"))
(require-package 'slim-mode)
(require-package 'haml-mode)
(require 'ac-haml)
(require 'ac-slim)
(defun setup-haml-environment ()
  "Setup html development environment."
  (require-package 'auto-complete)
  (setq ac-sources '(ac-source-haml-attribute))
  (add-to-list 'ac-sources 'ac-source-haml-tag)
  (auto-complete-mode t)
  (setq-default ac-auto-start t)
  (setq-default ac-dwim t))

(defun setup-slim-environment ()
  "Setup html development environment."
  (require-package 'auto-complete)
  (setq ac-sources '(ac-source-slim-attribute))
  (add-to-list 'ac-sources 'ac-source-slim-tag)
  (auto-complete-mode t)
  (setq-default ac-auto-start t)
  (setq-default ac-dwim t))

(add-hook 'haml-mode-hook 'setup-haml-environment)
(add-hook 'slim-mode-hook 'setup-slim-environment)
;; yaml
(require-package 'yaml-mode)

(provide 'init-ruby)
