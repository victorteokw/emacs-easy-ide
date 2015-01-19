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

