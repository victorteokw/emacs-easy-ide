(require-package 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(require 'ruby-mode)

;; Find view
(define-key ruby-mode-map (kbd "C-c f v") 'projectile-rails-find-view)
(define-key ruby-mode-map (kbd "C-c f V") 'projectile-rails-find-current-view)
(define-key ruby-mode-map (kbd "C-c f h") 'projectile-rails-find-helper)
(define-key ruby-mode-map (kbd "C-c f H")
  'projectile-rails-find-current-helper)
(define-key ruby-mode-map (kbd "C-c f j") 'projectile-rails-find-javascript)
(define-key ruby-mode-map (kbd "C-c f J")
  'projectile-rails-find-current-javascript)
(define-key ruby-mode-map (kbd "C-c f s") 'projectile-rails-find-stylesheet)
(define-key ruby-mode-map (kbd "C-c f S")
  'projectile-rails-find-current-stylesheet)

;; Find controller
(define-key ruby-mode-map (kbd "C-c f c") 'projectile-rails-find-controller)
(define-key ruby-mode-map (kbd "C-c f C")
  'projectile-rails-find-current-controller)

;; Find model
(define-key ruby-mode-map (kbd "C-c f m") 'projectile-rails-find-model)
(define-key ruby-mode-map (kbd "C-c f M") 'projectile-rails-find-current-model)
(define-key ruby-mode-map (kbd "C-c f n") 'projectile-rails-find-migration)
(define-key ruby-mode-map (kbd "C-c f N")
  'projectile-rails-find-current-migration)

;; Find test
(define-key ruby-mode-map (kbd "C-c f p") 'projectile-rails-find-spec)
(define-key ruby-mode-map (kbd "C-c f P") 'projectile-rails-find-current-spec)
(define-key ruby-mode-map (kbd "C-c f t") 'projectile-rails-find-test)
(define-key ruby-mode-map (kbd "C-c f T") 'projectile-rails-find-current-test)
(define-key ruby-mode-map (kbd "C-c f f") 'projectile-rails-find-fixture)
(define-key ruby-mode-map (kbd "C-c f F")
  'projectile-rails-find-current-fixture)

(define-key ruby-mode-map (kbd "C-c f r") 'projectile-rails-goto-routes)
(define-key ruby-mode-map (kbd "C-c f d") 'projectile-rails-goto-schema)




