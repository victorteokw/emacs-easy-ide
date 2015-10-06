;;; Use projectile-rails as basic rails minor mode

(require 'projectile-rails)

(add-hook 'projectile-mode-hook 'projectile-rails-on)

;;; create new rails app

(require 'rails-new)

;;; mini test

(require 'minitest)
(setq minitest-keymap-prefix (kbd "C-c t"))
(eval-after-load 'minitest
  '(minitest-install-snippets))

;;; rspec

(require 'rspec-mode)
(setq rspec-key-command-prefix (kbd "C-c t"))
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

;;; Open with ruby mine

(defun eide-rails-open-with-rubymine ()
  "Open current project with rubymine."
  (interactive)
  (call-process-shell-command "open" nil nil nil (projectile-project-root)
                              "-b" "com.jetbrains.rubymine"))

;;; Don't use s-r

(setq-default projectile-rails-discover-bind "")

;;; s-r to run server

(define-key projectile-rails-mode-map (kbd "s-r") 'projectile-rails-server)

;;; C-c C-z to open rails console

(define-key projectile-rails-mode-map (kbd "C-c C-z") 'projectile-rails-console)

(provide 'eide-rails)
