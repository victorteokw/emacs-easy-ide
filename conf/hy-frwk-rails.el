;; Use projectile-rails as basic rails minor mode
(require 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; create new rails app
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


(provide 'hy-frwk-rails)
