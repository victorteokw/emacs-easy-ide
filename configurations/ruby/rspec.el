(require-package 'rspec-mode)
;; load rspec-mode snippets for yasnippet
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(setq rspec-key-command-prefix (kbd "C-c r"))
