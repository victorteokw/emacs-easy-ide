(require-package 'minitest)
(setq minitest-keymap-prefix (kbd "C-c t"))
(eval-after-load 'minitest
  '(minitest-install-snippets))
(require 'minitest)
