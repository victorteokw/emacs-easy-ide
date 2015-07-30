;; git client
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key [(meta f12)] 'magit-status)
(require 'git-timemachine)
(require 'gitignore-mode)
(require 'gitconfig-mode)
(require 'git-messenger)
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


(provide 'hy-core-cvs)
