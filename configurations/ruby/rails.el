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
