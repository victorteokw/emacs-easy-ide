;; Health

(require 'health (expand-file-name "local-packages/health.el" user-emacs-directory))
(require 'playsound-bundle)

;; mode line format
(setq default-mode-line-format
      (list "%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification mode-line-position mode-line-end-spaces
            '(:eval (health-time-mode-line))))

(setq health-session-length 10)
(setq health-time-over-sound (expand-file-name "sounds/short.wav" user-emacs-directory))

;; Start health counting
(health-go-to-work)
(put 'set-goal-column 'disabled nil)

(provide 'health-bundle)
