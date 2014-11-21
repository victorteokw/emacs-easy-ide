(require-package 'projectile)
(projectile-global-mode)

(require-package 'helm)
(require 'helm-config)

(require-package 'helm-projectile)

;;(require-package 'grizzl)
;;(setq projectile-completion-system 'grizzl)

(require-package 'diminish)
(diminish 'projectile-mode)

(provide 'project-additional)
