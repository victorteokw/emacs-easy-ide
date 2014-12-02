(require-package 'slim-mode)
(require-package 'haml-mode)
(require 'ac-haml)
(defun setup-haml-environment ()
  "Setup html development environment."
  (require-package 'auto-complete)
  (setq ac-sources '(ac-source-haml-attribute))
  (add-to-list 'ac-sources 'ac-source-haml-tag)
  (auto-complete-mode t)
  (setq-default ac-auto-start t)
  (setq-default ac-dwim t))

(add-hook 'haml-mode-hook 'setup-haml-environment)
