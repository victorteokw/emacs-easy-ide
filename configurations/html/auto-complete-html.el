(require-package 'ac-html)
(require 'ac-html)
(defun setup-html-environment ()
  "Setup html development environment."
  (require-package 'auto-complete)
  (setq ac-sources '(ac-source-html-attribute))
  (add-to-list 'ac-sources 'ac-source-html-tag)
  (add-to-list 'ac-sources 'ac-source-html-attribute-value)
  (auto-complete-mode t)
  (setq-default ac-auto-start t)
  (setq-default ac-dwim t)
  )

(defun setup-html-environment-for-web-mode ()
  "Setup html development environment for web mode."
  (add-to-list 'ac-modes 'web-mode)
  (require 'tern-auto-complete)
  (setup-html-environment)
  (ac-config-default)
  (setq ac-sources '(ac-source-html-attribute))
  (add-to-list 'ac-sources 'ac-source-html-tag)
  (add-to-list 'ac-sources 'ac-source-html-attribute-value)

  (auto-complete-mode t)
  (setq-default ac-auto-start t)
  (setq-default ac-dwim t)

  (setq web-mode-ac-sources-alist
	'(("css" . (ac-source-words-in-buffer ac-source-css-property))
	  ("html" . (ac-source-html-tag
                     ac-source-tern-completion
		     ac-source-html-attribute
                     ac-source-html-attribute-value
                     ))
	  ("php" . (ac-source-words-in-buffer
		    ac-source-words-in-same-mode-buffers
		    ac-source-dictionary))))
  (setq web-mode-enable-auto-quoting nil)
  )

(defun setup-css-environment ()
  "Setup css development environment."
  (require-package 'auto-complete)
  ;; TODO: set css compatible ac sources
  )

(add-hook 'html-mode-hook 'setup-html-environment)
(add-hook 'web-mode-hook 'setup-html-environment-for-web-mode)

(provide 'auto-complete-html)
