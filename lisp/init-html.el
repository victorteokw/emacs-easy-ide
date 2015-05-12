(require-package 'web-mode)
(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-sql-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . web-mode))
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)

(defun hoiyu--rubify-and-translate ()
  "Translate."
  (interactive)
  (let ((start (1+ (save-excursion
                     (search-backward ">" nil t))))
        (end (1- (save-excursion
                   (search-forward "<" nil t))))
        content)
    (and start end (progn
                     (setq content (read-string "t: "))
                     (delete-region start end)
                     (goto-char start)
                     (insert "<%= t(\"")
                     (insert content)
                     (insert "\") %>")
                     (goto-char (- (point) 5))
                     ))))

(defun hoiyu--translate-ruby-string ()
  "Translate."
  (interactive)
  (let ((start (1+ (save-excursion
                     (re-search-backward "[^\\\\]'"))))
        (end (save-excursion
               (re-search-forward "[^\\]'")))
        content)
    (setq content (buffer-substring-no-properties start end))
    (setq content (downcase content))
    (setq content (replace-regexp-in-string " " "_" content))
    (setq content (replace-regexp-in-string "^'" "" content))
    (setq content (replace-regexp-in-string "'$" "" content))
    (setq content (read-string "t: " content nil content))
    (delete-region start end)
    (insert "t(\"")
    (insert content)
    (insert "\")")
    ))

(require-package 'elnode)
(require-package 'restclient)
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

(setq scss-compile-at-save nil) ;; DO NOT AUTO COMPLILE
(setq css-indent-offset 2)
(require-package 'scss-mode)
(require-package 'sass-mode)



(provide 'init-html)


