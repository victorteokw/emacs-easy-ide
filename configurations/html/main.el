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

(provide 'init-html)
