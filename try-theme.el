(deftheme try-theme "DOCSTRING for try")
(custom-theme-set-faces 'try-theme
                        '(default ((t (:foreground "#000000" :background "#ffffff" ))))
                        '(cursor ((t (:background "#174908" ))))
                        '(fringe ((t (:background "#ffffff" ))))
                        '(mode-line ((t (:foreground "#291e1e" :background "#c5eea9" ))))
                        '(region ((t (:background "#fffce3" ))))
                        '(secondary-selection ((t (:background "#f2e1d6" ))))
                        '(font-lock-builtin-face ((t (:foreground "#4129d2" ))))
                        '(font-lock-comment-face ((t (:foreground "#186506" ))))
                        '(font-lock-function-name-face ((t (:foreground "#28d0ff" ))))
                        '(font-lock-keyword-face ((t (:foreground "#cc34fb" ))))
                        '(font-lock-string-face ((t (:foreground "#fb4545" ))))
                        '(font-lock-type-face ((t (:foreground "#ff1354" ))))
                        '(font-lock-constant-face ((t (:foreground "#ec8b3f" ))))
                        '(font-lock-variable-name-face ((t (:foreground "#6914ce" ))))
                        '(minibuffer-prompt ((t (:foreground "#000000" :bold t ))))
                        '(font-lock-warning-face ((t (:foreground "red" :bold t ))))
                        )

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'try-theme)
(provide 'try-theme)
