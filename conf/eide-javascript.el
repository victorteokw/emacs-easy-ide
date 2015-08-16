;; javascript

(require 'js2-mode)

;; Use js2-mode for .es6 file, .js.erb file
(add-to-list 'auto-mode-alist
             '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js2-mode))

;; two space indentation
(setq js2-basic-offset 2)

(add-hook 'js2-mode-hook
          (lambda ()
            (js2-imenu-extras-mode)
            ;; Clean whitespace
            (add-hook 'before-save-hook 'whitespace-cleanup)
            ;; JST mode
            (if (fboundp 'jst-enable-appropriate-mode)
                (jst-enable-appropriate-mode))
            ))

;; Jasmine
(setq-default js2-global-externs
              '("angular" "inject" "describe" "expect" "it" "beforeEach"
                "afterEach" "$" "_" "JSON" "jasmine" "spyOn" "module" "breeze"
                "moment"))

;; rainbow delimiters mode
(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))

;; Auto insert semicolon

;; Auto insert /*global External vars */
(defun eide-js2-undeclared-vars-list ()
  "Not documented yet."
  (let ((name nil) (undeclared-vars-list nil))
    (dolist (entry js2-recorded-identifiers)
      (cl-destructuring-bind (name-node scope pos end) entry
        (setq name (js2-name-node-name name-node))
        (unless (or (member name js2-global-externs)
                    (member name js2-default-externs)
                    (member name js2-additional-externs)
                    (js2-get-defining-scope scope name pos))
          (push name undeclared-vars-list))))
    (-uniq undeclared-vars-list)))

(defun eide-js2-auto-insert-global-line ()
  "Not documented yet."
  (interactive)
  (let ((global-string
         (format "%s%s%s%s"
                 "/*global "
                 (string-join (eide-js2-undeclared-vars-list) ", ")
                 " */" "\n")))
    (save-restriction
      (widen)
      (goto-char 0)
      (insert global-string))))

(defun eide-js2-auto-colon ()
  "Not documented yet."
  (interactive)
  (insert ":")
  (delete-horizontal-space)
  (insert " "))

(define-key js2-mode-map (kbd ":") 'eide-js2-auto-colon)

(defun eide-js2-insert-colon-and-return ()
  "Not documented yet."
  (interactive)
  (move-end-of-line 1)
  (insert ";")
  (newline-and-indent))

(define-key js2-mode-map [M-return] 'eide-js2-insert-colon-and-return)

(defun eide-js2-insert-dot-dwim ()
  "Not documented yet."
  (interactive)
  (move-end-of-line 1)
  (insert "."))

(define-key js2-mode-map (kbd ".") 'eide-js2-insert-dot-dwim)

(defun eide-js2-insert-colon-dwim ()
  "Not documented yet."
  (interactive)
  (move-end-of-line 1)
  (insert ":"))

(define-key js2-mode-map (kbd ":") 'eide-js2-insert-colon-dwim)

(defun eide-js2-insert-comma-dwim ()
  "Not documented yet."
  (interactive)
  (move-end-of-line 1)
  (insert ","))

(define-key js2-mode-map (kbd ",") 'eide-js2-insert-comma-dwim)

(defun eide-js2-insert-semicolon-dwim ()
  "Not documented yet."
  (interactive)
  (move-end-of-line 1)
  (insert ";"))

(define-key js2-mode-map (kbd ";") 'eide-js2-insert-semicolon-dwim)


;; Prettify function keyword and more

(defun eide-js2-prettify-symbols-setup ()
  "Not documented yet."
  ;; use ƒ for function keyword
  (push '("function" . ?ƒ) prettify-symbols-alist)
  (prettify-symbols-mode))

(add-hook 'js2-mode-hook 'eide-js2-prettify-symbols-setup)

;; Code folding
(add-hook 'js2-mode-hook 'hs-minor-mode)

(provide 'eide-javascript)
