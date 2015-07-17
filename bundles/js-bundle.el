;; javascript
(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js2-mode))
(setq js2-basic-offset 2)
(push '("function" . ?ƒ) prettify-symbols-alist)
(require 'js2-mode)
(require 'jst)
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


;; coffeeScript

(defun ky/coffee-new-line-at-end-and-indent ()
  "Move to the end of line and indent like coffee."
  (interactive)
  (move-end-of-line 1)
  (coffee-newline-and-indent))

(require 'coffee-mode)
(define-key coffee-mode-map [s-return] 'ky/coffee-new-line-at-end-and-indent)

(add-hook 'coffee-mode-hook
          (lambda ()
            ;; use yard mode for highlight documentation
            (yard-mode)
            ;; Clean whitespace
            (add-hook 'before-save-hook 'whitespace-cleanup)
            ;; use snippets
            (yas-minor-mode-on)
            ;; syntax checking
            (setq flycheck-checker 'coffee)
            (flycheck-mode)
            ;; JST mode
            (if (fboundp 'jst-enable-appropriate-mode)
                (jst-enable-appropriate-mode))
            ))

(provide 'js-bundle)
