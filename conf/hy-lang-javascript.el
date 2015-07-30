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

(provide 'hy-lang-javascript)
