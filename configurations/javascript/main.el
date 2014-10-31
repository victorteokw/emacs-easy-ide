(require-package 'js2-mode)

(require-package 'json-mode)

(require-package 'ac-js2)

(require-package 'coffee-mode)

;; Javascript nests {} and () a lot, so I find this helpful

(require-package 'rainbow-delimiters)
(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))

;; Node.js
(require-package 'nodejs-repl)

;; Jasmine
(setq-default js2-global-externs
              '("angular" "inject" "describe" "expect" "it" "beforeEach"
                "afterEach" "$" "_" "JSON" "jasmine" "spyOn" "module" "breeze"
                "moment"))
