(require-package 'coffee-mode)
(add-hook 'coffee-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'whitespace-cleanup)))
