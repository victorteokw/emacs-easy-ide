;;; editing-additional.el

;; electric indent
(electric-indent-mode 1)

;; electric pair
(electric-pair-mode 1)

;; show paren mode
(show-paren-mode 1)

;; use helm to outline current source code
(global-set-key (kbd "M-n") 'back-to-indentation)
(global-set-key (kbd "M-m") 'helm-imenu)

;; show line number
;;(global-linum-mode 1)
;; actually, only few mode especially prog mode needs linum.
;; but inside terminal, if width is 80, should not show linum.
;; It takes space.

;; whitespace mode
(require 'whitespace)

(require-package 'diminish)
(diminish 'whitespace-mode)

;; Column Marker
;; (require-package 'column-marker)
;; (add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 79)))

;; Expand region
(require-package 'expand-region)
(global-set-key (kbd "C-z") 'er/expand-region)
(global-set-key (kbd "C-M-z") 'er/contract-region)
(setq expand-region-contract-fast-key (kbd "Z"))

;; move text
(require-package 'move-text)
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "ESC <up>") 'move-text-up)
(global-set-key (kbd "ESC <down>") 'move-text-down)

;; Highlight symbol
(require-package 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(eval-after-load 'highlight-symbol
  '(diminish 'highlight-symbol-mode))


;; Multiple cursor
(require-package 'multiple-cursors)
(global-set-key (kbd "M-@") 'mc--mark-symbol-at-point)
(global-set-key (kbd "C-M-@") 'mc/mark-all-like-this-in-defun)
(global-set-key (kbd "C-c c l") 'mc/edit-lines)
(global-set-key (kbd "C-c c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c c p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c c b") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c c a") 'mc/mark-all-like-this-in-defun)
(global-set-key (kbd "C-c c r") 'mc/mark-all-in-region)
(global-set-key (kbd "C-c c g") 'mc/mark-all-dwim)
;; kill back to indentation
(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

;; Dash at point
(require-package 'dash-at-point)
(global-set-key (kbd "C-h D") 'dash-at-point)

(provide 'editing)
