;;; Editor basic


;;; Prefix keys

;; Use C-z as prefix key
(define-prefix-command 'eide-c-z)
(global-set-key (kbd "C-z") 'eide-c-z)

;; Use s-g as prefix key
(define-prefix-command 'eide-s-g)
(global-set-key (kbd "s-g") 'eide-s-g)

;; smex
(global-set-key [remap execute-extended-command] 'smex)
(global-set-key (kbd "s-P") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)



;;; Move cursor

;; C-a smart move
(global-set-key (kbd "C-a") 'eide-smart-beginning-of-line)

;; M-n and M-p to navigate to another appearence of current symbol
(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)

;; s-return to ignore current line and open new line
(global-set-key [s-return] 'eide-just-open-new-line)
;; M-s-return just go to next line end
(global-set-key [M-s-return] 'eide-just-return-to-next-line)

;; s-backspace just kill to indentation
(global-set-key (kbd "s-<backspace>") 'eide-kill-back-to-indentation)

;; Ace jump
(global-unset-key (kbd "C-;"))
(global-unset-key (kbd "M-;"))
(define-key global-map (kbd "C-;") 'ace-jump-char-mode)
(define-key global-map (kbd "C-:") 'ace-jump-word-mode)
(define-key global-map (kbd "C-M-;") 'ace-jump-line-mode)

;; mark

(global-set-key (kbd "s-y") 'eide-push-mark)
(global-set-key (kbd "s-u") 'eide-pop-mark)

;; bookmark
(global-set-key (kbd "s-T") 'bookmark-set)
(global-set-key (kbd "s-t") 'bookmark-jump)

;; Multiple cursors

;; Use s-g as prefix key
(define-prefix-command 'eide-s-g)
(global-set-key (kbd "s-g") 'eide-s-g)

(global-set-key (kbd "s-d") 'mc/mark-next-like-this)
(global-set-key (kbd "s-D") 'mc/skip-to-next-like-this)
(global-set-key (kbd "s-g s-d") 'mc/unmark-next-like-this)
(global-set-key (kbd "s-e") 'mc/mark-previous-like-this)
(global-set-key (kbd "s-E") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "s-g s-e") 'mc/unmark-previous-like-this)
(global-set-key (kbd "s-g l") 'mc/mark-all-dwim)
(global-set-key (kbd "s-g a") 'mc/mark-all-like-this)

(global-set-key (kbd "s-g d") 'mc/mark-all-like-this-in-defun)

(global-set-key [s-mouse-1] 'mc/add-cursor-on-click)

;; From active region to multiple cursors:
(global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c c c") 'mc/edit-lines)
(global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)

;;; Searching

(global-set-key (kbd "M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-M-%") 'anzu-query-replace)

;;; Code comment

;; s-; comment do what i mean
(global-set-key (kbd "s-;") 'comment-dwim)

;;; Editor file operation

;; close current file
(global-set-key (kbd "s-w") 'kill-this-buffer)

;; previous file and next file
(global-set-key (kbd "s-b") 'previous-buffer)
(global-set-key (kbd "s-f") 'next-buffer)

;;; Window and frame

(defun eide-other-window-backward (count &optional all-frames)
  "Not documented yet."
  (interactive "p")
  (other-window (- count) all-frames))

(global-set-key (kbd "s-1") 'other-window)
(global-set-key (kbd "s-!") 'eide-other-window-backward)
(global-set-key (kbd "s-2") 'split-window-right)
(global-set-key (kbd "s-3") 'split-window-below)
(global-set-key (kbd "s-W") 'delete-window)
(global-set-key (kbd "s-@") 'delete-other-windows)

;;; Project

;; C-s-p to switch project (same with sublime text)
(global-set-key [C-s-268632080] 'projectile-switch-project)

;; s-p to switch file in project (same with sublime text)
(global-set-key (kbd "s-p") 'projectile-find-file)

;;; Help

;; C-h D to dash
(global-set-key (kbd "C-h D") 'dash-at-point)

;;; C-z z to swoop
(global-set-key (kbd "C-z z") 'helm-swoop)

;;; Undo and Redo

(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)

;;; Hippie expand

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "s-'") 'hippie-expand)

;;; expand-region

(global-set-key (kbd "C-=") 'er/expand-region)


;;; Recent files

(define-key recentf-mode-map (kbd "s-O") 'recentf-ido-find-file)

;;; Search web

(global-set-key (kbd "C-z q b") 'eide-search-bing)

;;; Open with external app

(global-set-key (kbd "C-z o s") 'eide-open-with-sublime-text)
(global-set-key (kbd "C-z o t") 'eide-open-with-textmate)
(global-set-key (kbd "C-z o a") 'eide-open-with-atom)


;;; Org GTD

;; s-@ to org-capture
(global-set-key (kbd "C-z [") 'org-capture)

;; s-! to cycle org files
(global-set-key (kbd "C-z ]") 'org-cycle-agenda-files)

;; s-% to open agenda menu
(global-set-key (kbd "C-z \\") 'org-agenda)


(provide 'eide-key-bindings)
