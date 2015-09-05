;;;;; Key definition sheet

;; The design of key strokes of this emacs configuration is combine the best of
;; Emacs and modern text editors such as Sublime Text, TextMate and Atom.

;; It should be as efficient as Emacs, as convinient as modern text editors.

;; Most of commands has two equivalent key bindings, one for terminal Emacs,
;; one for window system Emacs. Which means: it should work on terminal as plain
;; emacs, it should work on window system as modern text editor.

;;;; Prefix keys

;; Use C-z as prefix key

(define-prefix-command 'eide-c-z)
(global-set-key (kbd "C-z") 'eide-c-z)

;; Use s-g as prefix key

(define-prefix-command 'eide-s-g)
(global-set-key (kbd "s-g") 'eide-s-g)

;; Use s-k as prefix key for window manipulation

(define-prefix-command 'eide-s-k)
(global-set-key (kbd "s-k") 'eide-s-k)

;;;; Cancel

;; Use C-g to cancel

;; Esc doesn't work well for cancel

;;;; Command Palette (smex)

;; M-x to open command palette (emacs way)

(global-set-key [remap execute-extended-command] 'smex)

;; s-S-p to open command palette (sublime text way)

(global-set-key (kbd "s-P") 'smex)

;; M-S-x for major mode commands

(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;;; Move cursor

;;; Basic move

;; C-b, C-f, C-n, C-p same as arrows

;; C-a smart move to beginning of line

(global-set-key (kbd "C-a") 'eide-smart-beginning-of-line)

;; C-e move to end of line

;; M-r place cursor mid, top and bot of line

;;; Move to absolute position

;; M-g g go to line

;; M-g c go to char

;;; Move to exact somewhere in screen (ace-jump)

;; C-; to jump to char

(global-unset-key (kbd "C-;"))
(define-key global-map (kbd "C-;") 'ace-jump-char-mode)

;; C-S-; to jump to word

(define-key global-map (kbd "C-:") 'ace-jump-word-mode)

;; C-M-; to jump to line

(define-key global-map (kbd "C-M-;") 'ace-jump-line-mode)

;;; Move by screen

;; C-v scroll up

;; M-v scroll down

;;; Symbol move

;; M-n and M-p to navigate to another appearence of current symbol

(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)

;;; Move with mark

;; s-y to set mark, s-u to return to that mark

(global-set-key (kbd "s-y") 'eide-push-mark)
(global-set-key (kbd "s-u") 'eide-pop-mark)

;;;; Insertion

;;; Basic insertion

;; Most keys insert themselves through `self-insert-command'.

;; s-return to ignore current line and open new line

(global-set-key [s-return] 'eide-just-open-new-line)

;;; Multiple cursors

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

;;; Search and replace

;; C-s and C-r : isearch-forward and isearch-backward
;; C-M-s and C-M-r : isearch-forward-regexp and isearch-backward-regexp
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(global-set-key (kbd "s-f") 'isearch-forward)
(global-set-key (kbd "s-F") 'isearch-forward-regexp)
(global-set-key (kbd "s-g s-f") 'projectile-grep)
(global-set-key (kbd "s-r") 'anzu-query-replace)
(global-set-key (kbd "s-R") 'anzu-query-replace-regexp)
(global-set-key (kbd "s-g s-r") 'projectile-replace)

;;; Code comment

;; M-; comment
;; s-; comment do what i mean
(global-set-key (kbd "s-;") 'comment-dwim)

;;;; Selection

;;; Basic selection

;;; Expand region

;; s-L to expand current region

(global-set-key (kbd "s-L") 'er/expand-region)

;;;; Deletion

;;; Basic deletion

;; C-d to delete forward, backspace to delete backward

;; C-k to kill to end of line

;; s-backspace just kill to indentation

(global-set-key (kbd "s-<backspace>") 'eide-kill-back-to-indentation)

;;; Code folding

;; s-g f to fold
;; s-g s to show
;; s-g F to fold all
;; s-g S to show all

;;; Editor file operation

;; close current file
(global-set-key (kbd "s-w") 'kill-this-buffer)
(global-set-key (kbd "s-M-∑") 'eide-kill-last-buffer)

;; previous file and next file
;; (global-set-key (kbd "s-b") 'previous-buffer)
;; (global-set-key (kbd "s-f") 'next-buffer)

;;; Window and frame

;; Need an package

;;; Find file

;; bookmark
(global-set-key (kbd "s-T") 'bookmark-set)
(global-set-key (kbd "s-t") 'bookmark-jump)
(global-set-key (kbd "s-g t") 'bookmark-bmenu-list)

;;; Project

;; most with projectile

;; C-z p p to switch project (emacs way)
;; C-s-p   to switch project (sublime text way)
(global-set-key [C-s-268632080] 'projectile-switch-project)

;; C-z p f to switch file in project (emacs way)
;; s-p     to switch file in project (sublime text way)
(global-set-key (kbd "s-p") 'projectile-find-file)
;; C-z p 4 f  switch file in project new window

;; C-z p g   to switch file at point in project
;; C-z p 4 g to switch file at point in project new window

;; C-z p d   to switch dir in project
;; C-z p 4 d to switch dir in project new window
;; C-z p D   to open root dir

;; C-z p b   to switch to opened file in project
;; C-z p 4 b to switch to opened file in project

;; C-z p a   to switch file with same name diff ext
;; C-z p 4 a to swtich file with same name diff ext

;; C-z p T   to display a list of test files

;; C-z p o   runs multi-occur on all project buffers currently open

;; C-z p k   kill all this project buffer

;; C-z p e   show list of recently visited project files

;; C-z p C-h show projectile help

;; Project query and replace see Search and replace above

;;; CVS

;; C-z g g or M-f12 to view git status
(global-set-key (kbd "C-z g g") 'magit-status)
(global-set-key [(meta f12)] 'magit-status)
;; C-z g b to blame
(global-set-key (kbd "C-z g b") 'magit-blame-popup)
;; C-z g b to browse the file on github
(global-set-key (kbd "C-z g C-g") 'github-browse-file)
;; C-z g l to browse the file on github with blame
(global-set-key (kbd "C-z g C-b") 'github-browse-file-blame)
;; C-z g c to clone repo from github
(global-set-key (kbd "C-z g c") 'github-clone)
;; yagist not bind anything yet
;; gist not bind anything yet

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

;;; Recent files

(define-key recentf-mode-map (kbd "s-O") 'recentf-ido-find-file)

;;; Search web

(global-set-key (kbd "C-z q b") 'eide-search-bing)
(global-set-key (kbd "C-z q g") 'eide-search-google)

;;; Open with external app

(global-set-key (kbd "C-z o f") 'eide-open-with-finder)
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

;;; Go to key binding file

(defconst eide-key-bindings-file (or load-file-name
                                     (buffer-file-name (current-buffer))))

(defun eide-goto-key-bindings-file ()
  (interactive)
  (find-file eide-key-bindings-file))

(global-set-key (kbd "C-z k") 'eide-goto-key-bindings-file)

(provide 'eide-key-bindings)
