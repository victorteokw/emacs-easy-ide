;;;;; Key Definition Sheet

;; The design of key strokes of this emacs configuration is combine the best of
;; Emacs and modern text editors such as Sublime Text, TextMate and Atom.

;; It should be as efficient as Emacs, as convinient as modern text editors.

;; Most of commands has two equivalent key bindings, one for terminal Emacs,
;; one for window system Emacs. Which means: it should work on terminal as plain
;; emacs, it should work on window system as modern text editor.

;; On different operation systems, the system default key bindings differ, this
;; key bindings react to mimic the default system key binding, makes emacs more
;; seamless with the underlying operating system.

(eval-when-compile
  (require 'evil))

;;;; Command modifiers

(eide-only :osx '(:gnu-emacs :emacs-mac)
  (setq mac-control-modifier 'control)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq mac-right-command-modifier 'hyper))

;; (eide-only :windows '(:gnu-emacs)
;;            (setq w32-lwindow-modifier 'meta))

;;;; Prefix keys

;; Use C-z as emacs style prefix key

(define-prefix-command 'eide-c-z)
(global-set-key (kbd "C-z") 'eide-c-z)

;;;; Cancel

;; Use C-g to cancel

;; Esc doesn't work well for cancel

;;;; Exit

;; Use C-x C-s to exit

;; Use s-q to exit on OS X, too

(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)

;;;; Restart

;; Use C-M-s-w to restart emacs

(global-set-key (kbd "C-M-s-w") 'restart-emacs)

;;;; Suspend

;; Use C-x C-z to suspend

;; Use s-m to suspend on OS X, too

(global-set-key (kbd "s-m") 'iconify-frame)

;;;; Toggle full screen

;; Use f11 to toggle full screen

;; Use C-s-f to toggle full screen on OS X, too

(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

;;;; Configuration

;; Use s-, to customize on OS X

;; Use `customize' to customize.

(eide-only :osx '(:emacs-mac)
  (global-set-key (kbd "s-,") 'customize))

;; Use C-z c i to go to user init file

(global-set-key (kbd "C-z c i") 'eide-goto-init-file)

;; Use C-z c c to go to a configuration file

(global-set-key (kbd "C-z c c") 'eide-goto-conf-file)

;; Use C-z c k to go to key binding file

(global-set-key (kbd "C-z c k") 'eide-goto-key-bindings-file)

;;;; Command Palette (smex)

;; M-x to open command palette (emacs way)

(global-set-key [remap execute-extended-command] 'smex)

;; s-P to open command palette (sublime text way)

(global-set-key (kbd "s-P") 'smex)

;; M-S-x for major mode commands

(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;;; Menu

;; Use M-` to open menu bar

(define-key global-map (kbd "M-`") 'menu-bar-open)

;;;; Help

;; C-h k to get usage of key

;; C-h c to get usage of key too but not verbose

;; C-h f to get usage of function

;; C-h v to get usage of variable

;; C-h a to get command match predicate

;; C-h d to get everything match predicate

;; C-h b to get all key bindings

;; C-h m to get usage of current mode

;; C-h e to go to message buffer to see what's going on

;; C-h i to read manual

;; C-h D to dash about thing at point

(global-set-key (kbd "C-h D") 'dash-at-point)



;;;; Cursor, mark and selection

;; Cursor is the most important to editing, all your insertion, deletion and
;; modification occurs on cursor. Every cursor has a corresponding mark, a
;; cursor and a mark define a selection.
;;
;; Most of the time, you need only one cursor, additional cursors are very
;; helpful in some cases.
;;
;; You can add cursor and remove cursor however you can not remove the only
;; cursor.

;; Use M-space to set mark

(global-set-key (kbd "M-SPC") 'set-mark-command)

;; Use C-x C-x to exchange cursor and mark

;; TODO: Remove multiple cursors section, multiple cursors should feel like
;; just normal.

;;; Cursors

(global-set-key (kbd "s-d") 'eide-mark-word)

(global-set-key (kbd "H-d") 'mc/mark-next-like-this)
(global-set-key (kbd "H-D") 'mc/skip-to-next-like-this)
(global-set-key (kbd "H-M-d") 'mc/unmark-next-like-this)
(global-set-key (kbd "H-e") 'mc/mark-previous-like-this)
(global-set-key (kbd "H-E") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "H-M-e") 'mc/unmark-previous-like-this)
(global-set-key (kbd "H-l") 'mc/mark-all-dwim)
(global-set-key (kbd "H-a") 'mc/mark-all-like-this)

(global-set-key (kbd "H-f") 'mc/mark-all-like-this-in-defun)

(global-set-key (kbd "C-<down-mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (kbd "s-<down-mouse-1>") 'eide-jump-to-definition)

;; From active region to multiple cursors:
(global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c c c") 'mc/edit-lines)
(global-set-key (kbd "s-L") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)

;;; Basic selection

;; C-x h and s-a to select all

(global-set-key (kbd "s-a") 'mark-whole-buffer)

;;; Expand region

;; s-e to expand current region

(global-set-key (kbd "s-e") 'er/expand-region)

;;; Expand line

;; s-l to expand line

(global-set-key (kbd "s-l") 'turn-on-expand-line-mode)

(eval-after-load "expand-line"
  '(define-key expand-line-mode-map (kbd "s-l") 'expand-line-expand-next-line))

;;;; Movement

;;; Basic move

;; C-b, C-f, C-n, C-p same as arrows

;; C-a smart move to beginning of line

(global-set-key (kbd "C-a") 'eide-smart-beginning-of-line)

;; C-e move to end of line

;; M-r place cursor mid, top and bot of line

;;; Move to absolute position

;; M-g g go to line

;; M-g c go to char

;;; Move to exact somewhere in screen (avy)

;; C-; to jump to char

(global-unset-key (kbd "C-;"))
(define-key global-map (kbd "C-;") 'avy-goto-char)

;; C-S-; to jump to word

(define-key global-map (kbd "C-:") 'avy-goto-word-1)

;; C-M-; to jump to line

(define-key global-map (kbd "C-M-;") 'avy-goto-line)

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

;;; Move current line

(global-set-key [C-s-down] 'md/move-lines-down)
(global-set-key [C-s-up] 'md/move-lines-up)
(global-set-key [C-M-s-down] 'md/duplicate-down)
(global-set-key [C-M-s-up] 'md/duplicate-up)

;;; Swoop

;; C-c w to swoop
;; TODO: change swoop's keybinding

(global-set-key (kbd "C-c w") 'helm-swoop)

;;; Tab jump out quote or brackets



;;; Jump to definition

(global-set-key (kbd "M-.") 'eide-jump-to-definition)

(eval-after-load "ggtags"
  '(progn
     (define-key ggtags-mode-map (kbd "M-.") 'eide-jump-to-definition)))

;;;; Insertion

;;; Basic insertion

;; Most keys insert themselves through `self-insert-command'.

;; s-return to ignore current line and open new line

(global-set-key [s-return] 'eide-just-open-new-line)

;;; Hippie expand

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "s-'") 'hippie-expand)

;;; auto complete

;; auto complete will automatically trigger, use C-g to cancel

;; tab to complete

(define-key ac-completing-map [tab] 'ac-complete)
(define-key company-active-map [tab] 'company-complete-selection)

;; return to complete common part

(define-key ac-completing-map [return] 'ac-expand)
(define-key company-active-map [return] 'company-complete-common)

;;; Insertion by copy and paste

;; C-w to cut region or line

;; M-w to copy region or line

;; C-y to paste

;; M-y to cycle paste

;; C-Y to paste and indent

(global-set-key (kbd "C-Y") 'eide-yank-and-indent)

;; M-Y to cycle paste and indent

(global-set-key (kbd "M-Y") 'eide-yank-pop-and-indent)

;; C-M-y to insert by kill ring

(global-set-key (kbd "C-M-y") 'browse-kill-ring)

(eide-only :osx '(:emacs-mac :gnu-emacs)

  ;; s-x to cut region or line

  (global-set-key (kbd "s-x") 'kill-region)

  ;; s-c to copy region or line

  (global-set-key (kbd "s-c") 'kill-ring-save)

  ;; s-v to paste

  (global-set-key (kbd "s-v") 'yank)

  ;; s-M-v to cycle paste

  (global-set-key (kbd "s-M-v") 'yank-pop)

  ;; s-M-V to cycle paste and indent

  (global-set-key (kbd "s-M-V") 'eide-yank-pop-and-indent)

  ;; s-V to paste and indent

  (global-set-key (kbd "s-V") 'eide-yank-and-indent)

  ;; C-M-s-v to insert by kill ring

  (global-set-key (kbd "C-M-s-v") 'browse-kill-ring))

;;;; Deletion

;;; Basic deletion

;; C-d to delete forward, backspace to delete backward

;; C-k to kill to end of line

;; s-backspace just kill to indentation

(global-set-key (kbd "s-<backspace>") 'eide-kill-back-to-indentation)

;;;; Modification

;;; Uppercase and lowercase

;; M-u to upcase word

;; M-l to downcase word

;; M-c to capitalize word

;;; Search and replace

;; C-s and C-r : isearch-forward and isearch-backward
;; C-M-s and C-M-r : isearch-forward-regexp and isearch-backward-regexp
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(global-set-key (kbd "s-f") 'isearch-forward)
(global-set-key (kbd "s-F") 'projectile-grep)
(global-set-key (kbd "s-r") 'anzu-query-replace)
(global-set-key (kbd "s-R") 'anzu-query-replace-regexp)

;;; Code comment

;; M-; to comment

;; s-/ to comment dwim

(global-set-key (kbd "s-/") 'comment-dwim)



;;;; Buffer

;;; Open file

;; C-x C-f to open a file

;; s-o to open a file

(global-set-key (kbd "s-o") 'ido-find-file)

;; s-O to open a recent file

(define-key recentf-mode-map (kbd "s-O") 'recentf-ido-find-file)

;;; Save file

;; C-x C-s to save current buffer

;; s-s to save current buffer

(global-set-key (kbd "s-s") 'save-buffer)

;; C-x C-w to write file

;; s-S to write file (save-as)

(global-set-key (kbd "s-S") 'ido-write-file)

;; C-x s to save all

;; s-M-s to save all

(global-set-key (kbd "s-M-s") 'save-some-buffers)

;;; Close file

;; s-w to close current file

(global-set-key (kbd "s-w") 'kill-this-buffer)

;; s-M-w to kill last buffer

(global-set-key (kbd "s-M-w") 'eide-kill-last-buffer)
(global-set-key (kbd "s-M-∑") 'eide-kill-last-buffer)

;;; Switch buffer

;; C-x b to switch buffer

;; M-s- left arrow, M-s- right arrow

(global-set-key (kbd "M-s-<left>") 'previous-buffer)
(global-set-key (kbd "M-s-<right>") 'next-buffer)
(global-set-key (kbd "M-s-<up>") 'eide-find-other-file)
;;(global-set-key (kbd "M-s-<down>") 'eide-find-implementation)

;;; Bookmarking

;; s-T to set bookmark

(global-set-key (kbd "s-T") 'bookmark-set)

;; s-t to goto bookmark

(global-set-key (kbd "s-t") 'bookmark-jump)

;;;; Window

;; C-x o to cycle window

;; C-x 3 to split window right

;; C-x 2 to split window below

;; C-x 0 to close current window

;;;; Frame

;; C-x 5 o to cycle frame
;; s-` to cycle frame

(global-set-key (kbd "s-`") 'other-frame)

;; C-x 5 2 to create frame

;; s-N to create frame

(global-set-key (kbd "s-N") 'make-frame-command)

;; C-x 5 0 to close frame

;; s-W to close frame

(global-set-key (kbd "s-W") 'delete-frame)



;;;; Undo and Redo

;;; Undo and redo for editing

;; Use s-z to undo

(global-set-key (kbd "s-z") 'undo-tree-undo)

;; Use s-Z to redo

(global-set-key (kbd "s-Z") 'undo-tree-redo)

;; Use C-x u to visualize undo tree

;;; Undo and redo for buffer, window, and frame (aka workspace)

;; TODO: Add undo redo for these things

;;;; Editing modes

;; Use ESC to toggle along editing modes

(global-set-key [escape] 'eide-toggle-editing-mode)

(eval-after-load "evil"
  '(define-key evil-normal-state-map
     [remap evil-force-normal-state] 'eide-toggle-editing-mode))



;;;; Visual coding structure

;;; Code structure

(global-set-key (kbd "M-m") 'helm-imenu)

;;; Code folding

;; M-s-[ to fold

(define-key hs-minor-mode-map (kbd "M-s-[") 'hs-hide-block)
(define-key origami-mode-map (kbd "M-s-[") 'origami-close-node)

;; M-s-] to show

(define-key hs-minor-mode-map (kbd "M-s-]") 'hs-show-block)
(define-key origami-mode-map (kbd "M-s-]") 'origami-open-node)

;; H-F to fold all

(define-key hs-minor-mode-map (kbd "H-F") 'hs-hide-all)
(define-key origami-mode-map (kbd "H-F") 'origami-close-all-nodes)

;; H-S to show all

(define-key hs-minor-mode-map (kbd "H-S") 'hs-show-all)
(define-key origami-mode-map (kbd "H-S") 'origami-open-all-nodes)

;;;; Miscellaneous

;;; UI

(eide-only :osx nil
  ;; OSX, s-+ and s-- to adjust font size (TextMate and Sublime Text)
  (global-set-key (kbd "s-+") 'text-scale-adjust)
  (global-set-key (kbd "s-=") 'text-scale-adjust)
  (global-set-key (kbd "s--") 'text-scale-adjust)

  ;; OSX, s-M-l to toggle line number (TextMate)
  (global-set-key (kbd "s-M-l") 'global-linum-mode)

  ;; OSX, s-M-i to show invisible chars
  (global-set-key (kbd "s-M-i") 'global-whitespace-mode)
  )

;;; Project

;; most with projectile

;; C-z p p to switch project (emacs way)
;; C-s-p   to switch project (sublime text way)
(global-set-key (kbd "C-s-p") 'projectile-switch-project)

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

;;; Scratches

;; Use "C-z s n"   to scratches-new-scratch-dwim
;; Use "C-z s 4 n"   to scratches-new-scratch-other-window-dwim
;; Use "C-z s 5 n"   to scratches-new-scratch-other-frame-dwim

;; Use "C-z s f"   to scratches-visit-scratch
;; Use "C-z s 4 f"   to scratches-visit-scratch-other-window
;; Use "C-z s 5 f"   to scratches-visit-scratch-other-frame

;; Use "C-z s k"   to scratches-kill-all-scratches

;;; CVS

;; C-z g g or M-f12 to view git status

(global-set-key (kbd "C-z g g") 'magit-status)
(global-set-key [(meta f12)] 'magit-status)

;; C-z g b to blame

(global-set-key (kbd "C-z g b") 'magit-blame-popup)

;; C-z g v to browse the file on github

(global-set-key (kbd "C-z g v") 'github-browse-file)

;; C-z g l to browse the file on github with blame

(global-set-key (kbd "C-z g C-b") 'github-browse-file-blame)

;; C-z g c to clone repo from github

(global-set-key (kbd "C-z g c") 'github-clone)

;;;; Search web

;; C-z q b to search bing

(global-set-key (kbd "C-z q b") 'eide-search-bing)

;; C-z q g to search google

(global-set-key (kbd "C-z q g") 'eide-search-google)

;; C-z q y to search youtube

(global-set-key (kbd "C-z q y") 'eide-search-youtube)

;;;; Open with external app

;; C-z o f to open with finder

(global-set-key (kbd "C-z o f") 'eide-open-with-finder)

;; C-z o s to open with Sublime Text

(global-set-key (kbd "C-z o s") 'eide-open-with-sublime-text)

;; C-z o t to open with TextMate

(global-set-key (kbd "C-z o t") 'eide-open-with-textmate)

;; C-z o a to open with Atom

(global-set-key (kbd "C-z o a") 'eide-open-with-atom)

;;;; Package

;; C-f9 to install package

(global-set-key [C-f9] 'package-install)

;; M-f9 to list packages

(global-set-key [M-f9] 'package-list-packages)

;; f9 to list packages no fetch

(global-set-key [f9] 'package-list-packages-no-fetch)

;;;; Org GTD

;; Use C-z [ to org-capture

(global-set-key (kbd "C-z [") 'org-capture)

;; Use C-z ] to cycle org files

(global-set-key (kbd "C-z ]") 'org-cycle-agenda-files)

;; Use C-z \ to open agenda menu

(global-set-key (kbd "C-z \\") 'org-agenda)

;;;; Open dired

;; C-M-s-d to open dired on this file

(global-set-key (kbd "C-M-s-d") 'eide-dired-on-file)

;; C-M-s-d to close dired buffer

(define-key dired-mode-map (kbd "C-M-s-d") 'eide-close-and-back)

;;;; Shell

;; Use C-z C-s to open shell and switch back

(global-set-key (kbd "C-z C-s") 'shell)
(eval-after-load "shell"
  '(define-key shell-mode-map (kbd "C-z C-s") 'previous-buffer))

;; Use C-M-s-s to open shell on OS X and switch back

(global-set-key (kbd "C-M-s-s") 'shell)
(eval-after-load "shell"
  '(define-key shell-mode-map (kbd "C-M-s-s") 'previous-buffer))

;; Use s-k to clear buffer

(eval-after-load "shell"
  '(define-key shell-mode-map (kbd "s-k") 'erase-buffer))

(provide 'eide-key-bindings)
