;;; Default indentation

(setq-default
 indent-tabs-mode nil ;; use whitespace to indent
 tab-width 2          ;; tab width 2
 )

;;; Do not backup files

(setq-default
 make-backup-files nil ;; do not backup files
 fill-column 80 ;; 80 fill column
 )

;;; Local variable safety

(setq enable-local-variables nil)
(setq enable-local-eval nil)

;;; exec path for os x and maybe other window systems
(if window-system
    (progn
      (require 'exec-path-from-shell)
      (exec-path-from-shell-initialize)))

;;; when selection is active, typing cause it deleted

(delete-selection-mode)

;;; Always superword

(global-superword-mode)

;;; pretty symbol everywhere

(global-prettify-symbols-mode)

;;; Project management
(setq-default projectile-keymap-prefix (kbd "C-z p"))
(eval-after-load "projectile"
  '(progn
     (setq projectile-cache-file (f-expand  "projectile.cache" eide-etc-dir))
     (setq projectile-known-projects-file (f-expand "projectile-bookmarks.eld" eide-etc-dir))
     ;; open main dir after go to that project
     (setq projectile-switch-project-action 'projectile-dired)
     (projectile-load-known-projects)
     ))

(projectile-global-mode)


;; (add-hook 'projectile-switch-project-hook ')

;;; Cursor Moving

(defun eide-push-mark ()
  "Not documented yet."
  (interactive)
  (set-mark-command nil)
  (deactivate-mark))


(defun eide-pop-mark ()
  "Not documented yet."
  (interactive)
  (set-mark-command 1))

;;; searching

(global-anzu-mode t)

;;; jumping

(eval-after-load "ace-jump-mode"
  '(progn
     ;; synx mark
     (ace-jump-mode-enable-mark-sync)
     ;; jump after
     (add-hook 'ace-jump-mode-end-hook
               (lambda () (unless (= (line-end-position) (line-beginning-position))
                       (forward-char))))))

;;; bookmark

(eval-after-load "bookmark"
  '(setq bookmark-default-file (expand-file-name "bookmarks" eide-etc-dir)
         bookmark-save-flag 1))

;;; global auto revert

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Undo and Redo

;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

(global-undo-tree-mode)


;;; Global Hippie Expand

(eval-after-load "hippie-exp"
  '(setq hippie-expand-try-functions-list
         '(try-complete-file-name-partially
           try-complete-file-name
           try-expand-dabbrev
           try-expand-dabbrev-all-buffers
           try-expand-dabbrev-from-kill)))

;;; Cold folding

(require 'origami)
(require 'hideshow)

;;; Key bindings

(define-key hs-minor-mode-map (kbd "s-g f") 'hs-hide-block)
(define-key hs-minor-mode-map (kbd "s-g F") 'hs-hide-all)
(define-key hs-minor-mode-map (kbd "s-g s") 'hs-show-block)
(define-key hs-minor-mode-map (kbd "s-g S") 'hs-show-all)

(define-key origami-mode-map (kbd "s-g f") 'origami-close-node)
(define-key origami-mode-map (kbd "s-g F") 'origami-close-all-nodes)
(define-key origami-mode-map (kbd "s-g s") 'origami-open-node)
(define-key origami-mode-map (kbd "s-g S") 'origami-open-all-nodes)

;;

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" eide-etc-dir))
;; activate it for all buffers
(setq-default save-place t)


;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" eide-etc-dir))
(savehist-mode +1)



;;; Expand region

;; we need to do this to make expand-region work
(defalias 'save-mark-and-excursion 'save-excursion)

;; kill back to indentation
(defun eide-kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

;; move line up and down
(global-set-key [s-down] 'md/move-lines-down)
(global-set-key [s-up] 'md/move-lines-up)
(global-set-key [M-s-down] 'md/duplicate-down)
(global-set-key [M-s-up] 'md/duplicate-up)

;; whole line or region
(require 'whole-line-or-region)
(whole-line-or-region-mode t)
(make-variable-buffer-local 'whole-line-or-region-mode)

;; Paste and indent

(defadvice whole-line-or-region-yank (after paste-and-indent activate)
  (call-interactively 'indent-region))

;;; Highlight escape sequences

(hes-mode)

;;; Extended return behavior

(defun eide-just-return-to-next-line ()
  "Ignore this line and move to next line."
  (interactive)
  (forward-line)
  (move-end-of-line 1))

(defun eide-just-open-new-line ()
  "Ignore this line and open new line below."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

;;; Add new line if no lines at eof

(setq next-line-add-newlines t)

;;; Multiple Cursors
(eval-after-load "multiple-cursors"
  '(setq mc/list-file (f-expand ".mc-lists.el" eide-etc-dir)))

;;; Auto complete

(eval-after-load "auto-complete"
  '(setq ac-comphist-file (f-expand "ac-comphist.dat" eide-etc-dir)))

(defun eide-smart-beginning-of-line ()
  (interactive)
  (let ((p (point)))
    (back-to-indentation)
    (if (= p (point))
        (move-beginning-of-line 1))))

;; Use M-m for helm-imenu
(global-set-key (kbd "M-m") 'helm-imenu)

;;; Snippets

(require 'yasnippet)
(setq yas-snippet-dirs (remove 'yas-installed-snippets-dir yas-snippet-dirs))
(yas-global-mode)

;;; pcache

(eval-after-load "pcache"
  (setq pcache-directory (f-expand "pcache" eide-etc-dir)))

;;; Auto save list

(setq auto-save-list-file-prefix (f-expand "auto-save-list/.saves-" eide-etc-dir))

;;; remember recent files

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((file-assoc-list
          (mapcar (lambda (x)
                    (cons (file-name-nondirectory x)
                          x))
                  recentf-list))
         (filename-list
          (remove-duplicates (mapcar #'car file-assoc-list)
                             :test #'string=))
         (filename (ido-completing-read "Choose recent file: "
                                        filename-list
                                        nil
                                        t)))
    (when filename
      (find-file (cdr (assoc filename
                             file-assoc-list))))))

(recentf-mode)

;;; remove url
(eval-after-load "url"
  '(setq url-configuration-directory (f-expand "url" eide-etc-dir)))

;;; Desktop restore and save

(setq desktop-path (list (f-expand "etc" user-emacs-directory)))
(desktop-save-mode 1)

;;; Better selection

(defvar eide-point-before-mark nil
  "This var is used to restore point when C-g in marking.")

(defvar eide-mark-functions
  '(mark-page mark-paragraph mark-whole-buffer mark-sexp mark-defun mark-word)
  "Functions behave like mark.")

(defadvice mark-page (before eide-set-restore-before-mark activate)
  (setq eide-point-before-mark (point)))
(defadvice mark-paragraph (before eide-set-restore-before-mark activate)
  (setq eide-point-before-mark (point)))
(defadvice mark-whole-buffer (before eide-set-restore-before-mark activate)
  (setq eide-point-before-mark (point)))
(defadvice mark-sexp (before eide-set-restore-before-mark activate)
  (setq eide-point-before-mark (point)))
(defadvice mark-defun (before eide-set-restore-before-mark activate)
  (setq eide-point-before-mark (point)))
(defadvice mark-word (before eide-set-restore-before-mark activate)
  (setq eide-point-before-mark (point)))

(defadvice keyboard-quit (before eide-restore-cursor-cg-mark activate)
  (when (memq last-command eide-mark-functions)
    (eide-restore-cursor)))

(defun eide-restore-cursor ()
  (if eide-point-before-mark
      (progn
        (goto-char eide-point-before-mark)
        (setq eide-point-before-mark nil))))

(provide 'eide-editor)
