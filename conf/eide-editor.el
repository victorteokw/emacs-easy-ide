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

;;; pretty symbol everywhere

(global-prettify-symbols-mode)

;;; Project management

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

(require 'anzu)
(global-anzu-mode t)

;;; jumping

(eval-after-load "ace-jump-mode"
  '(progn
     ;; synx mark
     (ace-jump-mode-enable-mark-sync)
     ;; jump after
     (add-hook 'ace-jump-mode-end-hook 'forward-char)))

;;; bookmark

(eval-after-load "bookmark"
  '(setq bookmark-default-file (expand-file-name "bookmarks" eide-etc-dir)
         bookmark-save-flag 1))

;;; global auto revert

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;;; Undo and Redo

;; use command+z to undo, command+shift+z to redo
(global-undo-tree-mode)

;;; Global Hippie Expand

(eval-after-load "hippie-exp"
  '(setq hippie-expand-try-functions-list
         '(try-complete-file-name-partially
           try-complete-file-name
           try-expand-dabbrev
           try-expand-dabbrev-all-buffers
           try-expand-dabbrev-from-kill)))

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



(provide 'eide-editor)
