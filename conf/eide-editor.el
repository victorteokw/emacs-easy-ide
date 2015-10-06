;;; Default indentation

(setq-default
 indent-tabs-mode nil ;; use whitespace to indent
 tab-width 2          ;; tab width 2
 standard-indent 2    ;; standard indent 2
 )

;;; Do not backup files

(setq-default
 make-backup-files nil ;; do not backup files
 fill-column 80 ;; 80 fill column
 )

;;; Local variable safety

(setq enable-local-variables nil)
(setq enable-local-eval nil)

;;; Dir local variable safety

(setq enable-dir-local-variables nil)

;;; exec path for os x and maybe other window systems

(eide-only :osx '(:gnu-emacs :emacs-mac)
  (exec-path-from-shell-initialize))

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
     (projectile-load-known-projects)))

(projectile-global-mode)

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

;;; Absolute cursor moving

(setq-default avy-background t)
(setq-default avy-all-windows nil)

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
      `((".*" . ,eide-etc-dir)))
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

;;; whole line or region

(require 'whole-line-or-region)
(whole-line-or-region-mode t)
(make-variable-buffer-local 'whole-line-or-region-mode)

;;; Paste and indent

(defun eide-yank-and-indent ()
  "Yank and indent."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(defun eide-yank-pop-and-indent ()
  "Yank pop and indent."
  (interactive)
  (yank-pop)
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
  (setq mc/list-file (f-expand "eide-cursor-commands.el" eide-conf-dir)))

;;; Auto complete

(require 'auto-complete)

(setq ac-expand-on-auto-complete nil)
(setq ac-comphist-file (f-expand "ac-comphist.dat" eide-etc-dir))
(setq ac-auto-start 0.1)
(setq ac-delay 0.1)
(setq ac-auto-show-menu 0.1)
(setq ac-quick-help-delay 0.1)

(ac-define-source eide-yasnippet
  '((depends yasnippet)
    (candidates . ac-yasnippet-candidates)
    (action . yas/expand)
    (symbol . "y")))

(require 'company)

(setq-default company-minimum-prefix-length 0)
(setq-default company-idle-delay 0.1)
(setq-default company-tooltip-align-annotations t)

;;;

(defun eide-smart-beginning-of-line ()
  (interactive)
  (let ((p (point)))
    (back-to-indentation)
    (if (= p (point))
        (move-beginning-of-line 1))))

;;; Snippets

(require 'yasnippet)
(setq yas-snippet-dirs (remove 'yas-installed-snippets-dir yas-snippet-dirs))
(yas-global-mode)

;;; Tab to jump out

(setq yas-fallback-behavior '(apply tab-jump-out 1))

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
          (cl-remove-duplicates (mapcar #'car file-assoc-list)
                                :test #'string=))
         (filename (ido-completing-read "Choose recent file: "
                                        filename-list
                                        nil
                                        t)))
    (when filename
      (find-file (cdr (assoc filename
                             file-assoc-list))))))

(recentf-mode)

;;; URL better place

(eval-after-load "url"
  '(setq url-configuration-directory (f-expand "url" eide-etc-dir)))

;;; Desktop restore and save

(setq desktop-path (list (f-expand "etc" user-emacs-directory)))
(desktop-save-mode 1)

;;; Better selection

(smart-mark-mode)

;;; Dired

(setq dired-listing-switches "-alh")

;;; Evil

(eval-after-load "evil"
  '(progn
     (setq evil-mode-line-format 'after)
     (defadvice evil-generate-mode-line-tag ;; No effect don't know why
         (around eide-modeline-better-tag activate)
       (let ((result ad-do-it))
         (if (not (null result))
             (progn
               (setq result (s-trim result))
               (propertize result 'face 'font-lock-constant-face)
               result)
           result)))))

;;; Whitespace cleanup

(add-hook 'before-save-hook 'whitespace-cleanup nil nil)

;;; Multiple scratches

(setq-default scratches-keymap-prefix (kbd "C-z s"))
(scratches-global-mode)

;;; Custom file

(setq custom-file (f-expand "custom.el" eide-etc-dir))

(provide 'eide-editor)
