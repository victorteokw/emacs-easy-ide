(defconst old-packages-directory
  (expand-file-name "old-packages" user-emacs-directory)
  "This directory contains old and obsolete packages. But they are powerful.")

(defun require-old-package (package &optional do-not-require)
  (let* ((package-string (symbol-name package))
         (package-dir (expand-file-name package-string
                                        old-packages-directory)))

    (if (file-directory-p package-dir)
        (add-to-list 'load-path package-dir)
      (message "Cannot load old package '%s'" package))
    (unless do-not-require (require package))))


(defun load-directory (dir-name &optional error-string)
  "Load all files from a directory."
  (or (file-directory-p dir-name)
      (error (or error-string "'%s' is neither a directory nor exist.")
             dir-name))
  (dolist (file (directory-files dir-name t "^[^.]"))
    (if (file-directory-p file)
        (load-directory file)
      (load-file file))))



;; Set custom file
(setq custom-file (expand-file-name "custom.el"
                                    user-emacs-directory))

(defun find-package (package-name)
  "Visit downloaded package."
  (interactive
   (list (ido-completing-read
          "Package name: "
          (mapcar (lambda (cell) (symbol-name (car cell))) package-alist)
          nil t)))
  (let* ((package-dir
          (package-desc-dir
           (nth 0 (cdr (assoc (intern package-name) package-alist)))))
         (package-files
          (delete nil (mapcar (lambda (file-name)
                                (if (or (string-suffix-p "-pkg.el" file-name)
                                        (string-suffix-p "-autoloads.el"
                                                         file-name)) nil
                                  file-name)
                                ) (directory-files package-dir nil ".el$"))))
         (file-to-visit
          (if (equal 1 (length package-files))
              (nth 0 package-files)
            (ido-completing-read
             "File name: "
             package-files nil t))))
    package-files
    (find-file (expand-file-name file-to-visit package-dir))))
(global-set-key (kbd "C-x P") 'find-package)

(defun find-dot-file (dot-file-name)
  "Visit a dot file."
  (interactive (list (ido-completing-read
                      "Dot file: "
                      (delete
                       nil (mapcar (lambda (file-name)
                                     (if (and
                                          (string-prefix-p "." file-name)
                                          (not (string-match "^\\.+$"
                                                             file-name)))
                                         file-name nil))
                                   (directory-files "~")))
                      nil t)))
  (find-file (format "~/%s" dot-file-name)))
(global-set-key (kbd "C-x D") 'find-dot-file)

;; Find user init file.
(defun find-user-init-file ()
  "Go to emacs init file."
  (interactive)
  (find-file user-init-file))

;; A handier way to kill all buffers.
(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; Test for symlink and maybe visit target file instead.
(defun test-for-symlink ()
  "This function is added to `find-file-hook'.
If the file is symlink, prompt user to visit target instead."
  (let ((target-file (file-symlink-p buffer-file-name)))
    (if target-file
        (progn
          (setq buffer-read-only t)
          (when (yes-or-no-p (format "This file is a symlink.
Visit target '%s' instead?" target-file))
            (find-alternate-file target-file))))))
(add-hook 'find-file-hook 'test-for-symlink)


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


(defun setup-ido ()
  "This function setup ido everywhere."
  ;; configure ido
  (require 'ido)
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length 0)
  (setq ido-use-virtual-buffers t)

  ;; flx-ido
  (require-package 'flx-ido)
  (flx-ido-mode t)
  (setq ido-use-faces nil)

  ;; configure ido-ubiquitous
  (require-package 'ido-ubiquitous)
  (ido-ubiquitous-mode t)

  ;; Use smex to handle M-x
  (require-package 'smex)
  ;; Change path for ~/.smex-items
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  ;; Remap M-x to smex
  (global-set-key [remap execute-extended-command] 'smex)

  ;; Configure idomenu
  (require-package 'idomenu)

  ;; Allow the same buffer to be open in different frames
  (setq ido-default-buffer-method 'selected-window)

  ;; http://www.reddit.com/r/emacs/comments/21a4p9/use_recentf_and_ido_together/cgbprem
  (add-hook 'ido-setup-hook (lambda ()
                              (define-key ido-completion-map [up]
                                'previous-history-element))))

(defun setup-projectile ()
  (require-package 'projectile)
  (projectile-global-mode)
  (diminish 'projectile-mode)
  (require-package 'project-explorer))

(defun setup-helm ()
  (require-package 'helm)
  (require 'helm-config))

(defun setup-helm-projectile ()
  (require-package 'helm-projectile))

(defun setup-grizzl ()
  (require-package 'grizzl)
  (setq projectile-completion-system 'grizzl))

(defun setup-diminish ()
  (require-package 'diminish))

(defun setup-auto-complete ()
  (require-package 'auto-complete)
  (setq ac-ignore-case nil))

(defun setup-yasnippet ()
  (require-package 'yasnippet))

(defun setup-magit ()
  (require-package 'magit)
  (global-set-key (kbd "M-/") 'magit-status))

(defun setup-anzu ()
  (require-package 'anzu)
  (global-anzu-mode t)
  ;;  (diminish 'anzu)
  (global-set-key (kbd "M-%") 'anzu-query-replace-regexp)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace))

(defun setup-ace-jump ()
  (require-package 'ace-jump-mode)
  (eval-after-load "ace-jump-mode"
    '(ace-jump-mode-enable-mark-sync))
  (global-unset-key (kbd "M-h"))
  (define-key global-map (kbd "M-h c") 'ace-jump-char-mode)
  (define-key global-map (kbd "M-h l") 'ace-jump-line-mode)
  (define-key global-map (kbd "M-h w") 'ace-jump-word-mode)
  (define-key global-map (kbd "M-h p") 'ace-jump-mode-pop-mark)
  (define-key global-map (kbd "M-p") 'mark-paragraph))

(mapc (lambda (f) (add-hook 'after-init-hook f))
      (list 'setup-ido 'setup-projectile 'setup-helm 'setup-helm-projectile
            'setup-diminish 'setup-auto-complete 'setup-yasnippet
            'setup-magit 'setup-anzu 'setup-ace-jump))



;; Chinese support
(require-old-package 'eim)

(autoload 'eim-use-package "eim" "Another emacs input method")

(setq eim-use-tooltip nil)              ; don't use tooltip

(setq eim-punc-translate-p nil)         ; use English punctuation

(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "pinyin" "EIM Chinese Pinyin Input Method" "py.txt"
 'my-eim-py-activate-function)

(setq default-input-method "eim-py")

;; follow os x way
(defun my-eim-py-activate-function ()
  (add-hook 'eim-active-hook
            (lambda ()
              (let ((map (eim-mode-map)))
                (define-key eim-mode-map "[" 'eim-previous-page)
                (define-key eim-mode-map "]" 'eim-next-page)))))


;; Disable tool bar
;;(if (functionp 'tool-bar-mode) (tool-bar-mode 0))

;; Using meta-backtip to open menu bar
(define-key global-map (kbd "M-`") 'menu-bar-open)

(setq-default
 inhibit-splash-screen t ;; disable splash screen
 visible-bell t ;; do not beep
 indent-tabs-mode nil ;; use whitespace to indent
 line-number-mode t ;; show line numbers
 column-number-mode t ;; and column numbers
 make-backup-files nil ;; do not backup files
 initial-scratch-message ""
 tab-width 2
 ;; initial-major-mode 'ruby-mode
 )

(if (display-graphic-p)
    (progn
      (require-package 'solarized-theme)
      (load-theme 'solarized-light t)
      (setenv "PATH" (concat  "/usr/local/bin:" (getenv "PATH")))))



(provide 'setup)
;;; setup.el ends here
