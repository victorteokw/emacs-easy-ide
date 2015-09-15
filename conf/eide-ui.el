;;; Do not show toolbar

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;;; Do not show scroll bar

(scroll-bar-mode -1)

;;; Do not show startup screen

(setq-default inhibit-splash-screen t)

;;; Initial scratch message

(setq-default initial-scratch-message "\n")

;;; Nice scrolling

;; copied from prelude, don't know why it works
;; (setq scroll-margin 0
;;       scroll-conservatively 100000
;;       scroll-preserve-screen-position 1)

;;; Disable the beep sound

(setq-default visible-bell t)
(setq ring-bell-function (lambda () ()))

;;; Fringe

(set-fringe-mode '(10 . 10))
(add-to-list 'default-frame-alist '(width . 83))

;;; Global default font

;;(set-face-attribute 'default nil :height 150)

;;; y-or-n-p

(defalias 'yes-or-no-p 'y-or-n-p)

;;; scroll preserve screen position

(setq scroll-preserve-screen-position 'always)

;;; Ido interface

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-file-extensions-order '(".rb" ".coffee" ".js" ".erb"
                                  ".c" ".html" ".md" ".py" ".el" ".es6"))
(setq ido-ignore-files '(".DS_Store" "*.scssc"))
(setq ido-use-filename-at-point nil) ;; do not guess my taste
(setq ido-save-directory-list-file
      (f-expand "ido.hist" eide-etc-dir))
(setq ido-auto-merge-work-directories-length -1)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations
      (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]"
              " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable
                                            'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

(ido-mode t)
(ido-ubiquitous-mode)


;; smex

(eval-after-load "smex"
  '(progn
     (setq smex-save-file (f-expand ".smex-items" eide-etc-dir))))

;;; guide key

(require 'guide-key)
(setq guide-key/popup-window-position 'bottom)
(setq guide-key/guide-key-sequence
      '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n"
        "C-x C-r" "C-z" "C-z g" "C-z p" "C-z q" "C-z o"))
(guide-key-mode 1)

;;; discover

(require 'discover)
(global-discover-mode)

;; helm
(require 'helm)
(require 'helm-config)
;; (helm-autoresize-mode)
(global-set-key (kbd "s-i") 'helm-mini)

(setq helm-M-x-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-imenu-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)

;; Calendar

(require 'calfw)

;;; themes

(if window-system
    (load-theme 'eide t)
  (load-theme 'wombat t))

;;; Line number

;; disable linum function
(defun hy-core-ui-linumoff ()
  (linum-mode -1))

;; format
(setq-default  linum-format "%2d ")

;; disable in non-programming modes
(eval-when-compile (require 'linum-off))
(setq linum-diabled-modes-list
      '(eshell-mode compilation-mode org-mode help-mode dired-mode
                    doc-view-mode image-mode comint-mode))

;; fix for image mode (linum-off doesn't work here)
(add-hook 'image-mode-hook 'hy-core-ui-linumoff)

;; global line number
(global-linum-mode)
(add-hook 'linum-mode-hook 'hlinum-activate)
(eval-when-compile (require 'hlinum))

;; show column number at the status bar
(column-number-mode)

;;; Highlight current line

(global-hl-line-mode)

(defun eide-cycle-confs ()
  "Visit user init file"
  (interactive)
  (let ((current-file-name (buffer-file-name (current-buffer))))
    (cond ((string= current-file-name user-init-file)
           (dired eide-conf-dir))
          (t (find-file user-init-file)))))

(defun eide-visit-conf-readme ()
  "Go to user readme file."
  (interactive)
  (find-file (f-expand "README.org" user-emacs-directory)))
(global-set-key [M-f11] 'eide-visit-conf-readme)

;; Do not show minor modes
(setq mode-line-modes
      (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
        (list (propertize "%[" 'help-echo recursive-edit-help-echo)
              "("
              `(:propertize ("" mode-name)
                            help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                            mouse-face mode-line-highlight
                            local-map ,mode-line-major-mode-keymap)
              '("" mode-line-process)

              (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
                          'mouse-face 'mode-line-highlight
                          'local-map (make-mode-line-mouse-map
                                      'mouse-2 #'mode-line-widen))
              ")"
              (propertize "%]" 'help-echo recursive-edit-help-echo)
              " "
              `(:propertize eide--current-editing-mode face font-lock-constant-face)
              " ")))

;; Weather forecast with sunshine

(provide 'eide-ui)
