;;;; Emacs lisp

;;; .el or .elc just loading new one

(setq load-prefer-newer t)

;;; open Cask file in elisp-mode

(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))

;;; expand macro

;; (require 'macrostep)

;;; Code folding

(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;;; Jump to definition

(defun eide-elisp-click-to-jump (event)
  "Click to jump to definition."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (goto-char (posn-point (event-start event)))
  (elisp-slime-nav-find-elisp-thing-at-point (elisp-slime-nav--read-symbol-at-point)))

(require 'elisp-slime-nav)

(define-key elisp-slime-nav-mode-map (kbd "s-<down-mouse-1>") 'eide-elisp-click-to-jump)

(add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)

;;; Rainbow delimiters

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;;; Paredit mode, the editing mode

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(require 'paredit)

(define-key paredit-mode-map (kbd "M-<left>") 'paredit-wrap-sexp)
(define-key paredit-mode-map (kbd "M-<right>") 'paredit-wrap-sexp)

;;; Delete should react to region

(defadvice paredit-backward-delete (around eide-paredit-delete-region activate)
  (if mark-active
      (delete-active-region)
    ad-do-it))

(defadvice paredit-forward-delete (around eide-paredit-delete-region activate)
  (if mark-active
      (delete-active-region)
    ad-do-it))

;;; Delete should react to superword mode

(defadvice paredit-backward-kill-word (around eide-paredit-kill-superword activate)
  (eide-alter-function
      'backward-kill-word 'subword-backward-kill
      ad-do-it))

(defadvice paredit-forward-kill-word (around eide-paredit-kill-superword activate)
  (eide-alter-function
      'kill-word 'subword-kill
      ad-do-it))

;;; Aggressive indent

(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

;;; Snippets

(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)

;;; Display documentation in mini buffer

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;; Show paren mode

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;;; Auto complete

(defun eide-arglist-to-string (arglist)
  "Return ARGLIST as a string enclosed by ().
ARGLIST is either a string, or a list of strings or symbols.

This function is copied from eldoc."
  (cond ((stringp arglist))
        ((not (listp arglist))
         (setq arglist nil))
        ((symbolp (car arglist))
         (setq arglist
               (mapconcat (lambda (s) (symbol-name s))
                          arglist " ")))
        ((stringp (car arglist))
         (setq arglist
               (mapconcat (lambda (s) s)
                          arglist " "))))
  (if arglist
      (format "(%s)" arglist)))

(defun eide-arglist-str-of-sym (sym)
  "Return arglist of sym"
  (let (arglist should-strip-fun-name)
    (setq should-strip-fun-name t)
    (setq arglist
          (or (car (help-split-fundoc (documentation sym t) sym))
              (help-function-arglist sym)))
    (when (listp arglist)
      (setq arglist (eide-arglist-to-string arglist))
      (setq should-strip-fun-name nil))
    (setq arglist
          (replace-regexp-in-string "^(" "" arglist))
    (setq arglist
          (replace-regexp-in-string ")$" "" arglist))
    (if should-strip-fun-name
        (setq arglist
              (replace-regexp-in-string "^[a-zA-Z0-9-_]*" "" arglist)))
    (setq arglist
          (replace-regexp-in-string "^ *" "" arglist))
    arglist))

(defun eide-process-to-snippet (arglist)
  "Process arglist into snippet."
  (let ((index 1) (tokens (s-split " " arglist)) (retval ""))
    (dolist (token tokens)
      (setq retval (s-append (format " ${%s:%s}" index token) retval))
      (setq index (1+ index)))
    retval))

(defun eide-expand-sym ()
  (yas-expand-snippet
   (eide-process-to-snippet
    (eide-arglist-str-of-sym (symbol-at-point)))))

(ac-define-source eide-functions
  '((candidates . ac-function-candidates)
    (document . ac-symbol-documentation)
    (action . eide-expand-sym)
    (symbol . "f")
    (prefix . "(\\(\\(?:\\sw\\|\\s_\\)+\\)")
    (cache)))

(defun eide-elisp-auto-complete ()
  (require 'auto-complete)
  (require 'auto-complete-config)
  (setq ac-ignore-case nil)
  (setq ac-sources '(ac-source-eide-yasnippet
                     ac-source-dictionary
                     ac-source-features
                     ac-source-eide-functions
                     ac-source-symbols
                     ac-source-variables
                     ac-source-words-in-same-mode-buffers))
  (auto-complete-mode))

(add-hook 'emacs-lisp-mode-hook 'eide-elisp-auto-complete)

;;; Check parens after save

(defun eide-elisp-check-parens ()
  (add-hook 'after-save-hook #'check-parens nil t))

(add-hook 'emacs-lisp-mode-hook 'eide-elisp-check-parens)

;;; Byte compile conf file after save

(defun eide-elisp-compile-this-conf-file ()
  (when (string= (f-dirname (buffer-file-name (current-buffer)))
                 eide-conf-dir)
    (byte-compile-file (buffer-file-name (current-buffer)))))

(defun eide-elisp-compile-after-save ()
  (add-hook 'after-save-hook 'eide-elisp-compile-this-conf-file nil t))

(add-hook 'emacs-lisp-mode-hook 'eide-elisp-compile-after-save)

;;; REPL

(defvar eide-elisp-repl-last-buffer ()
  nil)

(defadvice ielm (before eide-remember-last-file activate)
  (setq eide-elisp-repl-last-buffer (current-buffer)))

(defun eide-elisp-back-to-file ()
  (interactive)
  (switch-to-buffer-other-window eide-elisp-repl-last-buffer))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'ielm)
(eval-after-load "ielm"
  '(define-key ielm-map (kbd "C-c C-z") 'eide-elisp-back-to-file))

(defadvice ielm (after eide-elisp-repl-nice-behavior activate)
  (previous-buffer)
  (pop-to-buffer "*ielm*"))

;;; Flycheck elisp

(defun eide-elisp-flycheck ()
  (flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(add-hook 'emacs-lisp-mode-hook 'eide-elisp-flycheck)

;;; eshell

(eval-after-load
    "eshell"
  '(setq eshell-directory-name (f-expand "etc/eshell" eide-etc-dir)))

(provide 'eide-elisp)
