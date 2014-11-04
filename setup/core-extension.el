
;; Handier way to add modes to auto-mode-alist.
(defun auto-major-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE'
for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; return all matches from string
(defun string-all-matches (regex str &optional group)
  "Find all match for `REGEX' within `STR', returning the full
match string or group `GROUP'."
  (let ((result nil) (pos 0) (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

;; This variable alias is for compatibility.
;; Some ruby package may use this.
(unless (boundp 'last-command-char)
  (defvaralias 'last-command-char 'last-command-event))

;; Use this constant to determine if this computer is a mac.
(defconst this-computer-is-mac (eq system-type 'darwin)
  "Use this constant to determine if this computer is a mac")
(defvaralias 'this-computer-is-mac 'current-operating-system-is-os-x)
(defvaralias 'this-computer-is-mac 'operating-system-is-os-x)

(provide 'core-extension)
