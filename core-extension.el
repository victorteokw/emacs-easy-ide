

;; This variable alias is for compatibility.
;; Some ruby package may use this.
(unless (boundp 'last-command-char)
  (defvaralias 'last-command-char 'last-command-event))

;; Use this constant to determine if this computer is a mac
(defconst this-computer-is-mac (eq system-type 'darwin)
  "Use this constant to determine if this computer is a mac")
(defvaralias 'this-computer-is-mac 'current-operating-system-is-os-x)
(defvaralias 'this-computer-is-mac 'operating-system-is-os-x)

(provide 'core-extension)
