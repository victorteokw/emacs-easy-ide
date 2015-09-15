(if (fboundp 'mac-next-buffer)
    (defconst emacs-distribution "emacs-mac")
  (defconst emacs-distribution "gnu-emacs"))

(defvar-local eide--current-editing-mode
  "(E)")
(put 'eide--current-editing-mode 'risky-local-variable t)

(defun eide-current-editing-mode ()
  "Return current editing mode."
  eide--current-editing-mode)

(defvar-local eide-mode-line-editing-mode
  `(eide-current-editing-mode)
  "Editing mode info for mode line use.")
(put 'eide-mode-line-editing-mode 'risky-local-variable t)

(defvar-local eide-evil-mode-is-on nil)

(defun eide-toggle-editing-mode ()
  "Toggle editing mode."
  (interactive)
  (if eide-evil-mode-is-on
      (progn
        (require 'evil)
        (turn-off-evil-mode)
        (setq eide--current-editing-mode "(E)")
        (setq eide-evil-mode-is-on nil)
        (force-mode-line-update))
    (require 'evil)
    (turn-on-evil-mode)
    (setq eide--current-editing-mode "(V)")
    (setq eide-evil-mode-is-on t)
    (force-mode-line-update)))

(provide 'eide-environment)
