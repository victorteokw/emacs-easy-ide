(defvar eide-normal-editing-modes '("(E)" "(V)"))

(defvar eide-temp-editing-mode "(A)")

(defvar-local eide--current-editing-mode
  "(E)")
(put 'eide--current-editing-mode 'risky-local-variable t)

(defvar-local eide-mode-line-editing-mode
  `(eide-current-editing-mode)
  "Editing mode info for mode line use.")
(put 'eide-mode-line-editing-mode 'risky-local-variable t)

(defvar-local eide-current-normal-editing-mode "(E)")

(defun eide-current-editing-mode ()
  "Return current editing mode."
  eide--current-editing-mode)

(defun eide-editing-mode-open-emacs ()
  (setq eide--current-editing-mode "(E)")
  (setq eide-current-normal-editing-mode "(E)")
  (force-mode-line-update)
  (message "Emacs editing mode"))

(defun eide-editing-mode-open-vim ()
  (require 'evil)
  (turn-on-evil-mode)
  (setq eide--current-editing-mode "(V)")
  (setq eide-current-normal-editing-mode "(V)")
  (force-mode-line-update)
  (message "Vim editing mode"))

(defun eide-editing-mode-open-ace ()
  (setq eide--current-editing-mode "(A)")
  (force-mode-line-update)
  (message "Ace editing mode"))

(defun eide-editing-mode-close-emacs ())

(defun eide-editing-mode-close-vim () (turn-off-evil-mode))

(defun eide-editing-mode-close-ace ())

(defun eide-toggle-editing-mode ()
  "Toggle editing mode."
  (interactive)
  (if (string= eide--current-editing-mode eide-temp-editing-mode)
      (cond ((string= eide-current-normal-editing-mode "(E)")
             (eide-editing-mode-close-ace)
             (eide-editing-mode-open-vim))
            ((string= eide-current-normal-editing-mode "(V)")
             (eide-editing-mode-close-ace)
             (eide-editing-mode-open-emacs)))
    (cond ((string= eide-current-normal-editing-mode "(E)")
           (eide-editing-mode-close-emacs)
           (eide-editing-mode-open-ace))
          ((string= eide-current-normal-editing-mode "(V)")
           (eide-editing-mode-close-vim)
           (eide-editing-mode-open-ace)))))

(provide 'eide-editing-mode)
