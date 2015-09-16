(defvar eide-os
  (cond ((string-equal system-type "darwin")
         :osx)
        ((string-equal system-type "windows-nt")
         :windows)
        ((string-equal system-type "gnu/linux")
         :linux))
  "Current operating system.")

(defvar eide-dist
  (cond ((null window-system)
         :terminal)
        ((and (equal eide-os :osx) (fboundp 'mac-next-buffer))
         :emacs-mac)
        ((equal eide-os :osx)
         :gnu-emacs)
        ((equal eide-os :windows)
         :gnu-emacs)
        ((equal eide-os :linux)
         :gnu-emacs)))

(defmacro eide-only (os dist &rest body)
  (declare (indent 0))
  `(and (or (null ,os) (-contains? (-flatten ,os) eide-os))
        (or (null ,dist) (-contains? (-flatten ,dist) eide-dist))
        (progn
          ,@body)))

(provide 'eide-environment)
