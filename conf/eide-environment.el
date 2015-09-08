(if (fboundp 'mac-next-buffer)
    (defconst emacs-distribution "emacs-mac")
  (defconst emacs-distribution "gnu-emacs"))

(provide 'eide-environment)
