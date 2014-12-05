(defun mou ()
  "Mou on current md file."
  (interactive)
  (dired-do-shell-command 'open -a Mou ' nil)
  )
  ;; (let ((file-name (dired-file-name-at-point)))
  ;;   (shell-command (format "open -a Mou"))))


