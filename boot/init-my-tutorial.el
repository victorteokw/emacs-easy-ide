(defun insert-date ()
  "Insert the current time"
  (interactive "*")
  (insert (current-time-string)))

(defun insert-time ()
  "Insert the current am or pm"
  (interactive "*")
  (insert (format-time-string "%l:%M %p" (current-time))))



(provide 'init-my-tutorial)
;;; init-my-tutorial.el ends here
