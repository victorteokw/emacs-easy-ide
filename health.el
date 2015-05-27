;; Life and health, family and oxygen.

(defgroup health nil
  "Health timer."
  :group nil
  :prefix "health-")

(defcustom health-session-length 10
  "The health coding session length in minutes."
  :group 'health
  :type 'number)

(defvar health-start-time nil
  "The counting start of health coding session.")

(defvar health-end-time nil
  "The expected end of health coding session.")

(defvar health-zero-time nil
  "The helper variable which means 0 seconds.")

(defvar health-time-left nil
  "The seconds in time format lefted for the current health coding session.")

(defvar health-timer nil
  "The health timer.")

(defun health-time-mode-line ()
  "Return the mode line formatted health time."
  (if (null health-time-left)
      ""
    (if (health-overdue)
        "Do some exercise please!"
      (format-time-string "%M:%S" health-time-left))))

(defun health-count ()
  "Update `health-time-left' variable periodically."
  (setq health-time-left (time-subtract health-end-time (current-time)))
  (force-mode-line-update t)
  (health-stop-counting-if-needed))

(defun health-start-counting ()
  "Run health count timer."
  (setq health-timer nil)
  (setq health-time-left nil)
  (setq health-timer (run-with-timer 0 1 'health-count)))

(defun health-stop-counting-if-needed ()
  "Stop health counting if needed."
  (if (health-overdue)
      (health-stop-counting)))

(defun health-overdue ()
  "Return true if time overdue."
  (and health-time-left (time-less-p health-time-left health-zero-time)))

(defun health-stop-counting ()
  "Stop health counting."
  (and health-timer (cancel-timer health-timer)))

(defun health-go-to-work ()
  "Start count for 10 seconds to rest."
  (interactive)
  (setq health-start-time (current-time))
  (setq health-end-time
        (time-add health-start-time
                  (seconds-to-time (* 60 health-session-length))))
  (setq health-zero-time (seconds-to-time 0))
  (setq health-time-left nil)
  (setq health-timer nil)
  (health-stop-counting-if-needed)
  (health-start-counting))

(provide 'health)
