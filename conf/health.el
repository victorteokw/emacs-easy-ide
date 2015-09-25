;; Life and health, family and oxygen.

(defgroup health nil
  "Health timer."
  :group nil
  :prefix "health-")

(defcustom health-session-length 10
  "The health coding session length in minutes."
  :group 'health
  :type 'number)

(defcustom health-time-over-sound nil
  "The sound to play when health coding session is over."
  :group 'health
  :type 'string)

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
      (progn
        (health-stop-counting)
        (health-play-sound-if-needed))))

(defun health-overdue ()
  "Return true if time overdue."
  (and health-time-left (time-less-p health-time-left health-zero-time)))

(defun health-stop-counting ()
  "Stop health counting."
  (and health-timer (cancel-timer health-timer)))

(defun health-play-sound-if-needed ()
  "Play sound."
  (if health-time-over-sound (play-sound-file health-time-over-sound)))

(defun health-go-to-work ()
  "Start count for 10 seconds to rest."
  (interactive)
  (setq health-start-time (current-time))
  (set-health-end-time-by-minutes health-session-length)
  (setq health-zero-time (seconds-to-time 0))
  (setq health-time-left nil)
  (setq health-timer nil)
  (health-stop-counting-if-needed)
  (health-start-counting))

(defun health-change-time-interval (minutes)
  "Change the health interval."
  (interactive "nChange to how long: ")
  (set-health-end-time-by-minutes minutes))

(defun set-health-end-time-by-minutes (minutes)
  "Count from `health-start-time', recalculate `health-end-time'."
  (setq health-end-time
        (time-add health-start-time
                  (seconds-to-time (* 60 minutes)))))

(provide 'health)
