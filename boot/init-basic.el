;; This file is to be deleted

(defun scroll-other-window-n-lines-ahead (&optional n)
  "Scroll other window N lines ahead"
  (interactive "P")
  (scroll-other-window (prefix-numeric-value n)))
(global-set-key "\C-\M-z" 'scroll-other-window-n-lines-ahead)

(defun point-to-top ()
  "Put point on top line of window."
  (interactive)
  (move-to-window-line 0))
;(global-set-key "\C-," 'point-to-top)

(add-hook 'find-file-hook
          '(lambda ()
             (if (file-symlink-p buffer-file-name)
                 (progn
                   (setq buffer-read-only t)
                   (message "File is a symlink")))))
(defun visit-target-instead ()
  "Replace this buffer with a buffer visiting the link target."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
        (if target
            (find-alternate-file target)
          (error "Not visiting a symlink")))
    (error "Not visiting a file")))
(defun clobber-symlink ()
  "Replace symlink with a copy of the file."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
        (if target
            (if (yes-or-no-p (format "Replace %s with %s? "
                                     buffer-file-name
                                     target))
                (progn
                  (delete-file buffer-file-name)
                  (write-file buffer-file-name)))
          (error "Not visiting a symlink")))
    (error "Not visiting a file")))

(defadvice switch-to-buffer (before existing-buffer activate compile)
  "When interactive, switch to existing buffers only, unless given a prefix argument."
  (interactive (list (read-buffer "Switch to buffer: "
                                  (other-buffer)
                                  (null current-prefix-arg)))))

(defvar unscroll-point (make-marker) "Cursor position for next call to 'unscroll'.")
(defvar unscroll-window-start (make-marker) "Window start for next call to 'unscroll'.")
(defvar unscroll-hscroll nil "Hscroll for next call to 'unscroll'.")
(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)
(put 'scroll-right 'unscrollable t)
(defun unscroll-maybe-remember ()
  (if (not (get last-command 'unscrollable))
      (progn
        (set-marker unscroll-point (point))
        (set-marker unscroll-window-start (window-start))
        (setq unscroll-hscroll (window-hscroll)))))

(defadvice scroll-up (before remember-for-unscroll activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-down (before remember-for-unscroll activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-left (before remember-for-unscroll activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-right (before remember-for-unscroll activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defun unscroll ()
  "Revert to 'unscroll-point' and 'unscroll-window-start'."
  (interactive)
  (if (not unscroll-point)
      (error "Cannot unscroll yet"))
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))

(provide 'init-basic)
;;; init-basic.el ends here
