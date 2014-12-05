;; Delete the current file
(defun delete-this-file-and-kill-this-buffer ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited."))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Rename the current file and buffer
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name:")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' is already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name))))))

;; Test for symlink and maybe visit target file instead.
(defun test-for-symlink ()
  "This function is added to `find-file-hook'.
If the file is symlink, prompt user to visit target instead."
  (let ((target-file (file-symlink-p buffer-file-name)))
    (if target-file
        (progn
          (setq buffer-read-only t)
          (when (yes-or-no-p (format "This file is a symlink.
Visit target '%s' instead?" target-file))
            (find-alternate-file target-file))))))
(add-hook 'find-file-hook 'test-for-symlink)

;; Find user init file.
(defun find-user-init-file ()
  "Go to emacs init file."
  (interactive)
  (find-file user-init-file))

;; A handier way to kill all buffers.
(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(provide 'buffer)
