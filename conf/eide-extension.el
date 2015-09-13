(defun eide-goto-init-file ()
  (interactive)
  (find-file user-init-file))

(defun eide-goto-conf-file (file)
  (interactive
   (list
    (ido-completing-read
     "Goto conf file: "
     (-map
      (lambda (f) (s-replace "eide-" "" (f-no-ext (f-filename f))))
      (f-files eide-conf-dir (lambda (f) (f-ext? f "el")))))))
  (find-file (f-expand (format "eide-%s.el" file) eide-conf-dir)))

(defun eide-goto-key-bindings-file ()
  (interactive)
  (eide-goto-conf-file "key-bindings"))

(defun eide-get-filename ()
  "Not documented yet."
  (if (eq major-mode 'dired-mode)
      (dired-get-file-for-visit)
    buffer-file-name))

(defun eide-open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name (eide-get-filename))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (start-process "eide-open-with-process" nil program current-file-name)))

(defun eide-open-with-app (app)
  "Not documented yet."
  (interactive "sApplication: ")
  (let* ((cur-file-name (eide-get-filename))
         (command (pcase system-type
                    (`darwin "open")
                    ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (arg (pcase system-type
                (`darwin "-a"))))
    (start-process "eide-open-with-process"
                   nil command cur-file-name arg app)))

(defmacro eide-install-external-opener (name disp)
  "Not documented."
  `(defun ,(intern (format "eide-open-with-%s" name)) ()
     ,(format "Open file with %s" disp)
     (interactive)
     (eide-open-with-app ,disp)))

(eide-install-external-opener "sublime-text" "Sublime Text")
(eide-install-external-opener "textmate" "Textmate")
(eide-install-external-opener "atom" "Atom")
(eide-install-external-opener "finder" "Finder")

(defun eide-copy-string (string)
  "Copy string both emacs internally and externally with clipboard."
  (with-temp-buffer
    (insert string)
    (kill-ring-save (buffer-end -1) (buffer-end 1)))
  (if window-system
      (ns-set-pasteboard string)))

(defun eide-copy-filename ()
  "Not documented yet."
  (interactive)
  (eide-copy-string (eide-get-filename)))

(defun eide-copy-filename-and-linum ()
  "Not documented yet."
  (interactive)
  (let ((filename (eide-get-filename))
        (linumber (line-number-at-pos)))
    (eide-copy-string (format "%s:%s" filename linumber))))

(defun eide-search-web (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string' prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro eide-install-search-engine (name url prompt)
  "Given some information regarding a search engine,
install the interactive command to search through them."
  `(defun ,(intern (format "eide-search-%s" name)) ()
     ,(format "Search %s with a query or region if any." name)
     (interactive)
     (eide-search-web ,url ,prompt)))

(eide-install-search-engine "google" "http://www.google.com/search?q="
                            "Google: ")

(eide-install-search-engine "youtube"
                            "http://www.youtube.com/results?search_query="
                            "Youtube: ")

(eide-install-search-engine "github" "https://github.com/search?q="
                            "Github: ")

(eide-install-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="
                            "DuckDuckGo: ")

(eide-install-search-engine "bing" "https://www.bing.com/?q="
                            "Bing: ")

(eide-install-search-engine "baidu"
                            "https://www.baidu.com/baidu?tn=baidu&word="
                            "Baidu: ")

(defun eide-rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun eide-delete-file-and-buffer ()
  "Kill the current buffer and delete the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (yes-or-no-p (format "Are you sure you want to delete %s"
                                   filename))
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun eide-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

(defun eide-recursive-add-to-load-path (parent-dir)
  "Add all level PARENT-DIR sibdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (eide-recursive-add-to-load-path name)))))

(defun eide-kill-last-buffer ()
  "Kill last buffer."
  (interactive)
  (kill-buffer (car (last (buffer-list)))))

(provide 'eide-extension)
