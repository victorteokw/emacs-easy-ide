(defun find-nginx-conf-file ()
  "Find nginx configuration file."
  (interactive)
  (find-file (nginx-value-for-conf-key "--conf-path")))

(defun find-nginx-http-log-file ()
  "Find nginx http log file."
  (interactive)
  (find-file (nginx-value-for-conf-key "--http-log-path")))

(defun find-nginx-error-log-file ()
  "Find nginx error log file."
  (interactive)
  (find-file (nginx-value-for-conf-key "--error-log-path")))

(defconst nginx-regexp
  "\\(--[^=\n[:space:]]+\\)=?\\('.*?'\\|[^\n[:space:]]*\\)"
  "The nginx configuration output regexp.")

(defun nginx-value-for-conf-key (key)
  "Return value for conf key."
  (cdr (assoc key nginx-configuration-alist)))

(defun nginx-configuration-alist-gen ()
  "Turn command 'nginx -V' into alist."
  (let ((ret-val (shell-command-to-string "nginx -V")))
    (if (string-match "command not found" ret-val)
        (error "Nginx not found."))
    (save-match-data
      (let ((pos 0) alist)
        (while (string-match nginx-regexp ret-val pos)
          (push (cons (match-string 1 ret-val)
                      (if (= 0 (length (match-string 2 ret-val)))
                          t
                        (match-string 2 ret-val)))
                alist)
          (setq pos (match-end 0)))
        alist))))

(defvar nginx-configuration-alist
  (nginx-configuration-alist-gen)
  "The nginx configuration alist.")

(provide 'init-nginx)
