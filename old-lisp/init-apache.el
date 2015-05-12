;; See http://wiki.apache.org/httpd/DistrosDefaultLayout

(defmacro apache--with-system (type &rest body)
  "Evaluate body if `system-type' equals type."
  `(when (eq system-type ,type)
     ,@body))

(defun find-apache-config-file ()
  "Go to apache config file."
  (interactive)
  (apache--with-system 'darwin
                       (find-file "/etc/apache2/httpd.conf"))
  (apache--with-system 'gnu/linux
                       (find-file "/etc/httpd/conf/httpd.conf")))

(defun apache--execute (command)
  "Execute command and jump to that buffer."
  (shell-command command)
  (switch-to-buffer "*Shell Command Output*"))

;; See http://httpd.apache.org/docs/2.4/configuring.html
(defun test-apache-config-file ()
  "Test apache config file."
  (interactive)
  ;; apachectl configtest or apachectl -k
  (apache--execute "apachectl configtest"))

(defun apache-show-static-modules ()
  "Show apache's modules that are currently compiled into the server."
  (interactive)
  (apache--execute "apachectl -l"))

(defun apache-show-dynamic-modules ()
  "Show apache's modules that are loaded dynaically."
  (interactive)
  (apache--execute "apachectl -M"))


(provide 'init-apache)
