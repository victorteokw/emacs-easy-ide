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

;; See http://httpd.apache.org/docs/2.4/configuring.html
(defun test-apache-config-file ()
  "Test apache config file."
  (interactive)
  (shell-command "apachectl configtest") ;; or apachectl -k
  (switch-to-buffer "*Shell Command Output*"))

