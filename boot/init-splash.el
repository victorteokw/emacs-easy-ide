;;; init-splash.el --- loading startup screen -*- lexical-binding: t -*-

;; Maintainer: Zhang Kai Yu
;; Keywords: ide

;;; Commentary:

;;; This config load the startup screen for easy ide

;;; Code:

;; Disable tool bar
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))

;; Disable splash screen
(setq inhibit-splash-screen t)


(add-hook 'emacs-startup-hook 'show-initial-screen)
(defun show-initial-screen ()
  "Show the initial screen for the easy ide."
  (let ((ide-buffer (get-buffer-create "*IDE Start Up*")))
    (with-current-buffer ide-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq default-directory command-line-default-directory)
      (set (make-local-variable 'tab-width) 18)

      (insert "Welcome to Easy IDE, happy hacking!\n")

      (insert "\nSelect an IDE\n\n")

      (insert-button "C IDE"
                     'action (lambda (_button) (message "C IDE"))
                     'follow-link t)
      (insert (substitute-command-keys "\t   \\[info-emacs-manual]\t"))

      (insert-button "C++ IDE"
                     'action (lambda (_button) (message "C++ IDE"))
                     'follow-link t)
      (insert (substitute-command-keys "\t   \\[info]\n"))

      (insert-button "Ruby IDE"
                     'action (lambda (_button) (message "Ruby IDE"))
                     'follow-link t)
      (insert (substitute-command-keys "\t   \\[info]\t"))

      (insert-button "Python IDE"
                     'action (lambda (_button) (message "Python IDE"))
                     'follow-link t)
      (insert (substitute-command-keys "\t   \\[info]\n"))

      (insert-button "Javascript IDE"
		     'action (lambda (_button) (message "Python IDE"))
		     'follow-link t)
      (insert (substitute-command-keys "\t   \\[info]\t"))

      (insert-button "PHP IDE"
		     'action (lambda (_button) (message "PHP IDE"))
		     'follow-link t)
      (insert (substitute-command-keys "\t   \\[info-emacs-manual]\n"))

      (insert-button "Swift and Objective-C IDE"
		     'action (lambda (_button) (message "Swift and Objective-C IDE"))
		     'follow-link t)
      (insert (substitute-command-keys "\t   \\[info-emacs-manual]\n"))

      (insert-button "Lisp IDE"
		     'action (lambda (_button) (message "Lisp IDE"))
		     'follow-link t)
      (insert (substitute-command-keys "\t   \\[info-emacs-manual]\n"))

      ;; Help for this easy ide
      (insert (format "\nHelp:\t   %s\n"
                      (let ((where (where-is-internal 'help-command nil t)))
                        (cond
                         ((equal where [?\C-h])
                          "C-h (Hold down CTRL and press h)")
                         (where (key-description where))
                         (t "M-x help")))))

      (insert "\nCurrently, only Ruby IDE is implemented.\n")

      (setq buffer-read-only t)

      (switch-to-buffer ide-buffer))))

(defun show-ruby-initial-screen ()
  "show the ruby initial screen for the easy ide"
  (let ((ruby-buffer (get-buffer-create "Welcome to Ruby IDE")))
    (with-current-buffer ruby-buffer
      (insert-button "Create a project" 'action (lambda (x) (find-file user-init-file)))
      (insert "\n")
      (insert "Visit a last opened project\n")
      (insert-button "Old project" 'action (lambda (x) (find-file user-init-file)))
      (insert "\n")
      (insert-button "Old project" 'action (lambda (x) (find-file user-init-file)))
      (insert "\n")
      (insert-button "Old project" 'action (lambda (x) (find-file user-init-file)))
      (insert "\n")
      (insert-button "Old project" 'action (lambda (x) (find-file user-init-file)))
      (insert "\n")
      (insert-button "Open Another project" 'action (lambda (x) (find-file user-init-file)))
      (insert "\n")
      (insert-button "Run the shell" 'action (lambda (x) (find-file user-init-file)))
      (insert "\n")
      (switch-to-buffer ruby-buffer))))

(provide 'init-splash)

;;; init-splash.el ends here
