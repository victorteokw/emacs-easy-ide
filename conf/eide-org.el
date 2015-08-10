(eval-after-load "org"
  '(progn
     (setq org-id-locations-file (f-expand ".org-id-locations" eide-etc-dir))

     ;; default org directory
     (setq org-directory (expand-file-name "org" user-emacs-directory))

     ;; archive directory
     (setq org-archive-location (expand-file-name "archives" org-directory))

     ;; special ctrl-a/e behavior in some circumstance
     (setq org-special-ctrl-a/e t)

     ;; unfinished children block state changes in the parent
     (setq org-enforce-todo-dependencies t)

     ;; unfinished checkboxes block state changes in the parent
     (setq org-enforce-todo-checkbox-dependencies t)

     ;; org log done
     (setq org-log-done 'time)

     ;; default notes file
     (setq org-default-notes-file
           (expand-file-name "default.org" org-directory))

     ;; mobile org
     (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
     (setq org-mobile-inbox-for-pull
           (expand-file-name "mobile_inbox.org" org-directory))
     ))


(provide 'eide-org)
