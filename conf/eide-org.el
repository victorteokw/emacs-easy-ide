(eval-after-load "org"
  '(progn
     (setq org-id-locations-file (f-expand ".org-id-locations" eide-etc-dir))

     ;; default org directory
     (setq org-directory (expand-file-name "orgs" user-emacs-directory))

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

;; s-@ to org-capture
(global-set-key (kbd "s-@") 'org-capture)

;; s-! to cycle org files
(global-set-key (kbd "s-!") 'org-cycle-agenda-files)

;; s-% to open agenda menu
(global-set-key (kbd "s-%") 'org-agenda)

(provide 'eide-org)
