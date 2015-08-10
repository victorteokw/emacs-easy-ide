;;; Mode line

(setq mode-line-modes
      (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
        (list (propertize "%[" 'help-echo recursive-edit-help-echo)
              "("
              `(:propertize ("" mode-name)
                            help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                            mouse-face mode-line-highlight
                            local-map ,mode-line-major-mode-keymap)
              '("" mode-line-process)

              (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
                          'mouse-face 'mode-line-highlight
                          'local-map (make-mode-line-mouse-map
                                      'mouse-2 #'mode-line-widen))
              ")"
              (propertize "%]" 'help-echo recursive-edit-help-echo)
              " ")))

(make-face 'octicons)
(set-face-attribute 'octicons nil
                    :family "octicons")
(make-face 'octicons-mode-line)
(set-face-attribute 'octicons-mode-line nil
                    :inherit 'mode-line
                    :inherit 'octicons)

(set-face-attribute 'octicons nil
                    :family "octicons")

(require 'magit-git)

(setq eide-mode-line-buffer-name
      '(" "
        (:propertize
         (:eval
          (let ((bname (buffer-name))
                (bfname (or (buffer-file-name)
                            (and (eq major-mode 'dired-mode)
                                 (dired-current-directory)))))
            (if (string-match-p "^\\*.*\\*$" bname)
                (propertize " %14b" 'face 'octicons-mode-line)
              (if (f-directory? bfname)
                  (propertize " %14b" 'face 'octicons-mode-line)
                (propertize " %14b" 'face 'octicons-mode-line)))
            )) face font-lock-type-face)))
(put 'eide-mode-line-buffer-name 'risky-local-variable t)
(make-variable-buffer-local 'eide-mode-line-buffer-name)
(setq eide-mode-line-vcs
      '(" "
        (:propertize
         (:eval
          (let ((is-repo (magit-inside-worktree-p))
                (remote-url (car (magit-git-lines "ls-remote" "--get-url")))
                (branch (magit-get-current-branch)))
            (if is-repo
                (if (and (stringp remote-url)
                         (string-match-p "github" remote-url))
                    (concat "" " " branch)
                  (concat "" " " branch))
              (propertize " " 'face font-lock-variable-name-face))))
         face font-lock-variable-name-face)))
(put 'eide-mode-line-vcs 'risky-local-variable t)
(make-variable-buffer-local 'eide-mode-line-vcs)

(setq-default mode-line-format '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification eide-mode-line-buffer-name     mode-line-position eide-mode-line-vcs " " mode-line-modes mode-line-misc-info mode-line-end-spaces))
