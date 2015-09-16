(when (functionp 'discover-add-context-menu)

  (defun eide-cm-project-submenu ()
    (interactive)
    (call-interactively
     (discover-get-context-menu-command-name 'eide-cm-project)))

  (defun eide-cm-cvs-submenu ()
    (interactive)
    (call-interactively
     (discover-get-context-menu-command-name 'eide-cm-cvs)))

  (defun eide-cm-open-submenu ()
    (interactive)
    (call-interactively
     (discover-get-context-menu-command-name 'eide-cm-open)))

  (defun eide-cm-search-web-submenu ()
    (interactive)
    (call-interactively
     (discover-get-context-menu-command-name 'eide-cm-search-web)))

  (defun eide-cm-config-submenu ()
    (interactive)
    (call-interactively
     (discover-get-context-menu-command-name 'eide-cm-config)))

  (discover-add-context-menu
   :context-menu '(eide-cm-c-z
                   (description "C-z commands")
                   (actions
                    ("Available"
                     ("o" "open in external app" eide-cm-open-submenu)
                     ("g" "CVS" eide-cm-cvs-submenu)
                     ("p" "project" eide-cm-project-submenu)
                     ("q" "search web" eide-cm-search-web-submenu)
                     ("c" "configuration" eide-cm-config-submenu))))
   :bind (kbd "C-z C-z"))

  (discover-add-context-menu
   :context-menu '(eide-cm-open
                   (description "Open in external app")
                   (actions
                    ("Available"
                     ("s" "open in Sublime Text" eide-open-with-sublime-text)
                     ("t" "open in TextMate" eide-open-with-textmate)
                     ("f" "open in Finder" eide-open-with-finder)
                     ("a" "open in Atom" eide-open-with-atom)
                     )))
   :bind "")

  (discover-add-context-menu
   :context-menu '(eide-cm-search-web
                   (description "Search web")
                   (actions
                    ("Available"
                     ("b" "Search in Bing" eide-search-bing)
                     ("g" "Search in Google" eide-search-google))))
   :bind "")

  (discover-add-context-menu
   :context-menu '(eide-cm-config
                   (description "Configuration")
                   (actions
                    ("Available"
                     ("c" "conf file" eide-goto-conf-file)
                     ("i" "init file" eide-goto-init-file)
                     ("k" "key bindings file" eide-goto-key-bindings-file))))
   :bind "")

  (discover-add-context-menu
   :context-menu '(eide-cm-cvs
                   (description "CVS")
                   (actions
                    ("Available"
                     ("b" "blame" magit-blame-popup)
                     ("g" "status" magit-status))))
   :bind ""))

(provide 'eide-context-menu)
