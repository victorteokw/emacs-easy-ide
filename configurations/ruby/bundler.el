(require-package 'bundler)

;; Fix bundle-bug
(defun bundle-gem-location (gem-name)
  "Returns the location of the given gem, or 'no-gemfile if the
Gemfile could not be found, or nil if the Gem could not be
found."
  (let ((bundler-stdout
         (shell-command-to-string
          (format "bundle show %s" (shell-quote-argument gem-name)))))
    (cond
     ((string-match "Could not locate Gemfile" bundler-stdout)
      'no-gemfile)
     ((string-match "Could not find " bundler-stdout)
      nil)
     (t
      (concat (replace-regexp-in-string
               "Resolving dependencies...\\|\n" ""
               bundler-stdout)
              "/")))))
