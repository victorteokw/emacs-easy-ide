(defvar hoiyu/c-mac-include-directories
  (list "/usr/include" "/usr/local/include"
        "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefau\
lt.xctoolchain/usr/bin/../lib/clang/6.1.0/include"
        "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefau\
lt.xctoolchain/usr/include")
  "Mac OS X C include directories.")

(defun hoiyu/c-mode-setup ()
  "Setup for C mode."
  ;; Electric pair mode
  (electric-pair-mode)

  ;; Clear default auto complete sources
  (setq ac-sources '())

  ;; clang-async auto complete
  (require 'auto-complete-clang-async)
  (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
  (add-to-list 'ac-sources 'ac-source-clang-async)
  (ac-clang-launch-completion-process)

  ;; c headers completion
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (setq achead:include-directories hoiyu/c-mac-include-directories)

  ;; start auto complete
  (auto-complete-mode)

  ;; Snippets
  (yas-minor-mode-on)

  ;; Snippets completion
  (add-to-list 'ac-sources 'ac-source-yasnippet)

  ;; Syntax checking
  (flycheck-mode)

  ;; Man page
  (local-set-key (kbd "s-M") 'helm-man-woman)

  ;; iedit
  (local-set-key (kbd "C-c ;") 'iedit-mode)

  ;; semantic
  (semantic-mode)
  (semantic-idle-scheduler-mode 1)
  (add-to-list 'ac-sources 'ac-source-semantic)

  ;; irony
  (irony-mode)
  )

(add-hook 'c-mode-common-hook 'hoiyu/c-mode-setup)

;; (global-ede-mode 1)
;; (ede-cpp-root-project "my project"
;;                       :file "~/sss.cpp"
;;                       :include-path '("/../my_inc"))

(provide 'c-bundle)
