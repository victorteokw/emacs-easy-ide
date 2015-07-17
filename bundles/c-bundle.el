;; c
(add-hook 'c-mode-common-hook
          (lambda ()
            (electric-pair-mode)
            ;; Auto complete main
            (require 'auto-complete-clang-async)
            (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
            (setq ac-sources '(ac-source-clang-async))
            (ac-clang-launch-completion-process)

            ;; Auto complete c headers
            (require 'auto-complete-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (setq achead:include-directories
                  (list "/usr/include" "/usr/local/include"
                        "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/6.1.0/include"
                        "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"))
            (auto-complete-mode)

            ;; Snippets
            (yas-minor-mode-on)

            ;; Syntax checking
            (flycheck-mode)
            ))

(global-set-key (kbd "s-;") 'comment-dwim)

(provide 'c-bundle)
