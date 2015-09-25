;;; TODO: Make this not depends on OS X
(defvar eide-mac-include-directories
  (list "/usr/include" "/usr/local/include"
        "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefau\
lt.xctoolchain/usr/bin/../lib/clang/6.1.0/include"
        "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefau\
lt.xctoolchain/usr/include")
  "Mac OS X C include directories.")

;;; Electric pair

(add-hook 'c-mode-common-hook 'electric-pair-mode)

;;; irony

(add-hook 'c-mode-common-hook 'irony-mode)

;;; Auto complete

;; (defun eide-c-auto-complete ()
;;   (setq ac-sources nil)
;;   (require 'auto-complete-clang-async)
;;   (setq ac-clang-complete-executable "~/.emacs.d/bin/clang-complete")
;;   (add-to-list 'ac-sources 'ac-source-clang-async)
;;   (ac-clang-launch-completion-process)
;;   (require 'auto-complete-c-headers)
;;   (add-to-list 'ac-sources 'ac-source-c-headers)
;;   (setq achead:include-directories eide-mac-include-directories)
;;   (add-to-list 'ac-sources 'ac-source-yasnippet)
;;   (auto-complete-mode))

(defun eide-c-auto-complete ()
  (require 'company)
  (setq company-backends nil)
  (add-to-list 'company-backends 'company-yasnippet)
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-irony-c-headers)
  (company-mode))

(add-hook 'c-mode-common-hook 'eide-c-auto-complete)

;;; Snippets

(add-hook 'c-mode-common-hook 'yas-minor-mode-on)

;;; Syntax checking

(add-hook 'c-mode-common-hook 'flycheck-mode)

;;; Man page

(defun eide-c-man-page ()
  (local-set-key (kbd "s-M") 'helm-man-woman))

(add-hook 'c-mode-common-hook 'eide-c-man-page)

;;; iedit

(defun eide-iedit ()
  (local-set-key (kbd "C-c ;") 'iedit-mode))

;;; code folding

(add-hook 'c-mode-common-hook 'origami-mode)

(provide 'eide-c)
