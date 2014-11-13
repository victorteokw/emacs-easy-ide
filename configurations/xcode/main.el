
(if (eval-when-compile (eq system-type 'darwin))
    (progn
      
      (defvar ios-frameworks
	"/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/\
Developer/SDKs/iPhoneOS.sdk/System/Library/Frameworks"
	"Where to find iOS Frameworks.")

      (defun ios-simulator ()
	"Open iOS Simulator."
	(interactive)
	(shell-command "open -a \"iOS Simulator\"" nil nil))

      (defun xcode ()
	"Open Xcode."
	(interactive)
	(shell-command "open -a Xcode" nil nil))
      
      (defun itunes ()
	"Open iTunes."
	(interactive)
	(shell-command "open -a iTunes" nil nil))
      
      (defun omni-focus ()
	"Open OmniFocus."
	(interactive)
	(shell-command "open -a OmniFocus" nil nil))

      (defun evernote ()
	"Open Evernote."
	(interactive)
	(shell-command "open -a Evernote" nil nil))

      (defun connect-vpn ()
	"Connect VPN."
	(interactive)
	(shell-command "
osascript <<-EOF
tell application \"System Events\"
tell current location of network preferences
set VPN to service \"VPN\"
if exists VPN then connect VPN
repeat while (current configuration of VPN is not connected)
delay 1
end repeat
end tell
end tell
EOF
" nil nil))

      (defun disconnect-vpn ()
	"Disconnect VPN."
	(interactive)
	(shell-command "
osascript <<-EOF
tell application \"System Events\"
tell current location of network preferences
set VPN to service \"VPN\"
if exists VPN then disconnect VPN
end tell
end tell
return
EOF
" nil nil))
      ))


(defun setup-objc ()
  "Setup Objective C development."
  ;; Yasnippet
  (require-package 'yasnippet)
  (yas-global-mode 1)

  ;; Auto-complete
  (require-package 'auto-complete)
  (require 'auto-complete-config)
  (setq-default ac-sources '(ac-source-yasnippet
			     ac-source-abbrev
			     ac-source-dictionary
			     ac-source-words-in-same-mode-buffers))
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)

  (add-to-list 'ac-modes 'objc-mode)
  )

(add-hook 'c-mode-common-hook 'setup-objc)

(provide 'init-xcode)
