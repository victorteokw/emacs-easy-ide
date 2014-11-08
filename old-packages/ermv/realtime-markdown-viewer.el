;;; realtime-markdown-viewer.el ---

;; Copyright (C) 2012 by Wataru MIYAGUNI

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; URL: https://github.com/gongo/emacs-realtime-markdown-viewer
;; Keywords: websocket markdown
;; Version: 0.0.1
;; Package-Requires: ((websocket "1.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-realtime-markdown-viewer

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'websocket)

(defgroup realtime-markdown-viewer nil
  "Realtime Markdown Viewer"
  :group 'text
  :prefix "rtmv:")

(defconst rtmv:process-name "rtmv")

(defcustom rtmv:port 5021
  "Port number for Plack App"
  :type 'integer
  :group 'realtime-markdown-viewer)

(defvar rtmv:websocket nil)

(defun rtmv:init-websocket (port)
  (let ((url (format "ws://0.0.0.0:%d/emacs" port)))
    (message "Connect to %s" url)
    (setq rtmv:websocket
          (websocket-open
           url
           :on-message (lambda (websocket frame)
                         (message "%s" (websocket-frame-payload frame)))
           :on-error (lambda (ws type err)
                       (message "error connecting"))))))

(defun rtmv:send-to-server (beg end len)
  (when realtime-markdown-viewer-mode
    (let ((str (buffer-substring-no-properties (point-min) (point-max))))
      (websocket-send-text rtmv:websocket str))))

(defvar rtmv:webapp-process nil)

(defvar rtmv:server-file "realtime_markdown_viewer.rb"
  "File name of Realtime markdown viewer app")

(defvar rtmv:webapp-root
  (when load-file-name
    (file-name-directory load-file-name))
  "WebApp full path")

(defun rtmv:webapp-launch-command (port)
  (format "cd %s && bundle exec ruby %s -p %d" rtmv:webapp-root rtmv:server-file port))

(defun rtmv:webapp-launch (port)
  (setq rtmv:webapp-process (get-process rtmv:process-name))
  (when (null rtmv:webapp-process)
    (let ((cmd (rtmv:webapp-launch-command port)))
      (setq rtmv:webapp-process
            (start-process-shell-command rtmv:process-name
                                         "*realtime markdown*" cmd)))))

(defun rtmv:webapp-stop ()
  (when rtmv:webapp-process
    (kill-process rtmv:webapp-process)
    (setq rtmv:webapp-process nil)))

(defun rtmv:init ()
  "rtmv に関連する状態(websocket,hook)をカレントバッファにセットする。
もし全てのバッファで websocket が閉じられているのであれば
webapp も stop する。"
  (let ((port rtmv:port))
    (make-local-variable 'rtmv:websocket)
    (rtmv:webapp-launch port)
    (sleep-for 3)
    (rtmv:init-websocket port)
    (when (fboundp 'make-local-hook) ;; for Emacs 23.x or lower
      (make-local-hook 'kill-buffer-hook)
      (make-local-hook 'after-change-functions))
    (add-hook 'kill-buffer-hook 'rtmv:finalize nil t)
    (add-hook 'after-change-functions 'rtmv:send-to-server nil t)))

(defun rtmv:finalize ()
  "rtmv に関連する状態(websocket,hook)をカレントバッファから削除する。
もし全てのバッファで websocket が閉じられているのであればwebapp も stop する。"
  (websocket-close rtmv:websocket)
  (remove-hook 'kill-buffer-hook 'rtmv:finalize t)
  (remove-hook 'after-change-functions 'rtmv:send-to-server t)
  (kill-local-variable 'rtmv:websocket)

  (let ((cnt 0))
    (dolist (b (buffer-list))
      (unless (null (buffer-local-value 'rtmv:websocket b))
        (setq cnt (1+ cnt))))
    (when (= cnt 0)
      (rtmv:webapp-stop))))

(define-minor-mode realtime-markdown-viewer-mode
  "Realtime Markdown Viewer mode"
  :group      'realtime-markdown-viewer
  :init-value nil
  :global     nil
  :lighter    "(RTMV)"
  (if realtime-markdown-viewer-mode
      (rtmv:init)
    (rtmv:finalize)))

(provide 'realtime-markdown-viewer)

;;; realtime-markdown-viewer.el ends here
