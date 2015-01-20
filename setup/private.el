;;; private.el --- take care of your private configuration files.

;; Copyright (C) 2015 Cheung Mou Wai

;; Author: Cheung Mou Wai <yeannylam@gmail.com>
;; URL: https://github.com/cheunghy/private
;; Keywords: private, configuration, backup, recover
;; Package-Requires: ((aes "0.6"))

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

;; Usage:
;;
;; Put every private configuration into ~/.emacs.d/private
;; and make git ignoring the private dir.
;; (require-private)
;; This function requires all your private configuration files.
;; (find-private-configuration-file)
;; This function creates or visits a private configuration file.
;; (backup-private)
;; This function backups all your private configuration files.
;; So it's safe to put any private project related configuraiton on the
;; public git repo.
;; (recover-private)
;; This function recovers all your private configuration files.
;; (clear-private-backup)
;; This function removes all your private configuration backup files.

;;; Code:

(require 'aes)

(defconst user-private-config-directory
  (expand-file-name "private" user-emacs-directory)
  "This directory is under `user-emacs-directory' and is the directory that
contains all users' private configuration files. These normally include
corporation project configuration, corporation code, personal project code
etc.")

(defconst user-private-backup-directory
  (expand-file-name "private-backup" user-emacs-directory)
  "This directory is under `user-emacs-directory' and is the directory that
contains all users' private backup files. These files are encrypted with user
specified password. These are used to recover user's private files on another
computer.")

(defun rq--load-directory (dir-name &optional error-string)
  "Load all files from a directory."
  (or (file-directory-p dir-name)
      (error (or error-string "'%s' is neither a directory nor exist.")
             dir-name))
  (dolist (file (directory-files dir-name t "^[^.]"))
    (if (file-directory-p file)
        (rq--load-directory file)
      (load-file file))))

(defun rq--create-backup-dir (&optional error-string)
  "Create backup dir if needed."
  (unless (and (file-exists-p user-private-backup-directory)
               (file-directory-p user-private-backup-directory))
    (if (file-exists-p user-private-backup-directory)
        (error (or error-string "Private backup file '%s' is occupied."
                   user-private-backup-directory)))
    (make-directory user-private-backup-directory)))

(defun rq--backup-file-name (config-file)
  "Convert CONFIG-FILE into backup file name."
  (expand-file-name (file-name-nondirectory config-file)
                    user-private-backup-directory))

(defun rq--config-file-name (backup-file)
  "Convert BACKUP-FILE into config file name."
  (expand-file-name (file-name-nondirectory backup-file)
                    user-private-config-directory))

(defun rq--aes-key-from-password (password type-or-file Nk)
  "Return a key, generated from PASSWORD.
This function does not suck."
  (let* (passwd passwdkeys (p ""))
    (if (and (not aes-always-ask-for-passwords)
             aes-enable-plaintext-password-storage
             (assoc type-or-file aes-plaintext-passwords))
        (setq passwd (cdr (assoc type-or-file aes-plaintext-passwords)))
      (while (equal p "")
        (setq p password))
      (if (and (not aes-always-ask-for-passwords)
               aes-enable-plaintext-password-storage
               (not (get-buffer type-or-file))
               (not (equal "string" type-or-file)))
          (progn
            ;; store the new password
            (setq aes-plaintext-passwords
                  (cons (cons type-or-file p) aes-plaintext-passwords))
            ;; reset idle timer
            (if aes-idle-timer-value
                (progn (cancel-timer aes-idle-timer-value)
                       (setq aes-idle-timer-value nil)))
            ;; set new idle timer
            (if (< 0 aes-delete-passwords-after-idle)
                (setq aes-idle-timer-value
                      (run-with-idle-timer
                       aes-delete-passwords-after-idle
                       nil
                       'aes-idle-clear-plaintext-keys)))))
      (setq passwd p))
    (setq passwd (aes-zero-pad passwd (lsh Nk 2)))
    (setq passwdkeys
          (aes-KeyExpansion
           (aes-str-to-b (substring passwd 0 (lsh Nk 2))) Nk))
    (substring (aes-cbc-encrypt passwd (make-string (lsh Nk 2) 0) passwdkeys Nk)
               (- (lsh Nk 2)))))

(defun rq--aes-encrypt-current-buffer (password)
  "Encrypt current buffer with PASSWORD.
This function does not suck."
  (let* ((Nb aes-Nb) (Nk aes-Nk) (buffer (current-buffer)) (nonb64 nil)
         (length (with-current-buffer buffer (point-max)))
         (type aes-default-method)
         (group (or (aes-exec-passws-hooks (buffer-file-name buffer))
                    (buffer-name buffer)))
         (key (aes-str-to-b (rq--aes-key-from-password password group Nk)))
         (keys (aes-KeyExpansion key Nb))
         (iv (let* ((x (make-string (lsh Nb 2) 0))
                    (aes-user-interaction-entropy nil)
                    (y (aes-user-entropy (lsh Nb 2) 256))
                    (i 0)
                    (border (lsh Nb 2)))
               (while (< i border) (aset x i (car y)) (setq y (cdr y))
                      (setq i (1+ i)))
               x))
         (multibyte
          (if (with-current-buffer buffer
                enable-multibyte-characters)
              "M" "U"))
         (unibyte-string
          (with-current-buffer buffer
            (if (equal multibyte "M") (set-buffer-multibyte nil))
            (buffer-substring-no-properties (point-min) (point-max))))
         (header (format "aes-encrypted V 1.2-%s-%s-%d-%d-%s\n"
                         type (if nonb64 "N" "B") Nb Nk multibyte))
         (plain (if (equal type "OCB") unibyte-string
                  (concat (number-to-string (length unibyte-string))
                          "\n" unibyte-string)))
         (enc (if (equal type "OCB")
                  (let* ((res (aes-ocb-encrypt plain header iv keys Nb)))
                    (concat iv (cdr res) (car res)))
                (concat iv (aes-cbc-encrypt plain iv keys Nb)))))
    (if nonb64 nil
      (setq enc (base64-encode-string enc)))
    (setq enc (concat header enc))
    (with-current-buffer buffer
      (erase-buffer)
      (insert enc)
      (if aes-discard-undo-after-encryption
          (setq buffer-undo-list))
      t)))

(defun rq--aes-decrypt-current-buffer (password)
  "Decrypt current buffer with PASSWORD.
This function does not suck."
  (let* ((buffer (current-buffer))
         (sp (with-current-buffer buffer
               (buffer-substring-no-properties
                (point-min) (point-max)))))
    (and
     (or (string-match
          (concat
           "aes-encrypted V 1.2-\\(CBC\\|OCB\\)-\\([BN]\\)-"
           "\\([0-9]+\\)-\\([0-9]+\\)-\\([MU]\\)\n") sp)
         (and (message "Buffer '%s' is not properly encrypted." buffer)
              nil))
     (let* ((type (match-string 1 sp))
            (b64 (equal "B" (match-string 2 sp)))
            (Nb (string-to-number (match-string 3 sp)))
            (blocksize (lsh Nb 2))
            (Nk (string-to-number (match-string 4 sp)))
            (Nr (+ (max Nk Nb) 6))
            (um (match-string 5 sp))
            (multibyte (equal "M" (match-string 5 sp)))
            (header (match-string 0 sp))
            (res1 (substring sp (match-end 0)))
            (res2 (if b64 (base64-decode-string res1) res1))
            (iv (substring res2 0 blocksize))
            (enc-offset (cond ((equal type "CBC") blocksize)
                              ((equal type "OCB") (lsh blocksize 1))))
            (tag (substring res2 blocksize enc-offset))
            (enc (substring res2 enc-offset))
            (group (or (aes-exec-passws-hooks
                        (buffer-file-name buffer))
                       (buffer-name buffer)))
            (key (aes-str-to-b (rq--aes-key-from-password password group Nk)))
            (keys (aes-KeyExpansion key Nb))
            (res (if (equal type "CBC")
                     (aes-cbc-decrypt enc iv (nreverse keys) Nb)
                   (aes-ocb-decrypt enc header tag iv keys Nb)))
            len)
       (if (or (and (equal type "CBC")
                    (not (string-match "\\`\\([0-9]+\\)\n" res)))
               (and (equal type "OCB") (not res)))
           (progn (message (concat "buffer '"
                                   buffer
                                   "' could not be decrypted."))
                  (if group
                      (setq aes-plaintext-passwords
                            (assq-delete-all group aes-plaintext-passwords))))
         (setq len (and (equal type "CBC")
                        (string-to-number (match-string 1 res))))
         (setq res (if (equal type "OCB") res
                     (substring res (match-end 0) (+ (match-end 0) len))))
         (with-current-buffer buffer
           (erase-buffer)
           (set-buffer-multibyte nil)
           (insert res)
           (setq buffer-file-coding-system
                 (car (find-coding-systems-region
                       (point-min) (point-max))))
           (if multibyte (set-buffer-multibyte t))
           t))))))

(defun require-private ()
  "Require private configurations which located at '~/.emacs.d/private'.
If the private folder does not exist, create by default."
  (interactive)
  (if (and (file-exists-p user-private-config-directory)
           (file-directory-p user-private-config-directory))
      (rq--load-directory user-private-config-directory)
    (if (not (file-exists-p user-private-config-directory))
        (make-directory user-private-config-directory)
      (error "You may delete %s by hand in order
to use private package features." user-private-config-directory))))

(defun find-private-configuration-file ()
  "Visit or create private configuration file."
  (interactive)
  (if (not (file-exists-p user-private-config-directory))
      (make-directory user-private-config-directory))
  (ido-find-file-in-dir user-private-config-directory))

(defun clear-private-backup (sure remove-or-trash)
  "Delete all private backup files."
  (interactive (list (yes-or-no-p "Are you sure? ")
                     (completing-read "Remove or trash: "
                                      (list "remove" "trash")
                                      nil nil nil nil
                                      "trash")))
  (and sure (file-directory-p user-private-backup-directory)
       (progn
         (delete-directory user-private-backup-directory t
                           (if (string= "trash" remove-or-trash)
                               t nil))
         (rq--create-backup-dir))))

(defun backup-private (password confirm)
  "Backup all private files."
  (interactive "sEnter password: \nsConfirm password: ")
  (rq--create-backup-dir)
  (unless (string= password confirm)
    (error "Password not consistent."))
  (dolist (file (directory-files user-private-config-directory t "^[^.]"))
    (if (file-exists-p (rq--backup-file-name file))
        (message "File exists at: '%s', won't backup '%s'."
                 (rq--backup-file-name file) file)
      (copy-file file (rq--backup-file-name file))
      (with-current-buffer (find-file-noselect (rq--backup-file-name file))
        (rq--aes-encrypt-current-buffer password)
        (save-buffer)
        (kill-buffer)))))

(defun recover-private (password)
  "Recover all private files."
  (interactive "sEnter password: ")
  (rq--create-backup-dir)
  (dolist (file (directory-files user-private-backup-directory t "^[^.]"))
    (if (file-exists-p (rq--config-file-name file))
        (message "File exists at: '%s', won't recover '%s'."
                 (rq--config-file-name file) file)
      (copy-file file (rq--config-file-name file))
      (with-current-buffer (find-file-noselect (rq--config-file-name file))
        (rq--aes-decrypt-current-buffer password)
        (save-buffer)
        (kill-buffer)))))

(provide 'private)
;;; private.el ends here
