;;; scratches.el --- Multiple scratches in any language

;; Copyright (C) 2015 Zhang Kai Yu

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Keywords: scratch

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

;;

;;; Code:

(defgroup scratches nil
  "Multiple scratches in any language."
  :group 'convenience
  :prefix "scratch-")

(defcustom scratches-save-location
  (expand-file-name "scratches" user-emacs-directory)
  "The directory to store scratches."
  :group 'scratches
  :type 'string)

(defun scratches-visit-scratch (name)
  "Visit scratch file with NAME."
  (interactive))

(defun scratches-visit-scratch-other-window (name)
  "Visit scratch file with NAME other window."
  (interactive))

(defun scratches-visit-last-scratch ()
  "Visit last scratch file."
  (interactive))

;;;###autoload
(define-minor-mode scratches-mode
  "Multiple scratches in any language.

When called interactively, toggle `scratches-mode'.  With prefix
ARG, enable `scratches-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `scratches-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `scratches-mode'.
Otherwise behave as if called interactively.

\\{scratches-mode-map}"
  :lighter projectile-mode-line
;  :keymap scratches-mode-map
  :group 'convenience
  :require 'scratches
  )

;;;###autoload
(define-globalized-minor-mode scratches-global-mode
  scratches-mode
  scratches-mode)

(provide 'eide-scratches)
;;; eide-scratches.el ends here
