;;; init-scroll.el --- make up and down nice to scroll -*- lexical-binding: t -*-

;; Maintainer: Zhang Kai Yu
;; Keywords: ide

;;; Commentary:

;;; This config load the startup screen for easy ide

;;; Code:

;; Using C-x p as backward movement of C-x o
(defun other-window-backward (&optional n)
  "Move to the Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))
(global-set-key "\C-xp" 'other-window-backward)

;; Using Up Arrow and Down Arrow to navigate up and down
;; FIXME: left and right scroll won't scroll if cursor is at left edge
;; FIXME: up and down scroll, cursor position shouldn't change if scroll over a screen
(defvar cursor-position-before-scroll nil
  "The curson position before scroll, restore after scroll.")

(defun scroll-n-lines-up (&optional n)
  "Scroll up N lines (1 by default)."
  (interactive "P")
  (setq cursor-position-before-scroll (point))
  (scroll-up (prefix-numeric-value n))
  (goto-char cursor-position-before-scroll)
  (setq cursor-position-before-scroll nil))

(defun scroll-n-lines-down (&optional n)
  "Scroll down N lines (1 by default)."
  (interactive "P")
  (setq cursor-position-before-scroll (point))
  (scroll-down (prefix-numeric-value n))
  (goto-char cursor-position-before-scroll)
  (setq cursor-position-before-scroll nil))

(defun scroll-n-lines-left (&optional n)
  "Scroll left N lines (1 by default)."
  (interactive "P")
  (setq cursor-position-before-scroll (point))
  (scroll-left (prefix-numeric-value n))
  (goto-char cursor-position-before-scroll)
  (setq cursor-position-before-scroll nil))

(defun scroll-n-lines-right (&optional n)
  "Scroll right N lines (1 by default)."
  (interactive "P")
  (setq cursor-position-before-scroll (point))
  (scroll-right (prefix-numeric-value n))
  (goto-char cursor-position-before-scroll)
  (setq cursor-position-before-scroll nil))

(global-set-key [down] 'scroll-n-lines-down)
(global-set-key [up] 'scroll-n-lines-up)
(global-set-key [left] 'scroll-n-lines-left)
(global-set-key [right] 'scroll-n-lines-right)

(defvar other-cursor-position-before-scroll nil
  "The cursor position of other window before scroll, restore after scroll.")

(defun scroll-other-window-n-lines-up (&optional n)
  "Scroll other window N lines up (1 by default)."
  (interactive "P")
  (setq other-cursor-position-before-scroll nil)
  (scroll-other-window-down (prefix-numeric-value n)))

(defun scroll-other-window-n-lines-down (&optional n)
  "Scroll other window N lines down (1 by default)."
  (interactive "P")
  (scroll-other-window-down (prefix-numeric-value n)))

(setq shift-select-mode nil)

;(global-set-key [C-up] 'scroll-other-window-n-lines-up)
;(global-set-key [C-down] 'scroll-other-window-n-lines-down)

; S-down S-up S-left S-right
;;; init-scroll.el ends here
