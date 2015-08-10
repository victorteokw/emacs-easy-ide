;; Disable free variable warning
(eval-when-compile
  (require 'scss-mode))

;; Do not compile at save
(setq scss-compile-at-save nil)


;;; Rainbow

(add-hook 'scss-mode-hook 'rainbow-mode)

;;; Electric pair

(add-hook 'scss-mode-hook 'electric-pair-mode)

;; Inset colon pair
(defun hy-lang-sass-insert-colon-pair ()
  "Insert colon pair"
  (interactive)
  (let (string-before string-after temp)
    (setq string-after
          (save-excursion
            (setq temp (point))
            (end-of-line)
            (buffer-substring temp (point))))
    (setq string-before
          (save-excursion
            (setq temp (point))
            (beginning-of-line)
            (buffer-substring (point) temp)))
    (if (and (string-match "^[[:space:]]*;[[:space:]]*$" string-after)
             (not (= (line-beginning-position) (point))))
        (progn
          (kill-line)
          (setq string-after "")))
    (if (string-match ":[[:space:]]*$" string-before)
        (progn
          (delete-horizontal-space)
          (backward-delete-char 1)))
    (if (string-match-p ";" string-after)
        (insert ": ")
      (insert ": ;")
      (backward-char 1))))
(define-key scss-mode-map (kbd ":") 'hy-lang-sass-insert-colon-pair)

(defun hy-lang-sass-insert-block ()
  "Insert block {"
  (interactive)
  (delete-horizontal-space)
  (insert " {}")
  (backward-char 1)
  (newline-and-indent))
(define-key scss-mode-map (kbd "{") 'hy-lang-sass-insert-block)

(defun hy-lang-sass-make-block-inline ()
  "Make block inline"
  (delete-horizontal-space)
  (delete-char 1)
  (delete-char -1)
  (delete-horizontal-space)
  (insert "  ")
  (backward-char 1))

(defadvice delete-backward-char (around hy-lang-sass-block-inline activate)
  "Let's do."
  (if (eq last-command 'hy-lang-sass-insert-block)
      (hy-lang-sass-make-block-inline)
    ad-do-it))

(defadvice newline (before hy-lang-sass-auto-semicolon activate)
  "Let's do"
  (if (equal major-mode 'scss-mode)
      (let (line-content last-char at-eol)
        (setq line-content (buffer-substring (line-beginning-position)
                                             (line-end-position)))
        (setq at-eol (equal (point) (line-end-position)))
        (setq last-char (char-before (line-end-position)))
        (if (and at-eol
                 (not (string-match-p "[{}][:space:]*$" line-content))
                 (not (string-match-p "^[[:space:]{}]*$" line-content))
                 (not (string= ";" (format "%c" last-char))))
            (insert ";")
          (if (equal ";" (format "%c" last-char))
              (move-end-of-line 1))))))

;;; Auto complete css property

(eval-when-compile
  (require 'auto-complete))

(add-hook 'scss-mode-hook
          (lambda ()
            (require 'auto-complete)
            (setq ac-sources '(ac-source-css-property))
            (auto-complete-mode)))

(provide 'hy-lang-scss)
