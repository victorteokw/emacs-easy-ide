;; ruby editing features

;; See http://hbin.me/blog/2012/08/24/emacs-user-tips-for-rails/
(defun insert-arrow ()
  (interactive)
  (delete-horizontal-space t)
  (insert " => "))
;;(define-key ruby-mode-map (kbd "C-c .") 'insert-arrow)

(add-hook 'ruby-mode-hook
          '(lambda ()
             (modify-syntax-entry ?$ "w")
             (modify-syntax-entry ?@ "w")
             (modify-syntax-entry ?: ".")))


;; Ruby tools minor mode
;; Which is a collection of handy functions

(defun ruby-tools-looking-around (back at)
  "Check if looking backwards at BACK and forward at AT."
  (and (looking-at-p at) (looking-back back)))

(defun ruby-tools-symbol-at-point-p ()
  "Check if cursor is at a symbol or not."
  (ruby-tools-looking-around ":[A-Za-z0-9_]*" "[A-Za-z0-9_]*"))

(defun ruby-tools-string-at-point-p ()
  "Check if cursor is at a string or not."
  (ruby-tools-string-region))

(defun ruby-tools-symbol-region ()
  "Return region for symbol at point."
  (list
   (save-excursion
     (search-backward ":" (line-beginning-position) t))
   (save-excursion
     (if (re-search-forward "[^A-Za-z0-9_]" (line-end-position) t)
         (1- (point))
       (line-end-position)))))

(defun ruby-tools-string-region ()
  "Return region for string at point."
  (let ((orig-point (point))
        (regex "'\\(\\(\\\\'\\)\\|[^']\\)*'\\|\"\\(\\(\\\\\"\\)\\|[^\"]\\)*\"")
        beg end)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (and (re-search-forward regex (line-end-position) t)
                  (not (and beg end)))
        (let ((match-beg (match-beginning 0)) (match-end (match-end 0)))
          (when (and
                 (> orig-point match-beg)
                 (< orig-point match-end))
            (setq beg match-beg)
            (setq end match-end))))
      (and beg end (list beg end)))))

(defun ruby-tools-interpolate ()
  "Interpolate with #{} in some places."
  (interactive)
  (if (and mark-active (equal (point) (region-end)))
      (exchange-point-and-mark))
  (insert ?#)
  (when (or
         (ruby-tools-looking-around "\"[^\"\n]*" "[^\"\n]*\"")
         (ruby-tools-looking-around "`[^`\n]*" "[^`\n]*`")
         (ruby-tools-looking-around "%([^(\n]*" "[^)\n]*)"))
    (cond (mark-active
           (goto-char (region-beginning))
           (insert ?\{)
           (goto-char (region-end))
           (insert ?\}))
          (t
           (insert "{}")
           (forward-char -1)))))

(defun ruby-tools-to-symbol ()
  "Turn string at point to symbol."
  (interactive)
  (if (ruby-tools-string-at-point-p)
      (let* ((region (ruby-tools-string-region))
             (min (nth 0 region))
             (max (nth 1 region))
             (content (buffer-substring-no-properties (1+ min) (1- max))))
        (when (string-match-p "^\\([a-zA-Z_][a-zA-Z0-9_]*\\)?$" content)
          (let ((orig-point (point)))
            (delete-region min max)
            (insert (concat ":" content))
            (goto-char orig-point))))))

(defun ruby-tools-to-single-quote-string ()
  (interactive)
  (ruby-tools-to-string "'"))

(defun ruby-tools-to-double-quote-string ()
  (interactive)
  (ruby-tools-to-string "\""))

(defun ruby-tools-to-string (string-quote)
  "Convert symbol or string at point to string."
  (let* ((at-string (ruby-tools-string-at-point-p))
         (at-symbol (and (not at-string) (ruby-tools-symbol-at-point-p))))
    (when (or at-string at-symbol)
      (let* ((region (or
                      (and at-symbol (ruby-tools-symbol-region))
                      (and at-string (ruby-tools-string-region))))
             (min (nth 0 region))
             (max (nth 1 region))
             (content
              (buffer-substring-no-properties (1+ min)
                                              (if at-symbol max (1- max)))))
        (setq content
              (if (equal string-quote "'")
                  (replace-regexp-in-string
                   "\\\\\"" "\""
                   (replace-regexp-in-string "\\([^\\\\]\\)'" "\\1\\\\'"
                                             content))
                (replace-regexp-in-string
                 "\\\\\'" "'"
                 (replace-regexp-in-string "\\([^\\\\]\\)\"" "\\1\\\\\""
                                           content))))
        (let ((orig-point (point)))
          (delete-region min max)
          (insert
           (format "%s%s%s" string-quote content string-quote))
          (goto-char orig-point))))))

(defun ruby-tools-clear-string ()
  "Clear string at point."
  (interactive)
  ;; Will this go into kill ring?
  )

(defun ruby-tools-to-regexp ()
  "Turn a string or a symbol into Regexp."
  (interactive)
  (ruby-tools-to-string "/")
  )
(defvar ruby-tools-mode-keymap-prefix (kbd "C-c")
  "`ruby-tools-mode' keymap prefix.")

(defvar ruby-tools-mode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ":") 'ruby-tools-to-symbol)
    (define-key map (kbd "\"") 'ruby-tools-to-double-quote-string)
    (define-key map (kbd "'") 'ruby-tools-to-single-quote-string)
    (define-key map (kbd "#") 'ruby-tools-interpolate)
    (define-key map (kbd ";") 'ruby-tools-clear-string)
    (define-key map (kbd "/") 'ruby-tools-to-regexp)
    map
    )
  "Key map after `ruby-tools-mode-keymap-prefix'.")

(defvar ruby-tools-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ruby-tools-mode-keymap-prefix ruby-tools-mode-command-map)
    map)
  "Key map for ruby tools mode.")

(define-minor-mode ruby-tools-mode
  "Collection of handy functions for ruby-mode."
  :init-value nil
  :lighter nil
  :keymap ruby-tools-mode-map)

(add-hook 'ruby-mode-hook 'ruby-tools-mode)
;; Ruby tools mode ends here.


(add-hook 'ruby-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup)
             (setq-local whitespace-style '(empty indentation::space
                                                  space-befure-tab::space
                                                  trailing
                                                  whitespace-style::space))
             ;; Whitespace showing and cleaning

             ;; Do not deep indent
             (setq ruby-deep-indent-paren nil)

             ))
