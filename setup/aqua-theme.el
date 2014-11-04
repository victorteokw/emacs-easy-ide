;; Define a new theme
(deftheme aqua "The Aqua Color Theme")

(defvar aqua-colors-alist
  '(("aqua-fg" . "#000000")
    ("aqua-bg" . "brightwhite")
    ("aqua-keyword" . "#AA0D91")
    ("aqua-comment" . "#007400")
    ("aqua-preprocessor" . "#af8700")
    ("aqua-function-name" . "#1614FF")
    ("aqua-builtin" . "#00cdcd")
    ("aqua-constant" . "#000000")
    ("aqua-string" . "#C41A16")
    ("aqua-variable" . "#5fafff")
    ("aqua-type" . "#000000")
    ("aqua-warning" . "#ff0000")
    ("aqua-region" . "#A3CDFE")
    ("aqua-select-bg-y" . "#1C57AB")
    ("aqua-select-bg-n" . "#EEEEEE")
    ("aqua-bar-bg" . "#999999")
    )
  "List of aqua colors. Each element has the form (NAME . HEX).")

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
		  (file-name-as-directory
		   (file-name-directory load-file-name))))

(defmacro aqua-with-color-variables (&rest body)
  "`let' bind all colors defined in `zenburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   aqua-colors-alist))
     ,@body))

;; Theme faces
(aqua-with-color-variables
  (custom-theme-set-faces
   'aqua
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(menubar ((t (:foreground ,aqua-comment :background ,aqua-builtin))))
   `(link ((t (:foreground ,aqua-fg :underline t :weight bold))))
   `(link-visited ((t (:foreground ,aqua-fg :underline t :weight normal))))
   ;;`(default ((t (:foreground ,aqua-fg :background ,aqua-bg))))
   `(cursor ((t (:foreground ,aqua-fg :background ,aqua-fg))))
   `(escape-glyph ((t (:foreground ,aqua-constant :bold t))))
   `(fringe ((t (:foreground ,aqua-fg :background ,aqua-bg))))
   `(header-line ((t (:foreground ,aqua-fg
                                  :background ,aqua-bg
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,aqua-bg))))
   `(region ((t (:background, aqua-region))))
   `(success ((t (:foreground ,aqua-fg :weight bold))))
   `(warning ((t (:foreground ,aqua-fg :weight bold))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,aqua-builtin :weight normal))))
   `(font-lock-comment-face ((t (:foreground ,aqua-comment))))
   `(font-lock-commentelimiter-face ((t (:foreground ,aqua-comment))))
   `(font-lock-constant-face ((t (:foreground ,aqua-constant))))
   `(font-lockoc-face ((t (:foreground ,aqua-fg))))
   `(font-lock-function-name-face ((t (:foreground ,aqua-function-name))))
   `(font-lock-keyword-face ((t (:foreground ,aqua-keyword :weight normal))))
   `(font-lock-negation-char-face ((t (:foreground ,aqua-fg :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,aqua-preprocessor))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,aqua-fg :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,aqua-fg :weight bold))))
   `(font-lock-string-face ((t (:foreground ,aqua-string))))
   `(font-lock-type-face ((t (:foreground ,aqua-type))))
   `(font-lock-variable-name-face ((t (:foreground ,aqua-variable))))
   `(font-lock-warning-face ((t (:foreground ,aqua-warning :weight normal))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
   ;; auto complete
   `(ac-candidate-face ((t (:background, aqua-select-bg-n :foreground ,aqua-fg))))
   `(ac-selection-face ((t (:background, aqua-select-bg-y :foreground ,aqua-bg))))
   `(popup-tip-face ((t (:background, aqua-region :foreground ,aqua-fg))))
   `(popup-scroll-bar-foreground-face ((t (:background ,aqua-bar-bg))))
   `(popup-scroll-bar-background-face ((t (:background ,aqua-select-bg-n))))
   ))

(provide-theme 'aqua)
