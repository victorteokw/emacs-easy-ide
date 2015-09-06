(add-to-list 'custom-theme-load-path
             (file-name-directory
              (or load-file-name
                  (buffer-file-name (current-buffer)))))

(deftheme eide "Eide theme")

(defvar uju-tint-pink "#ff007a")
(defvar uju-tint-green "#3eddb0")
(defvar uju-tint-green "#3eddb0")
(defvar uju-tint-yellow "#ffd400")
(defvar uju-tint-blue "#00b7ff")
(defvar uju-tint-orange "#ff5b00")
(defvar uju-comment-gray "#a8a8a8")
(defvar uju-mode-line-bg "#f9f9f9")
(defvar uju-mode-line-fg "#a0a0a0")
(defvar uju-fringe-bg "#f7f7f7")
(defvar uju-line-num-fg "#c6c6c6")
(defvar uju-line-num-highlight "#4a4a4a")
(defvar uju-selection-bg "#eea9ff")
(defvar uju-white "#ffffff")
(defvar uju-black "#000000")
(defvar uju-cursor "#8200ff")
(defvar uju-window-divider "#e8e8e8")
(defvar uju-link "#f26bc9")
(defvar uju-link-highlight "#f946d3")

(defvar uju-gutter-green "#3eddb0")
(defvar uju-gutter-red "#e25980")
(defvar uju-gutter-blue "#2ca7f4")
(defvar uju-gutter-yellow "#f2da2f")

;; http://www.baidu.com
(custom-theme-set-faces
 'eide
;;; basic
 `(default ((t (:foreground "#000000" :background "#ffffff"))))
 `(shadow ((t (:foreground ,uju-line-num-fg))))
 `(cursor ((t (:foreground "#000000" :background ,uju-cursor
                           :inverse-video t))))
 `(match ((t (:weight bold))))
 ;;`(escape-glyph ((t (:foreground ,violet))))

 `(window-divider ((t (:foreground ,uju-window-divider :background ,uju-window-divider))))

 `(fringe ((t (:foreground ,uju-line-num-fg :background ,uju-fringe-bg))))

 `(highlight ((t (:foreground ,uju-link-highlight :background nil))))
 `(link ((t (:foreground ,uju-link :underline t))))
 `(link-visited ((t (:foreground ,uju-link :underline t))))
 `(success ((t (:foreground ,uju-gutter-green))))
 `(warning ((t (:foreground ,uju-gutter-yellow))))
 `(error ((t (:foreground ,uju-gutter-red))))
 ;;
 `(lazy-highlight ((t (:foreground ,uju-link-highlight))))
 `(widget-field ((t (:background ,uju-tint-blue))))
 '(button ((t (:underline t))))

;;; misc faces
 ;;`(menu ((,class (:foreground ,base0 :background ,base03))))
 ;;`(minibuffer-prompt ((,class (:foreground ,base0))))
 `(mode-line
   ((t (:inverse-video unspecified
                       :foreground ,uju-mode-line-fg
                       :background ,uju-mode-line-bg
                       ))))
 `(mode-line-buffer-id ((t (:foreground ,uju-tint-pink :weight bold))))
 `(mode-line-inactive
   ((t (:inverse-video unspecified
                       :foreground ,uju-mode-line-fg
                       :background ,uju-mode-line-bg
                       ))))
 `(header-line
   ((t (:inverse-video unspecified
                       :foreground ,uju-mode-line-fg
                       :background ,uju-mode-line-bg
                       ))))
 `(region ((t (:foreground ,uju-white :background ,uju-selection-bg))))
 ;; `(secondary-selection ((,class (:background ,base02))))

 `(trailing-whitespace ((t (:background ,uju-gutter-red))))

 `(vertical-border ((t (:foreground ,uju-window-divider))))

;;; font lock
 `(font-lock-comment-face ((t (:foreground ,uju-comment-gray))))
 `(font-lock-comment-delimiter-face ((t (:foreground ,uju-comment-gray))))
 `(font-lock-string-face ((t (:foreground ,uju-tint-green))))
 `(font-lock-doc-face ((t (:foreground ,uju-tint-green))))
 `(font-lock-keyword-face ((t (:foreground ,uju-tint-pink))))
 `(font-lock-builtin-face ((t (:foreground ,uju-tint-green))))
 `(font-lock-function-name-face ((t (:foreground ,uju-tint-orange))))
 `(font-lock-variable-name-face ((t (:foreground ,uju-tint-blue))))
 `(font-lock-type-face ((t (:foreground ,uju-tint-blue))))
 `(font-lock-constant-face ((t (:foreground ,uju-tint-blue))))
 `(font-lock-warning-face ((t (:foreground "#FF0000"))))
 `(font-lock-negation-char-face ((t (:foreground "#FF0000"))))
 `(font-lock-preprocessor-face ((t (:foreground "#FF0000"))))
 `(font-lock-regexp-grouping-backslash ((t (:foreground "#000000"))))
 `(font-lock-regexp-grouping-construct ((t (:foreground "#000000"))))
 `(c-annotation-face ((t (:foreground "#000000"))))

;;; linum-mode
 `(linum ((t (:foreground ,uju-line-num-fg :background ,uju-fringe-bg))))
;;; linum-highlight
 `(linum-highlight-face ((t (:foreground ,uju-line-num-highlight :background ,uju-fringe-bg))))

;;; hl-line-mode
 `(hl-line ((t (:background "#eeeeee"))))
 `(hl-line-face ((t (:background "#cccccc"))))

;;; diff-hl-mode
 `(diff-hl-insert ((t (:background ,uju-gutter-green))))
 `(diff-hl-delete ((t (:background ,uju-gutter-red))))
 `(diff-hl-change ((t (:background ,uju-gutter-blue))))

 )


;; (custom-theme-set-variables
;;  'eide)

(provide 'eide-theme)
