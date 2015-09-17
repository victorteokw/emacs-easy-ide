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

(defvar uju-deep-blue "#0d17d3")

(defvar uju-blue-cluster-01 "#56e9f1")
(defvar uju-blue-cluster-02 "#0fb9da")
(defvar uju-blue-cluster-03 "#3783a7")

(defvar uju-selection-bg "#eea9ff")
(defvar uju-cursor "#8200ff")

(defvar uju-white "#ffffff")
(defvar uju-black "#000000")

(defvar uju-window-divider "#e8e8e8")
(defvar uju-fringe-bg "#f7f7f7")
(defvar uju-line-num-fg "#c6c6c6")
(defvar uju-line-num-highlight "#4a4a4a")
(defvar uju-mode-line-bg "#f9f9f9")
(defvar uju-mode-line-fg "#a0a0a0")
(defvar uju-mode-line-inactive "#f0f0f0")

(defvar uju-link "#f26bc9")
(defvar uju-link-highlight "#f946d3")

(defvar uju-gutter-green "#3eddb0")
(defvar uju-gutter-red "#e25980")
(defvar uju-gutter-blue "#2ca7f4")
(defvar uju-gutter-yellow "#f2da2f")

(custom-theme-set-faces
 'eide
;;; basic
 `(default ((t (:foreground "#000000" :background "#ffffff"))))
 `(shadow ((t (:foreground ,uju-line-num-fg))))
 `(cursor ((t (:foreground ,uju-white :background ,uju-cursor
                           :inverse-video t))))
 `(match ((t (:weight bold))))
 `(escape-glyph ((t (:foreground ,uju-tint-yellow))))

 `(window-divider ((t (:foreground ,uju-window-divider :background ,uju-window-divider))))

 `(fringe ((t (:foreground ,uju-line-num-fg :background ,uju-fringe-bg))))

 `(highlight ((t (:foreground ,uju-link-highlight :background nil))))
 `(link ((t (:foreground ,uju-link :underline t))))
 `(link-visited ((t (:foreground ,uju-link :underline t))))
 `(success ((t (:foreground ,uju-gutter-green))))
 `(warning ((t (:foreground ,uju-gutter-yellow))))
 `(error ((t (:foreground ,uju-gutter-red))))

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
                       :background ,uju-mode-line-inactive
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
 `(font-lock-warning-face ((t (:foreground ,uju-gutter-red))))
 `(font-lock-negation-char-face ((t (:foreground "#FF0000"))))
 `(font-lock-preprocessor-face ((t (:foreground "#FF0000"))))
 `(font-lock-regexp-grouping-backslash ((t (:foreground "#000000"))))
 `(font-lock-regexp-grouping-construct ((t (:foreground "#000000"))))
 `(c-annotation-face ((t (:foreground "#000000"))))

;;; paren showing faces
 `(show-paren-match ((t (:weight bold))))
 `(show-paren-mismatch ((t (:foreground ,uju-gutter-red))))

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

;;; dired
 `(dired-directory ((t (:foreground ,uju-tint-blue :weight normal))))
 `(dired-flagged ((t (:foreground ,uju-gutter-red))))
 `(dired-header ((t (:foreground ,uju-tint-green))))
 `(dired-ignored ((t (:inherit shadow))))
 ;;`(dired-mark ((t (:foreground ,yellow :weight bold))))
 ;;`(dired-marked ((t (:foreground ,magenta :weight bold))))
 ;;`(dired-perm-write ((t (:foreground ,base0 :underline t))))
 ;;`(dired-symlink ((t (:foreground ,cyan :weight normal :slant italic))))
 `(dired-warning ((t (:inherit warning))))

;;; ace-jump-mode
 `(ace-jump-face-background
   ((t (:foreground ,uju-comment-gray :background ,uju-fringe-bg
                    :inverse-video nil))))
 `(ace-jump-face-foreground
   ((t (:foreground ,uju-deep-blue :background ,uju-fringe-bg :inverse-video nil :weight bold))))

;;; avy
 `(avy-background-face
   ((t (:foreground ,uju-comment-gray :background ,uju-fringe-bg
                    :inverse-video nil))))
 `(avy-lead-face
   ((t (:foreground ,uju-deep-blue :background ,uju-fringe-bg :inverse-video nil :weight bold))))
 `(avy-lead-face-0
   ((t (:foreground ,uju-deep-blue :background ,uju-fringe-bg :inverse-video nil :weight bold))))
 `(avy-lead-face-1
   ((t (:foreground ,uju-deep-blue :background ,uju-fringe-bg :inverse-video nil :weight bold))))
 `(avy-lead-face-2
   ((t (:foreground ,uju-deep-blue :background ,uju-fringe-bg :inverse-video nil :weight bold))))

;;; auto-complete
 `(ac-candidate-face ((t (:background ,uju-black :foreground ,uju-white))))
 `(ac-selection-face ((t (:background ,uju-tint-blue :foreground ,uju-white))))
 ;;`(ac-candidate-mouse-face ((t (:background ,cyan-hc :foreground ,cyan-lc))))
 `(ac-completion-face ((t (:foreground ,uju-comment-gray :underline t))))
 ;;`(ac-gtags-candidate-face ((t (:background ,base02 :foreground ,blue))))
 ;;`(ac-gtags-selection-face ((t (:background ,blue-lc :foreground ,blue-hc))))
 ;;`(ac-yasnippet-candidate-face ((t (:background ,base02 :foreground ,yellow))))
 ;;`(ac-yasnippet-selection-face ((t (:background ,yellow-lc :foreground ,yellow-hc))))

;;; ido-mode
 `(ido-first-match ((t (:weight bold))))
 `(ido-only-match ((t (:weight bold))))
 `(ido-subdir ((t (:foreground ,uju-tint-blue))))
 `(ido-incomplete-regexp ((t (:foreground ,uju-tint-pink :weight bold ))))
 `(ido-indicator ((t (:background ,uju-black :foreground ,uju-black))))
 `(ido-virtual ((t (:foreground ,uju-black))))

;;; multiple cursors
 `(mc/cursor-face ((t (:background ,uju-cursor :foreground ,uju-white, :inverse-video nil))))

;;;;; guide-key
 `(guide-key/prefix-command-face ((t (:foreground ,uju-black))))
 `(guide-key/highlight-command-face ((t (:foreground ,uju-black))))
 `(guide-key/key-face ((t (:foreground ,uju-black))))
;;;;; magit
;;;;;; headings and diffs
 `(magit-section-highlight           ((t (:background ,uju-blue-cluster-01))))
 `(magit-section-heading             ((t (:foreground ,uju-tint-pink :weight bold))))
 `(magit-section-heading-selection   ((t (:foreground ,uju-blue-cluster-02 :weight bold))))
 `(magit-diff-file-heading           ((t (:weight bold))))
 `(magit-diff-file-heading-highlight ((t (:background ,uju-tint-yellow :weight bold))))
 ;;  `(magit-diff-file-heading-selection ((t (:background ,base02
 ;;                                                       :foreground ,orange :weight bold))))
 ;;  `(magit-diff-hunk-heading
 ;;    ((t (:background ,(solarized-color-blend yellow base03 0.1)))))
 ;;  `(magit-diff-hunk-heading-highlight
 ;;    ((t (:background ,(solarized-color-blend yellow base02 0.1)))))
 ;;  `(magit-diff-hunk-heading-selection
 ;;    ((t (:background ,(solarized-color-blend yellow base02 0.1)
 ;;                     :foreground ,orange
 ;;                     :weight bold))))
 ;;  `(magit-diff-lines-heading          ((t (:background ,orange
 ;;                                                       :foreground ,base3))))
 ;;  `(magit-diff-context-highlight      ((t (:background ,base02))))
 `(magit-diffstat-added              ((t (:foreground ,uju-tint-green))))
 `(magit-diffstat-removed            ((t (:foreground ,uju-gutter-red))))
 ;; ;;;;;; popup
 ;;  `(magit-popup-heading             ((t (:foreground ,base1 :weight normal))))
 ;;  `(magit-popup-key                 ((t (:foreground ,base1 :weight bold))))
 ;;  `(magit-popup-argument            ((t (:foreground ,base1 :weight bold))))
 ;;  `(magit-popup-disabled-argument   ((t (:foreground ,base01 :weight normal))))
 ;;  `(magit-popup-option-value        ((t (:foreground ,base1 :weight bold))))
 ;; ;;;;;; process
 ;;  `(magit-process-ok    ((t (:foreground ,green :weight bold))))
 ;;  `(magit-process-ng    ((t (:foreground ,red   :weight bold))))
 ;; ;;;;;; log
 ;;  `(magit-log-author    ((t (:foreground ,base0))))
 ;;  `(magit-log-date      ((t (:foreground ,base01))))
 ;;  `(magit-log-graph     ((t (:foreground ,base1))))
 ;; ;;;;;; sequence
 ;;  `(magit-sequence-pick ((t (:foreground ,yellow-d))))
 ;;  `(magit-sequence-stop ((t (:foreground ,green))))
 ;;  `(magit-sequence-part ((t (:foreground ,yellow))))
 ;;  `(magit-sequence-head ((t (:foreground ,blue))))
 ;;  `(magit-sequence-drop ((t (:foreground ,red))))
 ;;  `(magit-sequence-done ((t (:foreground ,base01))))
 ;;  `(magit-sequence-onto ((t (:foreground ,base01))))
 ;; ;;;;;; bisect
 ;;  `(magit-bisect-good ((t (:foreground ,green))))
 ;;  `(magit-bisect-skip ((t (:foreground ,yellow))))
 ;;  `(magit-bisect-bad  ((t (:foreground ,red))))
 ;; ;;;;;; blame
 ;;  `(magit-blame-heading ((t (:background ,base1 :foreground ,base02))))
 ;;  `(magit-blame-hash    ((t (:background ,base1 :foreground ,base02))))
 ;;  `(magit-blame-name    ((t (:background ,base1 :foreground ,orange-l))))
 ;;  `(magit-blame-date    ((t (:background ,base1 :foreground ,orange-l))))
 ;;  `(magit-blame-summary ((t (:background ,base1 :foreground ,base02 :weight bold))))
 ;; ;;;;;; references etc.
 ;;  `(magit-dimmed         ((t (:foreground ,base01))))
 ;;  `(magit-hash           ((t (:foreground ,base01))))
 ;;  `(magit-tag            ((t (:foreground ,cyan :weight bold))))
 ;;  `(magit-branch-remote  ((t (:foreground ,green  :weight bold))))
 ;;  `(magit-branch-local   ((t (:foreground ,blue   :weight bold))))
 ;;  `(magit-branch-current ((t (:foreground ,blue   :weight bold :box t))))
 ;;  `(magit-head           ((t (:foreground ,blue   :weight bold))))
 ;;  `(magit-refname        ((t (:background ,base02 :foreground ,base01 :weight bold))))
 ;;  `(magit-refname-stash  ((t (:background ,base02 :foreground ,base01 :weight bold))))
 ;;  `(magit-refname-wip    ((t (:background ,base02 :foreground ,base01 :weight bold))))
 ;;  `(magit-signature-good      ((t (:foreground ,green))))
 ;;  `(magit-signature-bad       ((t (:foreground ,red))))
 ;;  `(magit-signature-untrusted ((t (:foreground ,yellow))))
 ;;  `(magit-cherry-unmatched    ((t (:foreground ,cyan))))
 ;;  `(magit-cherry-equivalent   ((t (:foreground ,magenta))))
 ;;  `(magit-reflog-commit       ((t (:foreground ,green))))
 ;;  `(magit-reflog-amend        ((t (:foreground ,magenta))))
 ;;  `(magit-reflog-merge        ((t (:foreground ,green))))
 ;;  `(magit-reflog-checkout     ((t (:foreground ,blue))))
 ;;  `(magit-reflog-reset        ((t (:foreground ,red))))
 ;;  `(magit-reflog-rebase       ((t (:foreground ,magenta))))
 ;;  `(magit-reflog-cherry-pick  ((t (:foreground ,green))))
 ;;  `(magit-reflog-remote       ((t (:foreground ,cyan))))
 ;;  `(magit-reflog-other        ((t (:foreground ,cyan))))

 )


;; (custom-theme-set-variables
;;  'eide)

(provide 'eide-theme)
