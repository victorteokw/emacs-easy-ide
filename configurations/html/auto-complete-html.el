(defvar html-root-element-list
  (list
   "html" "!DOCTYPE html"))

(defvar html-first-child-element-list
  (list
   "head" "body"))

(defvar html-unique-element-list
  (list
   "html" "head" "body" "title"))

(defvar html-block-level-element-list
  (list
   "address" "article" "aside" "audio" "blockquote" "canvas" "dd" "div" "dl"
   "fieldset" "figcaption" "figure" "footer" "form" "h1" "h2" "h3" "h4" "h5"
   "h6" "header" "hgroup" "hr" "noscript" "ol" "output" "p" "pre" "section"
   "table" "tfoot" "ul" "video"))

(defvar html-inline-element-list
  (list
   "b" "big" "i" "small" "tt"
   "abbr" "acronym" "cite" "code" "dfn" "em" "kbd" "strong" "samp" "var"
   "a" "bdo" "br" "img" "map" "object" "q" "script" "span" "sub" "sup"
   "button" "input" "label" "select" "textarea"))

(defvar html-all-element-list
  (list
   "a" "abbr" "acronym" "address" "applet" "area" "article" "aside" "audio"
   "b" "base" "basefont" "bdi" "bdo"
   "bgsound" "big" "blink" "blockquote" "body" "br" "button"
   "canvas" "caption" "center" "cite" "code" "col" "colgroup"
   "command" "content" "data" "datalist" "dd" "decorator" "del"
   "details" "dfn" "dialog" "dir" "div" "dl" "dt"
   "element" "em" "embed" "fieldset" "figcaption" "figure" "font"
   "footer" "form" "frame" "frameset" "head" "header"   "hgroup"
   "hr" "href" "html" "i" "iframe" "image" "img"
   "input" "ins" "isindex" "kbd" "keygen" "label" "legend"
   "li" "link" "listing" "main" "map" "mark" "marquee"
   "menu" "menuitem" "meta" "meter" "multicol" "nav" "nextid"
   "nobr" "noembed" "noframes" "noscript" "object" "ol" "optgroup"
   "option" "output" "p" "param" "picture" "plaintext" "portfolio"
   "pre" "progress" "q" "rb" "rp" "rt" "ruby"
   "s" "samp" "script" "section" "select" "shadow" "small"
   "source" "spacer" "span" "strike" "strong" "style" "sub"
   "summary" "sup" "svg" "table" "tbody" "td" "template"
   "textarea" "tfoot" "th" "thead" "time" "title" "tr"
   "track" "tt" "u" "ul" "var" "video" "wbr" "xmp"
   ))

(defvar html-user-defined-class-list
  ())

(defvar html-user-defined-id-list
  ())

(defun ac-source-html-tag-candidates ()
  (message "ac-source-candidates triggered")
  (let ((doctype-declared nil) (html-declared nil)
  	(head-declared nil) (body-declared nil)
  	(html-doctype-level nil) (head-body-level nil)
  	(inside-head-1-level nil) (inside-body-1-level nil))
    ;; inside head or not
    (message "before test")
    ;; (and (save-excursion (re-search-backward "<head" nil t))
    ;; 	 (save-excursion (re-search-forward "</head>" nil t))
    ;; 	 (setq inside-head-1-level t))
    ;; (message "finish head test")
    ;; ;; inside body or not
    ;; (and (save-excursion (search-backward-regexp "<body" nil t))
    ;; 	 (save-excursion (search-forward-regexp "</body>" nil t))
    ;; 	 (setq inside-body-1-level t))
    ;; (message "finish body test")
    ;; ;; inside html or not
    ;; (or inside-head-1-level inside-body-1-level
    ;; 	(and (save-excursion (search-backward-regexp "<html" nil t))
    ;; 	     (save-excursion (search-forward-regexp "</html>" nil t))
    ;; 	     (setq head-body-level t)))
    ;; (message "finish html test")
    ;; ;; must be top level
    ;; (or head-body-level
    ;; 	(setq html-doctype-level t))
    ;; (message "finish set top level")
    ;; ;; if top level check if doctype and html declared
    ;; (if html-doctype-level
    ;; 	(progn
    ;; 	  (and (save-excursion (goto-char (point-min))
    ;; 			      (search-forward-regexp "<!DOCTYPE" nil t))
    ;; 	       (setq doctype-declared t))
    ;; 	  (and (save-excursion (goto-char (point-min))
    ;; 			       (search-forward-regexp "<html" nil t))
    ;; 	       (setq html-declared t))))
    ;; ;; if head or body level
    ;; (if head-body-level
    ;; 	(progn
    ;; 	  (and (save-excursion (goto-char (point-min))
    ;; 			       (search-forward-regexp "<head" nil t))
    ;; 	       (setq head-declared t))
    ;; 	  (and (save-excursion (goto-char (point-min))
    ;; 			       (search-forward-regexp "<body" nil t))
    ;; 	       (setq body-declared t))))
    ;; (message "next cond")
    ;; ;; (or inside-head-1-level (message "Not inside head level"))
    ;; ;; (or inside-body-1-level (message "Not inside body level"))
    ;; ;; (or head-body-level (message "Not Head body level"))
    ;; ;; (or html-doctype-level (message "Not html doctype level"))
    ;; (cond (html-doctype-level (html-root-element-list))
    ;; 	  (head-body-level (html-first-child-element-list))
    ;; 	  (inside-head-1-level (html-all-element-list))
  	  ;; (inside-body-1-level (html-all-element-list)))
  html-all-element-list))

(defun ac-source-html-tag-documentation (symbol)
  "Currently not documented."
)

(defun ac-source-html-attribute-candidates ()
  (list "id" "class" "href" "src" "ref" "link" "title")
  )

(defvar ac-source-html-tag
  '((candidates . ac-source-html-tag-candidates)
    (prefix . "<\\(.*\\)")
    (symbol . "t")
    (document . ac-source-html-tag-documentation)))

(defvar ac-source-html-attribute
  '((candidates . (ac-source-html-attribute-candidates))
    (prefix . "[\\w\"]+[ ]+\\(.*\\)")
    (symbool . "a")))

(defvar ac-source-html-attribute-2
  '((candidates . (ac-source-html-attribute-candidates))
    (prefix . "\\w+[ ]+\\(.*\\)")
    (symbol . "a")))

(defun setup-html-environment ()
  "Setup html development environment."
  (require-package 'auto-complete)
  (setq ac-sources '(ac-source-html-attribute))
  (add-to-list 'ac-sources 'ac-source-html-tag)
  (add-to-list 'ac-sources 'ac-source-html-attribute-2)
  (auto-complete-mode t)
  (setq-default ac-auto-start t)
  (setq-default ac-dwim t)
  )

(defun setup-css-environment ()
  "Setup css development environment."
  (require-package 'auto-complete)
  ;; TODO: set css compatible ac sources
  )

(add-hook 'html-mode-hook 'setup-html-environment)


(provide 'init-html)
