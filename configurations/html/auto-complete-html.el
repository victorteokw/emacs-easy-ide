

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

(defun ac-source-html-tag-candidates ()

  html-all-element-list)

(defun ac-source-html-attribute-candidates ()
  (list "id" "class" "href" "src" "ref" "link" "title")
  )

(defvar ac-source-html-tag
  '((candidates . (ac-source-html-tag-candidates))
    (prefix . "<\\(.*\\)")
    (symbol . "t")))

(defvar ac-source-html-attribute
  '((candidates . (ac-source-html-attribute-candidates))
    (prefix . "[\\w\"]+[ ]+\\(.*\\)")
    (symbool . "a")))

(defvar ac-source-html-attribute-2
  '((candidates . (ac-source-html-attribute-candidates))
    (prefix . "\\w+[ ]+\\(.*\\)")
    (symbol . "a")))

(defun setup-html ()
  "Setup html."
  (require-package 'auto-complete)
  (setq ac-sources '(ac-source-html-attribute))
  (add-to-list 'ac-sources 'ac-source-html-tag)
  (add-to-list 'ac-sources 'ac-source-html-attribute-2)
  (auto-complete-mode t)

  )

(add-hook 'html-mode-hook 'setup-html)

(provide 'init-html)
