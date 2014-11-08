(require-package 'markdown-mode)

(auto-major-mode 'markdown-mode
                 "\\.markdown\\'" "\\.md\\'")

(require-package 'websocket)

(require-old-package 'realtime-markdown-viewer t)
(require 'realtime-markdown-viewer
         (expand-file-name "ermv/realtime-markdown-viewer.el"
                           old-packages-directory))
(setq rtmv:lang 'ruby)

