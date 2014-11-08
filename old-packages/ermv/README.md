# Emacs Realtime Markdown Viewer

Emacs Realtime Markdown Viewer with websocket.el and Sinatra.

## Requirements
* Emacs 23 or higher. (version 24 is better than version 23)
* Latest [websocket.el](https://github.com/ahyatt/emacs-websocket)
    - websocket.el older than 2012/SEP/01 does not support multibyte characters
* Ruby 1.9.3+

## Demonstration

[emacs-realtime-markdown-viewer test on Vimeo](https://vimeo.com/53631510)

## Setup

### Ruby side (server)

```
$ bundle install --path=vender/bundler
```

### Emacs side (client)

```lisp
(require 'realtime-markdown-viewer)
```

Run WebApp and connect to Web application

    M-x realtime-markdown-viewer-mode

### Browser

Access to http://0.0.0.0:5021/

Limitation
----------

 **There are some bugs.**
