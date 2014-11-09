This emacs config is used to be my own tutorial. It becomes more reasonable and robust day by day. I'm adding documentation to it whenever I'm free. Feel free to try it and make it better.

This config is loyal to emacs's default keys. It is easy to learn and use.

**Features include:**
* Editing text and code
  * Tons of convinience commands
  * Syntax highlighting
  * Auto complete
  * Snippet
  * Per language support
* Reading text and code
  * Code folding
  * Code outline
* Project management
  * Visit a project
  * open file in project
  * jumping between files

Editing Features
================

## Movement

This is the base movement for every mode.
Any mode may override the default movement key bindings.

### Basic movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| C-f | forward-char | Yes | C source code |
| C-b | backward-char | Yes | C source code |
| C-n | next-line | Yes | simple.el |
| C-p | previous-line | Yes | simple.el |

### Line level movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| C-a | move-beginning-of-line | Yes | simple.el |
| C-e | move-end-of-line | Yes | simple.el |
| M-m | back-to-indentation | Yes | simple.el |

### Block level movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| C-M-f | forward-sexp | Yes | lisp.el |
| C-M-b | backward-sexp | Yes | lisp.el |
| C-M-d | down-list | Yes | lisp.el |
| C-M-u | backward-up-list | Yes | lisp.el |
| C-M-n | forward-list | Yes | lisp.el |
| C-M-p | backward-list | Yes | lisp.el |
| C-M-a | beginning-of-defun | Yes | lisp.el |
| C-M-e | end-of-defun | Yes | lisp.el |

### Window level movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| C-l | recenter-top-bottom | Yes | window.el |
| M-r | move-to-window-line-top-bottom | Yes | window.el |
| C-v | scroll-up-command | Yes | window.el |
| M-v | scroll-down-command | Yes | window.el |

### Buffer level movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| M-< | beginning-of-buffer | Yes | simple.el |
| M-> | end-of-buffer | Yes | simple.el |
| M-g c | goto-char | Yes | C source code |
| M-g g | goto-line | Yes | simple.el |

### Move with mark

| key | function or description | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| C-@ with prefix argument | jump to the mark | Yes | simple.el |
| C-x C-@ | pop-global-mark | Yes | simple.el |
| C-x C-x | exchange-point-and-mark | Yes | simple.el |

## Selection

The selection equivalent in emacs is called "mark". Setting the mark alters the region, which is the text between point and mark. Selection in emacs is all about "mark" and "region".

### Basic Selection

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| C-@ | set-mark-command | Yes | simple.el |

### Advanced Selection
| key | function | default | required | package |
|:-----:|:---------------:|:---------:|:------:|:----:|
| C-z | er/expand-region | No | setup/editing-additional.el | expand-region |


### Convinience Mark

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| C-x h | mark-whole-buffer | Yes | simple.el |

## Insertion

## Deletion

# Reading Features

# Project Management Features

# Programming Language Support

## Ruby

### Test

| TDD prefix | Description  |
|:--------------------:|:---------------------------:|
| C-c r | rspec prefix |
| C-c t | minitest prefix |

# Escape Emacs
| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| C-x C-z | suspend-frame | Yes[^1] | frame.el |
| C-x C-c | save-buffers-kill-terminal | Yes | files.el |

[^1]: By default, 'suspend-frame' is bound to both C-z and C-x C-z. Currently C-z is overrided by another feature.
