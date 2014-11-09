emacs-easy-ide
==============

This emacs config is used to be my own tutorial.

It becomes more reasonable and robust day by day.

I'm adding documentation to it whenever I'm free.

Feel free to try it and make it better.

This config is loyal to emacs's default keys.

It is easy to learn and use.

# Editing Features

## Movement

This is the base movement for every mode.
Any mode may override the default movement key bindings.

### Basic movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| C-f | forward-char | true | C source code |
| C-b | backward-char | true | C source code |
| C-n | next-line | true | simple.el |
| C-p | previous-line | true | simple.el |

### Line level movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| C-a | move-beginning-of-line | true | simple.el |
| C-e | move-end-of-line | true | simple.el |
| M-m | back-to-indentation | true | simple.el |

### Block level movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| C-M-f | forward-sexp | true | lisp.el |
| C-M-b | backward-sexp | true | lisp.el |
| C-M-d | down-list | true | lisp.el |
| C-M-u | backward-up-list | true | lisp.el |
| C-M-n | forward-list | true | lisp.el |
| C-M-p | backward-list | true | lisp.el |
| C-M-a | beginning-of-defun | true | lisp.el |
| C-M-e | end-of-defun | true | lisp.el |

### Window level movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| C-l | recenter-top-bottom | true | window.el |
| M-r | move-to-window-line-top-bottom | true | window.el |
| C-v | scroll-up-command | true | window.el |
| M-v | scroll-down-command | true | window.el |

### Buffer level movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| M-< | beginning-of-buffer | true | simple.el |
| M-> | end-of-buffer | true | simple.el |
| M-g c | goto-char | true | C source code |
| M-g g | goto-line | true | simple.el |

## Selection

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

