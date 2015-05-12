# Notice
I decide to use [Steve Purcell's emacs config](https://github.com/purcell/emacs.d), so I will not update this repository in sometime. 
I recommend you to use [this config](https://github.com/purcell/emacs.d). 

## Old README.md for this config

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
  * git client

Engines
=======

This config includes many useful emacs package that does or doesn't come with emacs.
* ido, flx-ido, ido-ubiquitous, smex: better interface with find-file and M-x commands.
* projectile: project management.
* helm: better interface with search.
* auto-coplete: auto complete engine.
* yasnippet: snippet engine.
* magit: git client.
* anzu: better interface with search and replace.
* ace-jump-mode: very fast movement.

Editing Features
================

## Movement

This is the base movement for every mode.
Any mode may override the default movement key bindings.

### Basic movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| <kbd>C-f</kbd> | forward-char | Yes | C source code |
| <kbd>C-b</kbd> | backward-char | Yes | C source code |
| <kbd>C-n</kbd> | next-line | Yes | simple.el |
| <kbd>C-p</kbd> | previous-line | Yes | simple.el |

### Line level movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| <kbd>C-a</kbd> | move-beginning-of-line | Yes | simple.el |
| <kbd>C-e</kbd> | move-end-of-line | Yes | simple.el |
| <kbd>M-n</kbd> | back-to-indentation | No | simple.el |

### Flexible movement
| key | function | default | defined|
|:-----:|:---------------:|:---------:|:------:|
| <kbd>M-H c</kbd> | ace-jump-char-mode | No | ace-jump-mode.el |
| <kbd>M-H w</kbd> | ace-jump-word-mode | No | ace-jump-mode.el |
| <kbd>M-H l</kbd> | ace-jump-line-mode | No | ace-jump-mode.el |
| <kbd>M-H p</kbd> | ace-jump-mode-pop-mark | No | ace-jump-mode.el |

### Block level movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| <kbd>C-M-f</kbd> | forward-sexp | Yes | lisp.el |
| <kbd>C-M-b</kbd> | backward-sexp | Yes | lisp.el |
| <kbd>C-M-d</kbd> | down-list | Yes | lisp.el |
| <kbd>C-M-u</kbd> | backward-up-list | Yes | lisp.el |
| <kbd>C-M-n</kbd> | forward-list | Yes | lisp.el |
| <kbd>C-M-p</kbd> | backward-list | Yes | lisp.el |
| <kbd>C-M-a</kbd> | beginning-of-defun | Yes | lisp.el |
| <kbd>C-M-e</kbd> | end-of-defun | Yes | lisp.el |

### Window level movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| <kbd>C-l</kbd> | recenter-top-bottom | Yes | window.el |
| <kbd>M-r</kbd> | move-to-window-line-top-bottom | Yes | window.el |
| <kbd>C-v</kbd> | scroll-up-command | Yes | window.el |
| <kbd>M-v</kbd> | scroll-down-command | Yes | window.el |

### Buffer level movement

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| <kbd>M-<</kbd> | beginning-of-buffer | Yes | simple.el |
| <kbd>M-></kbd> | end-of-buffer | Yes | simple.el |
| <kbd>M-g c</kbd> | goto-char | Yes | C source code |
| <kbd>M-g g</kbd> | goto-line | Yes | simple.el |

### Move with mark

| key | function or description | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| <kbd>C-@ with prefix argument</kbd> | jump to the mark | Yes | simple.el |
| <kbd>C-x C-@</kbd> | pop-global-mark | Yes | simple.el |
| <kbd>C-x C-x</kbd> | exchange-point-and-mark | Yes | simple.el |

## Selection

The selection equivalent in emacs is called "mark". Setting the mark alters the region, which is the text between point and mark. Selection in emacs is all about "mark" and "region".

### Basic Selection

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| <kbd> C-@ </kbd> | set-mark-command | Yes | simple.el |

### Advanced Selection
| key | function | default | required | package |
|:-----:|:---------------:|:---------:|:------:|:----:|
| <kbd>C-z</kbd> | er/expand-region | No | setup/editing-additional.el | expand-region |


### Convinience Mark

| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| <kbd>C-x h<kbd> | mark-whole-buffer | Yes | simple.el |

## Insertion

### Multiple Cursors

All these command is required from package [multiple-cursors](https://github.com/magnars/multiple-cursors.el). All these command are configured in _setup/editing-additional.el_.

| key | function | description |
|:-----:|:---------------:|:------------------------:|
| <kbd>C-c c l</kbd> | mc/edit-lines | add cursors to each line of region. |
| <kbd>C-c c n</kbd> | mc/mark-next-like-this | add cursor to next appear that matches current region. |
| <kbd>C-c c p</kbd> | mc/mark-previous-like-this | add cursor to previous appear that matches current region. |
| <kbd>C-c c b</kbd> | mc/mark-all-like-this | Marks all parts of the buffer that matches the current region. |
| <kbd>C-c c a</kbd> | mc/mark-all-like-this-in-defun | Marks all parts of the current defun that matches the current region. |
| <kbd>C-c c r</kbd> | mc/mark-all-in-region | Prompts for a string to match in the region, adding cursors to all of them. |
| <kbd>C-c c g</kbd> | mc/mark-all-dwim | Tries to be smart about marking everything you want. Can be pressed multiple times. |

## Deletion

# Reading Features

# Project Management Features

## Version control with magit

| key | description | function |
|:---:|:-----------:|:--------:|
| <kbd>M-/</kbd> | start git client | magit-status |


# Programming Language Support

## Ruby

### irb

| key | description | function |
|:---:|:-----------:|:--------:|
| <kbd>C-c C-s</kbd> | start irb | inf-ruby |
| <kbd>C-c C-z</kbd> | switch between irb and code | ruby-switch-to-inf |
| <kbd>C-c C-b</kbd> | send a block of code to irb | ruby-send-block |
| <kbd>C-c C-r</kbd> | send code in region to irb | ruby-send-region |
| <kbd>C-c C-l</kbd> | make irb load a file | ruby-load-file |
| <kbd>C-c C-x</kbd> | send current definition to irb | ruby-send-defition |
| <kbd>C-x C-e</kbd> | send last sexp to irb | ruby-send-last-sexp |

### Test

| TDD prefix | Description  |
|:--------------------:|:---------------------------:|
| <kbd>C-c r</kbd> | rspec prefix |
| <kbd>C-c t</kbd> | minitest prefix |

# Escape Emacs
| key | function | default | defined |
|:-----:|:---------------:|:---------:|:------:|
| <kbd>C-x C-z</kbd> | suspend-frame | Yes[^1] | frame.el |
| <kbd>C-x C-c</kbd> | save-buffers-kill-terminal | Yes | files.el |

[^1]: By default, 'suspend-frame' is bound to both C-z and C-x C-z. Currently C-z is overrided by another feature.
