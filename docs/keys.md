# Key cheatsheet

The design of key strokes of this emacs configuration is combine the best of
Emacs and modern text editors such as Sublime Text, TextMate and Atom.

It should be as efficient as Emacs, as convinient as modern text editors.

Most of commands has two equivalent key bindings, one for terminal Emacs,
one for window system Emacs. Which means: it should work on terminal as plain
emacs, it should work on window system as modern text editor.

## Editor basic

### Executing command

|emacs-key|command|ns-equivalent|ns-key|
|:---:|:----------:|:------:|:------:|
|<kbd>M-x</kbd>|smex|| <kbd>⇧⌘p</kbd> |

### Cancel

|emacs-key|command|ns-equivalent|ns-key|
|:---:|:----------:|:------:|:------:|
|<kbd>C-g</kbd>|keyboard-quit|| <kbd>esc</kbd> |

### Cycle configuration files

|emacs-key|command|ns-equivalent|ns-key|
|:---:|:----------:|:------:|:------:|
|<kbd>C-f11</kbd>|hy-core-conf-goto-user-init-file|| <kbd>esc</kbd> |

## Insertion

Most keys insert themselves through `self-insert-command`.

Every mode has different insertion commands.

## Movement

### Basic movement

|emacs-key|command|emacs-default|ns-equivalent|ns-key|
|:---:|:----------:|:------:|:---:|:------:|
|<kbd>C-f</kbd>|forward-char| yes| right-char| <kbd>right</kbd> |
|<kbd>C-b</kbd>|backward-char|yes| left-char| <kbd>left</kbd> |
|<kbd>C-n</kbd>|next-line|yes | |<kbd>down</kbd> |
|<kbd>C-p</kbd>|previous-line|yes| | <kbd>up</kbd> |

### Same line movement

|emacs-key|command|emacs-default|ns-equivalent|ns-key|
|:---:|:----------:|:------:|:---:|:------:|
|<kbd>C-e</kbd>|move-end-of-line| yes| |  |
|<kbd>C-a</kbd>| |no|  |  |

### Move by screen aka scroll

|emacs-key|command|emacs-default|ns-equivalent|ns-key|
|:---:|:----------:|:------:|:---:|:------:|
|<kbd>C-v</kbd>|scroll-up-command| yes| |  |
|<kbd>M-v</kbd>| scroll-down-command |yes|  |  |

### Exact movement do want you mean

|emacs-key|command|emacs-default|ns-equivalent|ns-key|
|:---:|:----------:|:------:|:---:|:------:|
|<kbd>C-;</kbd>|ace-jump-char-mode| no| |  |
|<kbd>C-M-;</kbd>|ace-jump-line-mode |no|  |  |
|<kbd>C-:</kbd>|ace-jump-word-mode |no|  |  |

### Move back and forth

|emacs-key|command|emacs-default|ns-equivalent|ns-key|
|:---:|:----------:|:------:|:---:|:------:|
|<kbd>C-@</kbd>|set-mark-command| yes| hy-core-editor-remember-position | <kbd>⌘y</kbd> |
|<kbd>C-@</kbd>|set-mark-command |yes| hy-core-editor-last-cursor-position | <kbd>⌘u</kbd> |

## Selection

## Deletion
