q major mode for emacs.

* supports [tramp](https://www.emacswiki.org/emacs/TrampMode)
* supports [babel](https://orgmode.org/worg/org-contrib/babel/intro.html)
* supports "send-to-q" command bindings for editor windows
* supports automatic window sizing `\\c`
* supports in-line images/graphs

![screenshot](e.png)

### Installation

* `(load "/path/to/q-mode.el")`
* `(add-to-list 'auto-mode-alist '("\\.q" . q-mode))`
* `M-x q` to create a q process
