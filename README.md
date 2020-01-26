# virtual-auto-fill

`virtual-auto-fill` is an [Emacs Lisp Package](https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html) to display unfilled text in a readable way.  It wraps the text as if you had inserted line breaks (e.g. using `fill-paragraph` or `auto-fill-mode`) without actually modifying the underlying buffer.  It also indents paragraphs in bullet lists properly.

Specifically, `adaptive-wrap-prefix-mode`, `visual-fill-column-mode` and `visual-line-mode` are used to wrap paragraphs and bullet lists between the wrap prefix and the fill column.
