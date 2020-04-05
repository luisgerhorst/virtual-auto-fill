# virtual-auto-fill

`virtual-auto-fill` is an [Emacs Lisp Package](https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html) to display unfilled text in a readable way.  It wraps the text as if you had inserted line breaks (e.g. using [`fill-paragraph`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Fill-Commands.html) or `auto-fill-mode`) without actually modifying the underlying buffer.  It also indents paragraphs in bullet lists [properly](https://stackoverflow.com/questions/13559061/emacs-how-to-keep-the-indentation-level-of-a-very-long-wrapped-line).

Internally, [Adaptive Wrap Prefix mode](http://elpa.gnu.org/packages/adaptive-wrap.html) mode, [Visual Fill Column mode](https://github.com/joostkremers/visual-fill-column), and [Visual Line mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Visual-Line-Mode.html) are employed to wrap paragraphs and bullet lists between the wrap prefix and the fill column. If you don't need all of the features provided by Virtual Auto Fill mode, it may be better for you to use a subset of these modes directly. However, if you often switch between filled and unfilled text, Virtual Auto Fill mode catches you when you accidentally invoke `fill-paragraph` (by default bound to `M-q`) out of habit in a virtually filled buffer. To prevent you from breaking the formatting convention without noticing it, you are asked whether you actually meant to fill the paragraph and if you want to be warned again next time.

## Motivation

A common reason to edit unfilled text in Emacs are mails that are intended to look nice on mobile devices. If the display area is too narrow, filled lines have to be broken up a second time resulting in [alternating long and short visual lines](./README-filled-mail-portrait.jpeg) (compare this to [the same mail viewed in landscape](./README-filled-mail-landscape.jpeg)). [`format=flowed`](https://tools.ietf.org/html/rfc3676) was meant to fix this, but unfortunately it is [neither used by everyone who writes filled mails, nor is it supported by every mail client](https://fastmail.blog/2016/12/17/format-flowed/) (e.g. neither iOS Apple Mail nor FastMail support it). When communicating whith people who are likely using a GUI Mail client with a non-monospace font, you may therefore choose to just leave the text unfilled and use Virtual Auto Fill mode to edit it (which is exactly what the popular mail clients do).

## Screenshots

Without `virtual-auto-fill-mode`, only a small portion of the text file is visible:

![lines are truncated](./README-screenshot-default.png "A plain text file viewed without virtual-auto-fill-mode")

`virtual-auto-fill` displays the buffer as if you had filled it, but without actually modifying it:

![text wraps at 80 characters, continuation lines in bullet list are indented](./README-screenshot-virtual-auto-fill.png "A plain text file viewed with virtual-auto-fill-mode enabled")

## Installation

After installation, the command `virtual-auto-fill-mode` which enables the mode is autoloaded, you can call it without any additional configuration!

### [straight.el](https://github.com/raxod502/straight.el)

Having [set up straight.el](https://github.com/raxod502/straight.el#getting-started), add the following to your `.emacs.d/init.el`, then restart your Emacs:

``` emacs-lisp
(straight-use-package
 '(virtual-auto-fill
   :type git
   :host github
   :repo "luisgerhorst/virtual-auto-fill"))
```

For a complete example configuration see [`example-configs/straight/init.el`](./example-configs/straight/init.el). You can omit the parts already included in your `init.el`.

### MELPA

After [adding MELPA to your `package-archives`](https://melpa.org/#/getting-started), run `M-x package-install virtual-auto-fill` once.

See [`example-configs/melpa/init.el`](./example-configs/melpa/init.el) for a complete config (copying not recommened thought).

## Activation

To enable Virtual Auto Fill mode automatically in Markdown files, add the following after also installing `markdown-mode`:

``` emacs-lisp
(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook #'virtual-auto-fill-mode))
```

## Testing

The following serves as documentation for contributors to easy development. If you just want to use this package as a end user this should not concern you.

Load the example configuration into a vanilla instance of Emacs, this will install straight.el to `./example-config/straight`:

``` shell
make CONFIG=straight test
```

Run with `CONFIG=melpa` to test the MELPA configuration. Refer to the `Makefile` for additional commands that may be useful.
