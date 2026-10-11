# garamond

One space or two after a full stop, kept by the document itself.

How far apart sentences are is a property of a document, so the document says
it in its own `Local Variables:` block:

```
# Local Variables:
# sentence-end-double-space: t
# End:
```

A file that says nothing is left alone. Typing, filling and the sentence
commands then follow the spacing the file asks for.

## Loading it

In `~/.config/tuatara/init.lisp`, with this repository named as a source:

```lisp
(load-package "garamond" :source "yardquit")
```

## Commands

| Command | What it does |
|---|---|
| `M-x garamond-mode` | Makes one space typed after a sentence's end two, while the buffer's sentences are two spaces apart. On by itself in a file that says its spacing. The mode line says `2S`, or `1S` while they are one apart. |
| `M-x garamond-set-spacing` | Says how many spaces, 1 or 2, are between the buffer's sentences, changing none of its text, and offers to write it in the file. A count gives it: `C-u 2`. |
| `M-x garamond-adjust-spacing` | Makes every gap after a sentence's end in the selection, or the whole buffer, 1 or 2 spaces as one change, then says so as `garamond-set-spacing` does. |
| `M-x garamond-follow-declarations-mode` | Turns on or off `garamond-mode` starting by itself in files that say their spacing. |
| `M-x garamond-doctor` | Shows in `*Garamond*` what the package does in the current buffer and why: what the file says, the spacing in force, where a second space goes, the settings. |

## Where a second space goes

Only where prose is written: in a text mode (Org, Markdown, plain text and
the rest), never in code, tables, links, maths or a block's contents, as Org's
parse or the language's syntax tree tells them; in any other language only in
its comments. Where a language has a syntax tree that is not ready yet,
nothing is doubled. A word whose full stop shortens it (e.g., Dr., Sept.) ends
no sentence.

## Settings

| Setting | Standard | What it is |
|---|---|---|
| `*garamond-extra-abbreviations*` | `'()` | Abbreviations of your own whose full stop ends no sentence: `'("Ph.D." "et al.")`. |
| `*garamond-removed-abbreviations*` | `'()` | Abbreviations of the package's that should end a sentence after all: `'("St." "No.")`. |
| `*garamond-double-space-on-typing*` | `t` | `nil` leaves typing alone; the spacing is still said in the mode line, bracketed: `(2S)`. |
| `*garamond-persist*` | `t` | `nil` never asks to write the spacing in the file. A file that says already is kept up to date. |
| `*garamond-follow-declarations*` | `t` | `nil` leaves `garamond-mode` off in files that say their spacing, until asked for. |
| `*garamond-mode-line*` | `t` | `nil` says nothing in the mode line. |
| `*garamond-unsuitable-modes*` | `'(special-mode)` | Major modes the mode never runs in, those made from them included. |

Set them in the package's `:config`:

```lisp
(load-package "garamond" :source "yardquit"
  :config
  (setf *garamond-extra-abbreviations* '("Ph.D.")))
```

## Tests

`tools/test.sh` at the top of this repository runs them, with the others.

## Credits

Made from the Emacs package of the same name, by the same author:
[github.com/YardQuit/garamond](https://github.com/YardQuit/garamond).

## License

GPL-3.0-or-later; see `LICENSE` at the top of this repository.
