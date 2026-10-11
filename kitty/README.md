# kitty

Windows of the kitty terminal, opened, read and typed into from Tuatara.

A kitty window here is one of a kitty that Tuatara starts itself, never your
own kitty: drawn by the graphics card, with kitty's ligatures and pictures.
Tuatara talks to it over a socket of its own in your runtime folder, and only
there. Remote control is allowed over that socket alone
(`allow_remote_control socket-only`, given on kitty's command line, which no
configuration file can undo), so a program run in the window, or a file shown
in it, cannot steer it.

## Loading it

In `~/.config/tuatara/init.lisp`, with this repository named as a source:

```lisp
(load-package "kitty" :source "yardquit")
```

It needs the `kitty` program on your `PATH`.

## Commands

| Command | What it does |
|---|---|
| `M-x kitty` | Opens a kitty window in the current buffer's folder, with your shell; beside the editor's window when Tuatara itself runs in such a kitty. |
| `M-x kitty-text` | Brings all the text of a kitty window, scrollback too, into a buffer with its colours, to move through, select, search and copy. `r` there reads it again. |
| `M-x kitty-input` | Types the selection into a kitty window, or without one the current line and a return, so that the shell runs it. |

With several windows open, the last two ask which one.

## Tuatara in a kitty of its own

```sh
kitty/tuatara-kitty [--program KITTY] [FILE...]
```

runs Tuatara in a kitty of its own, editing the files given, with `M-x kitty`
opening windows beside the editor's. The editor run is `$TUATARA`, else the
repository's `tools/tuatara-tui.sh`, else `tuatara` on the `PATH`.

## Its looks

Your own `kitty.conf`, then `~/.config/tuatara/kitty.conf` over it, made the
first time with examples only.

## Settings

| Setting | Standard | What it is |
|---|---|---|
| `*kitty-program*` | `"kitty"` | The kitty program windows are opened with. Set it after the package is loaded, in its `:config`. |

## Safety

A window runs only what you type there. A program you run in it runs as you,
and like any such program may reach the socket; what a window only shows (a
file's escape sequences) cannot.

## Tests

`tools/test.sh` at the top of this repository runs them, with the others.

## Credits

kitty is by Kovid Goyal; this package drives it by its remote control.

## License

GPL-3.0-or-later; see `LICENSE` at the top of this repository.
