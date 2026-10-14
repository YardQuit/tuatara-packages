# outlook

Outlook items, saved mail and Outlook mailboxes, read as messages, read only.

Made for examining mail from evidence: nothing in a message is run, its
attachments are opened only on asking and from private copies, and the files
read are never changed.

## Loading it

In `~/.config/tuatara/init.lisp`, with this repository named as a source:

```lisp
(load-package "outlook" :source "yardquit")
```

Once it is loaded, opening one of these files shows it as a message, or a
list of messages:

| File | Shown as |
|---|---|
| `.msg` (an Outlook item) | the message: its headers, text and attachments |
| `.eml` (a saved mail) | the message, its parts read as MIME |
| `.pst`, `.ost` (an Outlook mailbox) | its folders, and under each its messages, a line each: when sent, from whom, how many attachments, the subject |

Rimu's preview shows them too, and `M-x check-health` says what reads them
here.

## Keys

| Where | Key | What it does |
|---|---|---|
| A message | `RET`, a click | Opens the attachment at point here, from a copy only you can read: an attached message as a message; a PDF, an Office document or a picture as rimu shows it, as pages or as the picture, drawn shut in the sandbox; anything else safely, read only. |
| A message | `O` | Opens the attachment at point in the desktop's own program for its kind, after asking: a copy only you can read, and only a kind that may be opened outside Tuatara (a picture, a PDF, a sound, a video, an Office document). That program is not shut in the sandbox. |
| A message | `P` | Shows it as it looks: its HTML drawn as pages, its pictures in place, nothing fetched from the internet, no script run (see Seeing a message as it looks). `T` there comes back to the message. |
| A message | `T` | Shows the message's file as its text: opened safely when the message came from a mailbox or an attachment, else as any file is opened. |
| A mailbox | `RET`, `l` | Opens the item on the line: a message as one, read only, the matches of the list's filters lit in it; a contact or an appointment as its text, safely. On a folder's line, folds or unfolds the folder. |
| A mailbox | `TAB`, `S-TAB` | Folds or unfolds the folder the line is in; every folder. |
| A mailbox | `m`, `u`, `U` | Marks the message on the line (`>`) and moves down; takes its mark off; takes every mark off. A count marks as many. |
| A mailbox | `C` | Copies the messages marked, or the one on the line, out into a folder: asks first for the format, `eml` (saved mail) or `msg` (Outlook item), then for the folder (made when it is not there). |
| A mailbox | `C-s`, `C-r` | Narrows the list, as a regular expression is typed, to the messages whose headers or contents match (see Searching a mailbox). `RET` keeps the pattern as a filter, `C-g` lets the one typed go. |
| A mailbox | `C-x / l` | Lists the filters narrowing the list, each with how many items it leaves; there `d` takes away the filter on the line and `D` all of them. |
| A mailbox | `C-x / r` | Takes every filter away: the whole list again. |
| A message | `q` | Closes it, back to the mailbox (or wherever it was opened from); `h` moves the cursor left, as everywhere. |
| A mailbox | `q` | Closes it. |
| A mailbox | `h` | Asks, then closes it and goes back to its folder in rimu, on the mailbox. The moves by word (`w`, `b`, `W`, `B`) do nothing in the list; `V` is the one selection, of whole lines. |

## Mailboxes

A mailbox is read by `readpst`, of the libpst package, shut in Tuatara's
sandbox: no network, and none of your files but the mailbox. Its messages are
written to your cache folder, each twice, as saved mail and as an Outlook
item, so that they can be copied out as either; they are deleted when the
list is closed. Install libpst from your system's packages
(`readpst` on the `PATH` of the system's own program folders).

## Seeing a message as it looks

`P` in a message draws it as a mail program shows it: its HTML laid out as
on a screen, its colours, tables and fonts, the pictures it carries in their
places, under its headers (from, to, subject, dates, attachments) as a mail
program prints it, and its internet headers whole, as they came, on a page
of their own after it. The pages are shown in Tuatara's PDF view, with its
keys (zoom, search, the text). The message's HTML comes from the message itself: the HTML part
of a saved mail, or an Outlook item's HTML, or the HTML its rich text keeps.
A message of plain text alone looks as it is shown already.

It is drawn by [WeasyPrint](https://weasyprint.org), shut in Tuatara's
sandbox as readpst is: no network, so nothing is fetched from the internet (a
picture, a style, a tracking pixel) and the sender cannot learn the message
was opened; the line over the pages says how many such things it holds.
WeasyPrint runs no scripts at all. It sees only the message's HTML and the
pictures it shows, written into a folder of your cache only you can read, and
your fonts, read only. Install it from your system's packages: `weasyprint`
on Fedora, Debian and Ubuntu, `python-weasyprint` on Arch, `python3-weasyprint`
on openSUSE. The pages need poppler's `pdftoppm` too, as any PDF does.

## Copying messages out

`C` copies the messages marked with `m` (or, with none marked, the one on the
line) out of the mailbox into a folder you choose, each a file of its own:
as saved mail (`.eml`, internet mail as readpst wrote it) or as Outlook items
(`.msg`, written by readpst from the mailbox itself, beside each `.eml`). A
copy is named by when the message was sent and its subject,
`2001-04-30 2031 unit for sale.msg`; a name already taken gets `(2)`, `(3)`
and so on, and nothing in the folder is overwritten. Characters a name cannot
hold on other systems' drives (`: * ? " < > | / \`) are left out or made `-`.
Copying runs in the background; the message line then says how many were
copied and which could not be. Marks stay on messages a filter hides, and
`C` copies them too: the question names how many it copies.

## Searching a mailbox

`C-s` (or `C-r`) in a mailbox's list does not move through its lines: it
narrows the list, as the pattern is typed, to the messages it matches. A
message matches when the pattern, a regular expression, is found in any of
its headers (all of them, not only those the list shows: `^X-Mailer:.*Outlook`,
`^Received:.*203\.0\.113`), in the text of any of its text parts, decoded
from base64 or quoted-printable and from its charset (HTML as its text), or in
an attachment's name; messages attached to it are looked through alike. Case
is ignored while the pattern has no capital letter; `M-r` switches to
literal text.

`RET` keeps the pattern as a filter, and the list's first line says by what
it is narrowed and how many items are left. The next `C-s` narrows what that
filter left, so filters stack: `invoice` then `^from:.*acme` leaves the
messages that have both. `C-g` lets go only the pattern being typed.
`C-x / l` lists the filters, to take one away (the later ones are worked out
again), and `C-x / r` takes them all away. `RET` on a message of the
narrowed list opens it with the filters' matches lit.

The text searched is made once, as the mailbox is listed: for each message,
in the list's own private folder, deleted with it. Matching runs in the
background, the mode line saying how far it has got, and starts again as the
pattern changes; the editor goes on meanwhile, however large the mailbox. Of
a message larger than `*outlook-search-text-limit*` only that much is
searched, and of one larger than `*outlook-largest-file*` only its headers;
the list says how many such messages there are. Nothing in a message is run,
attachments other than text are never decoded, and the mailbox and what
readpst wrote are never changed.

## Settings

| Setting | Standard | What it is |
|---|---|---|
| `*outlook-readpst-program*` | `"readpst"` | The program that reads mailboxes. |
| `*outlook-page-program*` | `"weasyprint"` | The program that draws a message as it looks. |
| `*outlook-include-deleted*` | `t` | Lists the items a mailbox still holds that were deleted, in the folders they were deleted from; `nil` for only those not deleted. |
| `*outlook-largest-file*` | 256 MB | The most an item or saved mail, or an attachment in one, may hold to be read as a message; a larger one opens as any file. |
| `*outlook-nesting-limit*` | `8` | How many levels of messages attached to messages are shown as messages; deeper ones are listed as not read. |
| `*outlook-mailbox-seconds*` | `3600` | The most seconds readpst may take; it is stopped then, and what it wrote is listed. |
| `*outlook-search-text-limit*` | 16 million | The most characters of a mailbox's message its list's search looks through; a larger one is searched that far, and the list counts them. |
| `*outlook-mailbox-space*` | 32 GB | The most readpst may write; it is stopped there, or when the drive has less than 1 GB free. |

Set them in the package's `:config`:

```lisp
(load-package "outlook" :source "yardquit"
  :config
  (setf *outlook-include-deleted* nil))
```

## Tests

`tools/test.sh` at the top of this repository runs them, with the others.

## Credits

- Microsoft's Open Specifications, MS-CFB and MS-OXMSG: how an Outlook item is
  read.
- libpst's `readpst`: how a mailbox is read.
- WeasyPrint: how a message is drawn as it looks.

## License

GPL-3.0-or-later; see `LICENSE` at the top of this repository.
