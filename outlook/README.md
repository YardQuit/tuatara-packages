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
| A message | `O` | Opens the attachment at point in the desktop's own program for its kind, after asking: a copy only you can read, and only a kind that may be opened outside Tuatara (a picture, a PDF, a sound, a video, an Office document). That program is not shut in the sandbox. |
| A message | `T` | Shows the message's file as its text: opened safely when the message came from a mailbox or an attachment, else as any file is opened. |
| A mailbox | `RET` | Opens the item on the line: a message as one, read only; a contact or an appointment as its text, safely. |
| Either | `q` | Closes it. |

## Mailboxes

A mailbox is read by `readpst`, of the libpst package, shut in Tuatara's
sandbox: no network, and none of your files but the mailbox. Its messages are
written to your cache folder. Install libpst from your system's packages
(`readpst` on the `PATH` of the system's own program folders).

## Settings

| Setting | Standard | What it is |
|---|---|---|
| `*outlook-readpst-program*` | `"readpst"` | The program that reads mailboxes. |
| `*outlook-include-deleted*` | `t` | Lists the items a mailbox still holds that were deleted, in the folders they were deleted from; `nil` for only those not deleted. |
| `*outlook-largest-file*` | 256 MB | The most an item or saved mail, or an attachment in one, may hold to be read as a message; a larger one opens as any file. |
| `*outlook-nesting-limit*` | `8` | How many levels of messages attached to messages are shown as messages; deeper ones are listed as not read. |
| `*outlook-mailbox-seconds*` | `3600` | The most seconds readpst may take; it is stopped then, and what it wrote is listed. |
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

## License

GPL-3.0-or-later; see `LICENSE` at the top of this repository.
