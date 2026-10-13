# csv-records

Records of a CSV or TSV file kept or deleted by a regular expression on one
column, and the columns of every record rearranged as awk's `print` does.

Columns are counted from 0, as [csvdt](https://github.com/yardquit/csvdt)
counts them: `$0` is the first column.

## Loading it

In `~/.config/tuatara/init.lisp`, with this repository named as a source:

```lisp
(load-package "csv-records" :source "yardquit")
```

## Commands

In a CSV or TSV buffer, under `C-c r`:

| Key | Command | What it does |
|---|---|---|
| `C-c r k` | `csv-keep-records` | Keeps the records whose column matches a regular expression, deleting the rest. |
| `C-c r d` | `csv-delete-records` | Deletes the records whose column matches, keeping the rest. |
| `C-c r a` | `csv-rearrange-columns` | Rearranges the columns of every line as awk's `print` would. |

### Keeping and deleting records

`C-c r k` asks for the column, offering the header's names with their
numbers (`DATA-2  $4`); type a name, a number (`4` or `$4`), or `RET` for the
whole record. Then it asks for the pattern. The pattern is tried on the
field's value alone, its quotes taken off and a doubled quote read as one, so
`^` and `$` are the value's start and end. A record without that column does
not match. Smart case, as in search: a pattern with a capital letter heeds
case.

The first line, the header, always stays. With a selection, only the lines it
touches are filtered.

### Rearranging columns

`C-c r a` asks for the columns as awk's `print` writes them, counted from 0:

```
$0,$1,$3,$4"cent","hello"
```

on five columns leaves out the third (`$2`), joins `cent` to the value of the
fifth and adds a sixth column, now the fifth, holding `hello`:

```
a,b,c,d,e   ->   a,b,d,ecent,hello
```

- `$N` is column N from 0, `$NF` the last column of each record, `$*` the
  whole record.
- `"text"` is text as it is (`\"` a quote, `\\` a backslash). Parts side by
  side are joined: `$4"cent"`, `$2" "$0`.
- A column of one field alone is written exactly as it stood, its quotes
  kept. A column joined from parts is quoted when it needs to be (it holds
  the separator, a quote or a line end, or begins or ends with a space), a
  quote inside doubled.
- A column a record lacks is empty, as in awk.
- Every line is rearranged, the header too, as awk would; with a selection,
  only the lines it touches.

## Large files

Each command is one step to undo (`u`). A large file is done in the
background on every core, its progress in the mode line (`Processing 42%`),
the editor answering meanwhile. On an 11.5 GB file of 100 million records,
keeping the 8 records whose fifth column begins with `7kL6` took 5 seconds,
and rearranging every record as above took 30 seconds.

A record is a line, as Tuatara's CSV mode reads one: a quoted field does not
run on over a line end.
