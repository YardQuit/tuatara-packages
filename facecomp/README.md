# facecomp

Faces in photos compared with the face of a master photo, by the facecomp
program.

[facecomp](https://github.com/yardquit/facecomp) finds the faces in a master
photo, or in several photos of one person averaged into one template, and in
each photo compared with it, and says how alike they are: a similarity, a
percentage and a confidence label (Almost certain, Very likely, Likely, Even
chance, Unlikely, Very unlikely, Almost no chance). This package asks for the
photos, runs facecomp and shows its report in Tuatara.

## Loading it

In `~/.config/tuatara/init.lisp`, with this repository named as a source:

```lisp
(load-package "facecomp" :source "yardquit"
  :config
  (setf *facecomp-program* "~/Applications/facecomp-x86_64.AppImage"))
```

It needs the facecomp program: its AppImage (which carries its models), or
your own build on the `PATH`. `M-x check-health` says whether it is found.

## Comparing

`M-x facecomp-compare`:

- **Anywhere:** asks for a master photo, whether to add another photo of the
  *same person* (several match more reliably), then for the photos to compare,
  one at a time, from the master's folder.
- **In a rimu list with two or more files marked:** asks which of them are
  masters (their names separated by commas, the first offered) and compares
  the rest.
- **With `C-u`:** also asks for a detection confidence for this run only, 0.8
  offered (the strict setting: it never took a non-face in facecomp's
  measurements).

The report, in `*facecomp*`, lists the masters (and how alike they are when
several, with a warning when they may not all be one person), the model and
cutoff, any photo that could not be read and why, then each photo, closest
first: its match and confidence, coloured by how likely it is, its similarity,
and how many faces it holds when more than one. `RET` on a photo opens it
safely; `q` closes the report.

## Safety

The photos may be evidence, and a picture can be made to harm the program
reading it, so facecomp runs shut in Tuatara's sandbox:

- it sees private copies of the photos under names of its own (`master-1.jpg`,
  `photo-2.png`), never the photos' folders, names or a glob pattern hidden in
  a name;
- it sees the model files you name and its own folder, read only, and nothing
  else of yours, with no network;
- an AppImage of it runs unpacked in the sandbox's own `/tmp`, as the sandbox
  has no FUSE;
- it runs in the background and is stopped after `*facecomp-seconds*`.

The copies are deleted when it is done; the photos are only read.

## Settings

| Setting | Standard | What it is |
|---|---|---|
| `*facecomp-program*` | `"facecomp"` | The program: a name on the system's own `PATH`, or a full path, an AppImage too. |
| `*facecomp-backend*` | `nil` | `:sface` (128 numbers a face) or `:arcface` (512, a little more accurate); `nil` for facecomp's own choice, SFace. |
| `*facecomp-threshold*` | `nil` | The similarity at which two faces count as one person's. Leave `nil`, so that the cutoff of the model in use is taken (0.316 SFace, 0.239 ArcFace): the other model's cutoff quietly scores everything wrong. |
| `*facecomp-detection-confidence*` | `nil` | How sure the detector must be for a face to count; `nil` for facecomp's own, 0.7. |
| `*facecomp-max*` | `nil` | The most photos listed, the closest first; `nil` for all. |
| `*facecomp-detector-model*` | `nil` | YuNet's model file, when facecomp does not know its own. |
| `*facecomp-encoder-model*` | `nil` | SFace's model file, used with SFace, when facecomp does not know its own. |
| `*facecomp-arcface-model*` | `nil` | ArcFace's model file, used with ArcFace, when facecomp does not know its own. |
| `*facecomp-seconds*` | `600` | The most seconds one comparison may take. |

The recognition model passed follows `*facecomp-backend*`, so it can never be
the other model's.

## Tests

`tools/test.sh` at the top of this repository runs them, with the others,
using a stand-in for facecomp run through the real sandbox.

## Credits

- facecomp, by the same author: the program this package drives, and the
  Emacs front end it was made from (github.com/yardquit/facecomp).
- OpenCV Zoo (YuNet, SFace) and the ONNX Model Zoo (ArcFace): the models
  facecomp uses.

## License

GPL-3.0-or-later; see `LICENSE` at the top of this repository.
