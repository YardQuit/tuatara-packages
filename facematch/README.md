# facematch

Candidate photos ranked by how probable it is that they show the person of one
or more master photos, each with its probability and the ODNI's words for it.

The photos are chosen in rimu, with its preview showing each: master photos
marked `M`, candidates marked `*`. The comparison runs in the background, shut
in Tuatara's sandbox, by the facematch engine: InsightFace with the AuraFace-v1
model, built into one AppImage (`engine/`).

## Getting the engine

```sh
facematch/engine/build-appimage.sh
```

builds `facematch/build/facematch-x86_64.AppImage` (podman needed; it
downloads Python, InsightFace and its packages, and the two models). See
`engine/README.md`.

## Loading it

In `~/.config/tuatara/init.lisp`, with this repository named as a source:

```lisp
(load-package "facematch" :source "yardquit"
  :config
  (setf *facematch-program* "~/Projects/public/yardquit/tuatara-packages/facematch/build/facematch-x86_64.AppImage"))
```

`M-x check-health` says whether the engine is found.

## Comparing

1. Open the folder with the master photos in rimu (`C-x d`). The preview shows
   the photo under the cursor.
2. **`C-c m`** marks a photo as master: `M` in the first column. Mark one, or
   several of the same person; they're averaged into one template. `u` takes a
   mark away.
3. Open the folder of the candidates (it may be another one). **`m`** marks
   each candidate (`*`), as for any rimu operation.
4. **`C-c f`** compares every master marked, in any rimu list, with the
   candidates marked in this one. `C-u C-c f` asks for a detection confidence
   for this run (0.8, the strict one).

The ranking, in `*facematch*`:

```
2 master photos, averaged into one template
  ~/case/masters/alice-1.jpg
  ~/case/masters/alice-2.jpg
Agreement: the least alike two masters are 0.6123 alike, 99.0% probably one person
Model: AuraFace-v1, faces found by YuNet 2023mar
Probabilities: calibrated on LFW, 6000 labelled pairs at even odds, 2026-10-11

  1.  99.0%  Almost certain     similarity 0.5312  ~/case/photos/p17.jpg
  2.  62.4%  Likely             similarity 0.2474  ~/case/photos/p03.jpg  (the likeliest of 3 faces)
  3.   1.2%  Almost no chance   similarity 0.0081  ~/case/photos/p11.jpg
```

Each candidate's words follow the ODNI's scale of estimative probability
(ICD 203):

| Words | Probability |
|---|---|
| Almost certain | 95–99% |
| Very likely | 80–95% |
| Likely | 55–80% |
| Even chance | 45–55% |
| Unlikely | 20–45% |
| Very unlikely | 5–20% |
| Almost no chance | 1–5% |

`RET` on a photo opens it safely; `q` closes the ranking.

## What the probability means

A similarity between two faces is not a probability. The engine turns it into
one with a curve fitted on LFW (Labeled Faces in the Wild), 6000 pairs of
photos known to be of one person or of two, and checked on folds of pairs the
curve never saw (see `engine/calibrate.py` and `engine/calibration.json`).

How it held on LFW, scored on folds the curve never saw:

| Measure | Result |
|---|---|
| Pairs right at 50% | 98.8% (±0.3% between folds) |
| Brier score | 0.010 |
| Pairs said 0–5% likely: one person | 0.9% |
| Said 5–20%: one person | 8.7% |
| Said 20–45%: one person | 21.6% |
| Said 80–95%: one person | 94.7% |
| Said 95% and over: one person | 100% |

AuraFace's makers report 99.65% on LFW with their own face detector; this
engine finds faces with YuNet, whose licence is settled, and measures 98.8%.
Probabilities are shown from 1% to 99%, the ends of the ODNI's scale: no
calibration on 6000 pairs supports more certainty than that. In a photo of
several people, a master's face is the largest; a candidate's is whichever of
its faces is likeliest.

- **Even prior odds.** LFW's pairs are half the same person, so the
  probability says how far the faces move the odds from 50%. If you already
  have reason to think a match is rare (one suspect among thousands of
  photos), the real probability is lower than shown.
- **News photos.** LFW is mostly frontal, well-lit news photography. Profile
  views, poor light, low resolution and age gaps lower accuracy, and the
  calibration says nothing about how much.
- **Several masters.** The curve was fitted on one photo against one; a
  template of several masters raises genuine scores by design, so the
  probabilities for a template are somewhat generous.
- **Ethnicity.** AuraFace's makers say its training data may not cover all
  ethnicities well.

Treat the ranking as a way to find which photos to examine, not as an
identification.

## Safety

The photos may be evidence, and a picture can be made to harm the program
reading it, so the engine runs shut in Tuatara's sandbox:

- it sees private copies of the photos under names of its own
  (`master-1.jpg`, `candidate-2.png`), never the photos' folders or names;
- its AppImage is unpacked once, outside the sandbox (it's your own program),
  into Tuatara's cache, and run from there read only, so no run can change it
  for the next;
- it has no network, and nothing of yours but its own files;
- it runs in the background and is stopped after `*facematch-seconds*`.

The copies are deleted when it is done; the photos are only read.

## Settings

| Setting | Standard | What it is |
|---|---|---|
| `*facematch-program*` | `"facematch"` | The engine: a name on the system's own `PATH`, or a full path, its AppImage too. |
| `*facematch-detection-confidence*` | `nil` | How sure the detector must be for a face to count; `nil` for the engine's own, 0.7. |
| `*facematch-seconds*` | `900` | The most seconds one comparison may take. |

## Tests

`tools/test.sh` at the top of this repository runs them, with the others: the
whole marking and comparing in rimu, by keys, with a stand-in engine run
through the real sandbox.

## Credits

- InsightFace (MIT): face alignment and the ArcFace runner.
- AuraFace-v1, by fal (Apache 2.0): the face recognition model.
- YuNet, from OpenCV Zoo (MIT): the face detector.
- Labeled Faces in the Wild, University of Massachusetts, Amherst: the
  calibration's photos.
- The Office of the Director of National Intelligence's ICD 203: the words of
  estimative probability.

## License

GPL-3.0-or-later; see `LICENSE` at the top of this repository. The models
keep their own licences, carried in the AppImage.
