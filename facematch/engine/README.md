# The facematch engine

A small program, `facematch.py`, that the facematch package drives: it finds
the faces in master photos and candidate photos and says, for each candidate,
how probable it is that it shows the same person.

| Step | What does it | Licence |
|---|---|---|
| Find the faces and their five landmarks | YuNet 2023mar (OpenCV Zoo) | MIT |
| Align each face | InsightFace's `norm_crop` | MIT (code) |
| Make it 512 numbers | AuraFace-v1's `glintr100.onnx` (fal), run by InsightFace's ArcFace runner on ONNX Runtime | Apache 2.0 |
| Similarity to probability | a logistic curve fitted on LFW (`calibration.json`) | — |

Several master photos are averaged into one template. Only AuraFace's own
model file is taken from its repository: the other files there carry the
names of InsightFace's research-only models.

## Building the AppImage

```sh
engine/build-appimage.sh
```

makes `facematch/build/facematch-x86_64.AppImage` in an Ubuntu 24.04 podman
container: a relocatable Python (python-build-standalone 3.12), numpy 1.26,
ONNX Runtime, OpenCV and InsightFace 0.7.3 (its C parts compiled there against
that numpy), the two models with their licences, `facematch.py` and
`calibration.json`. It runs where glibc is 2.39 or later. Downloads are kept
in `facematch/build/cache/`. The AppImage sets `NO_ALBUMENTATIONS_UPDATE=1`,
so that a library InsightFace loads never looks for updates on the internet.

## Calibrating

A similarity is not a probability. `calibrate.py` runs the same pipeline over
LFW's standard protocol (`pairs.txt`: ten folds of 300 same-person and 300
different-person pairs), fits a logistic curve P(same | similarity) on nine
folds and scores it on the tenth, ten times, then writes the curve fitted on
all pairs to `calibration.json` with the held-out scores and how the
probabilities hold in bands.

```sh
engine/build-appimage.sh --env          # the container's Python, with the models
# LFW's photos (lfw.tgz) and pairs.txt, unpacked somewhere, copied into the container, then:
python3 calibrate.py --lfw lfw --pairs pairs.txt --models models --out calibration.json
```

The pairs are half the same person, so the probabilities are at **even prior
odds**: they say how the photos change the odds, starting from 50%. LFW is
news photography, mostly frontal and well lit; case photos vary more.

## Running it

```sh
facematch-x86_64.AppImage --master a.jpg --master b.jpg --candidate c.jpg d.jpg [--json]
```

prints each candidate, the most probable first, with its probability (from
1% to 99%, the ends of the ODNI's scale), the ODNI's words for it and its
similarity. A master's face is the largest in its photo; a candidate's is
whichever of its faces is likeliest.

The calibration of 2026-10-11 on LFW: 98.8% of pairs right at 50% on folds
the curve never saw (±0.3%), Brier score 0.010; see calibration.json for the
bands.
