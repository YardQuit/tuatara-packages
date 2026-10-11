#!/usr/bin/env python3
# facematch/engine/calibrate.py --- Fit how a similarity becomes a probability, on LFW's labelled pairs.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

"""Fit how a similarity becomes a probability, on LFW's labelled pairs.

Runs facematch's own pipeline (YuNet, norm_crop, AuraFace) over LFW's
standard protocol, pairs.txt: ten folds of 300 pairs of one person and 300
of two. For each fold a logistic curve P(same | similarity) is fitted on the
other nine and scored on it (log loss, Brier score, accuracy at 50%), so the
report says how well the probabilities hold on photos the curve never saw;
then the curve fitted on all 6000 is written as calibration.json. The pairs
are half the same person, so the probabilities are at even odds.

Usage: calibrate.py --lfw DIR --pairs pairs.txt --models DIR --out calibration.json
"""

import argparse
import json
import math
import sys
from datetime import date

import numpy as np

import facematch


def read_pairs(path):
    """LFW's pairs.txt as a list of folds, each a list of (photo, photo, same)."""
    with open(path, encoding="utf-8") as f:
        folds, size = (int(n) for n in f.readline().split())
        rows = [line.split() for line in f if line.strip()]
    pairs = []
    for row in rows:
        if len(row) == 3:
            pairs.append(((row[0], int(row[1])), (row[0], int(row[2])), 1))
        else:
            pairs.append(((row[0], int(row[1])), (row[2], int(row[3])), 0))
    return [pairs[i * 2 * size:(i + 1) * 2 * size] for i in range(folds)]


def photo_path(lfw, person, number):
    """The path of PERSON's photo NUMBER under the LFW folder LFW."""
    return f"{lfw}/{person}/{person}_{number:04d}.jpg"


def fit_logistic(x, y, steps=200):
    """The slope and intercept of the logistic curve best fitting labels Y
    (0 or 1) on X, by Newton's method on the log likelihood."""
    w = np.zeros(2)
    a = np.column_stack([x, np.ones_like(x)])
    for _ in range(steps):
        p = 1 / (1 + np.exp(-a @ w))
        gradient = a.T @ (y - p)
        hessian = (a * (p * (1 - p))[:, None]).T @ a + 1e-9 * np.eye(2)
        step = np.linalg.solve(hessian, gradient)
        w += step
        if np.abs(step).max() < 1e-10:
            break
    return float(w[0]), float(w[1])


def scores(slope, intercept, x, y):
    """Log loss, Brier score and accuracy at 50% of the curve on X, Y."""
    p = np.clip(1 / (1 + np.exp(-(slope * x + intercept))), 1e-12, 1 - 1e-12)
    return (float(-np.mean(y * np.log(p) + (1 - y) * np.log(1 - p))),
            float(np.mean((p - y) ** 2)),
            float(np.mean((p >= 0.5) == (y == 1))))


def main():
    """Fit the curve on the LFW pairs named on the command line, report how it
    held on each fold, and write it as calibration.json."""
    parser = argparse.ArgumentParser(prog="calibrate.py", description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--lfw", required=True, help="the folder of LFW's photos, one folder a person")
    parser.add_argument("--pairs", required=True, help="LFW's pairs.txt")
    parser.add_argument("--models", required=True, help="the folder of the engine's models")
    parser.add_argument("--out", required=True, help="where the fitted curve is written, calibration.json")
    args = parser.parse_args()
    faces = facematch.Faces(args.models, 0.7)
    folds = read_pairs(args.pairs)
    cache = {}

    def embedding(photo):
        """The main face's embedding in PHOTO, as the engine takes a master's,
        or None when none is found."""
        if photo not in cache:
            found = faces.embeddings(photo_path(args.lfw, *photo))
            cache[photo] = facematch.main_face(found) if found else None
        return cache[photo]

    data, skipped = [], 0
    for k, fold in enumerate(folds):
        rows = []
        for a, b, same in fold:
            ea, eb = embedding(a), embedding(b)
            if ea is None or eb is None:
                skipped += 1
                continue
            rows.append((float(np.dot(ea, eb)), same))
        data.append(np.array(rows))
        print(f"fold {k + 1}: {len(rows)} pairs", file=sys.stderr, flush=True)
    held_out = []
    for k in range(len(data)):
        train = np.concatenate([d for i, d in enumerate(data) if i != k])
        slope, intercept = fit_logistic(train[:, 0], train[:, 1])
        held_out.append(scores(slope, intercept, data[k][:, 0], data[k][:, 1]))
    allrows = np.concatenate(data)
    slope, intercept = fit_logistic(allrows[:, 0], allrows[:, 1])
    loss, brier, accuracy = (np.mean([h[i] for h in held_out]) for i in range(3))
    spread = np.std([h[2] for h in held_out])
    # how the probabilities hold: the fraction of same-person pairs in bands of predicted probability
    p = 1 / (1 + np.exp(-(slope * allrows[:, 0] + intercept)))
    bands = []
    for low, high in [(0, .05), (.05, .2), (.2, .45), (.45, .55), (.55, .8), (.8, .95), (.95, 1.01)]:
        inside = (p >= low) & (p < high)
        if inside.any():
            bands.append({"predicted": [low, min(high, 1)], "pairs": int(inside.sum()),
                          "same_person": round(float(allrows[inside, 1].mean()), 4)})
    result = {"slope": slope, "intercept": intercept,
              "source": f"LFW, {len(allrows)} labelled pairs at even odds, {date.today().isoformat()}",
              "pairs": int(len(allrows)), "skipped": skipped,
              "held_out": {"log_loss": round(float(loss), 4), "brier": round(float(brier), 4),
                           "accuracy": round(float(accuracy), 4), "accuracy_spread": round(float(spread), 4)},
              "similarity_at_even_chance": round(-intercept / slope, 4),
              "bands": bands}
    with open(args.out, "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)
    print(json.dumps(result, indent=2))
    return 0


if __name__ == "__main__":
    sys.exit(main())
