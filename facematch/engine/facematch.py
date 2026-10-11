#!/usr/bin/env python3
# facematch/engine/facematch.py --- Faces in candidate photos compared with master photos' face.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
#
# Finds the faces in one or more master photos of one person and in each
# candidate photo, and says for each candidate how probable it is that it
# shows the same person: the probability, its words on the ODNI's scale
# (Almost certain ... Almost no chance) and the cosine similarity behind it,
# the candidates ranked most probable first.
#
# Faces are found with YuNet (OpenCV Zoo, MIT), aligned on their five
# landmarks with InsightFace's norm_crop, and made 512 numbers by AuraFace-v1
# (fal, Apache 2.0) through InsightFace's ArcFace runner. Several masters are
# averaged into one template. A similarity becomes a probability by the
# logistic curve in calibration.json, fitted on LFW's 6000 labelled pairs at
# even odds (calibrate.py).
#
# Usage: facematch --master PHOTO [--master PHOTO...] --candidate PHOTO... [--json]
#                  [--detection-confidence 0.7] [--models DIR]

import argparse
import json
import math
import os
import sys

import cv2
import numpy as np
from insightface.model_zoo.arcface_onnx import ArcFaceONNX
from insightface.utils.face_align import norm_crop

DETECTOR = "face_detection_yunet_2023mar.onnx"
RECOGNISER = "glintr100.onnx"

# The ODNI's words of estimative probability (ICD 203), each with the least
# probability, in percent, it is said for.
LABELS = [(95, "Almost certain"), (80, "Very likely"), (55, "Likely"), (45, "Even chance"),
          (20, "Unlikely"), (5, "Very unlikely"), (0, "Almost no chance")]


def label(percent):
    """The ODNI's words for a probability of PERCENT."""
    for least, words in LABELS:
        if percent >= least:
            return words
    return LABELS[-1][1]


def models_folder(given):
    """The folder of the models: GIVEN, else beside this program's AppImage
    ($APPDIR/usr/share/facematch/models), else models/ beside this file."""
    if given:
        return given
    appdir = os.environ.get("APPDIR")
    if appdir:
        return os.path.join(appdir, "usr/share/facematch/models")
    return os.path.join(os.path.dirname(os.path.abspath(__file__)), "models")


class Faces:
    """The detector and the recogniser, loaded once."""

    def __init__(self, folder, confidence):
        self.detector = cv2.FaceDetectorYN.create(os.path.join(folder, DETECTOR), "", (320, 320),
                                                  score_threshold=confidence, nms_threshold=0.3, top_k=50)
        self.recogniser = ArcFaceONNX(model_file=os.path.join(folder, RECOGNISER))
        self.recogniser.prepare(ctx_id=-1)

    def embeddings(self, path):
        """Each face in the photo at PATH as (embedding, area), the embedding of
        unit length, the area of the face's box in pixels; an error when it
        cannot be read."""
        image = cv2.imread(path, cv2.IMREAD_COLOR)
        if image is None:
            raise ValueError("not a picture OpenCV reads")
        height, width = image.shape[:2]
        self.detector.setInputSize((width, height))
        _, found = self.detector.detect(image)
        faces = []
        for row in (found if found is not None else []):
            landmarks = row[4:14].reshape(5, 2).astype(np.float32)
            aligned = norm_crop(image, landmark=landmarks, image_size=112)
            vector = self.recogniser.get_feat(aligned).flatten()
            faces.append((vector / np.linalg.norm(vector), float(row[2] * row[3])))
        return faces


def main_face(found):
    """The embedding of the main face of a photo's FOUND faces: the largest,
    as the person a photo is of stands nearest and in the middle, and the
    detector may be surer of a sharp face behind."""
    return max(found, key=lambda f: f[1])[0]


def load_calibration(folder):
    """The logistic curve turning a similarity into a probability, from
    calibration.json in FOLDER: a dict with slope and intercept."""
    with open(os.path.join(folder, "calibration.json"), encoding="utf-8") as f:
        return json.load(f)


def probability(similarity, calibration):
    """The probability, 0.01 to 0.99, that two faces SIMILARITY alike are one
    person's: held within the ODNI's scale, as no calibration on a few
    thousand pairs supports a certainty beyond it."""
    z = calibration["slope"] * similarity + calibration["intercept"]
    return min(0.99, max(0.01, 1.0 / (1.0 + math.exp(-z))))


def compare(masters, candidates, faces, calibration):
    """The report comparing the photos CANDIDATES with the template of the
    photos MASTERS (paths), as a dict ready for JSON."""
    errors = []
    template = []
    for path in masters:
        try:
            found = faces.embeddings(path)
        except Exception as e:  # a photo OpenCV cannot read
            errors.append(f"{path}: {e}")
            continue
        if not found:
            errors.append(f"{path}: no face found in this master photo")
            continue
        if len(found) > 1:
            errors.append(f"{path}: {len(found)} faces in this master photo; the largest taken")
        template.append(main_face(found))
    if not template:
        return {"masters": masters, "results": [], "errors": errors + ["no master photo with a face: nothing compared"]}
    agreement = None
    if len(template) > 1:
        agreement = min(float(np.dot(a, b)) for i, a in enumerate(template) for b in template[i + 1:])
    mean = np.mean(template, axis=0)
    mean = mean / np.linalg.norm(mean)
    results = []
    for path in candidates:
        try:
            found = faces.embeddings(path)
        except Exception as e:
            errors.append(f"{path}: {e}")
            continue
        if not found:
            errors.append(f"{path}: no face found")
            continue
        similarity = max(float(np.dot(mean, f[0])) for f in found)
        percent = 100.0 * probability(similarity, calibration)
        results.append({"photo": path, "faces": len(found), "similarity": round(similarity, 4),
                        "probability": round(percent, 1), "label": label(percent)})
    results.sort(key=lambda r: r["probability"], reverse=True)
    report = {"masters": masters, "model": "AuraFace-v1", "detector": "YuNet 2023mar",
              "calibration": calibration.get("source", ""), "results": results, "errors": errors}
    if agreement is not None:
        report["agreement"] = round(agreement, 4)
        report["agreement_probability"] = round(100.0 * probability(agreement, calibration), 1)
    return report


def text_report(report):
    """REPORT as lines of text, for a terminal."""
    lines = [f"master: {m}" for m in report["masters"]]
    if "agreement" in report:
        lines.append(f"agreement: {report['agreement']:.4f} ({report['agreement_probability']:.1f}% one person)")
    for e in report["errors"]:
        lines.append(f"warning: {e}")
    for r in report["results"]:
        lines.append(f"{r['probability']:5.1f}%  {r['label']:<17} similarity {r['similarity']:.4f}  {r['photo']}")
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(prog="facematch", description=__doc__)
    parser.add_argument("--master", action="append", required=True, metavar="PHOTO",
                        help="a photo of the person looked for; several are averaged into one template")
    parser.add_argument("--candidate", nargs="+", required=True, metavar="PHOTO", help="the photos compared")
    parser.add_argument("--detection-confidence", type=float, default=0.7,
                        help="how sure the detector must be for a face to count, above 0 and at most 1")
    parser.add_argument("--models", help="the folder of the models and calibration.json")
    parser.add_argument("--json", action="store_true", help="print the report as JSON")
    args = parser.parse_args()
    if not 0 < args.detection_confidence <= 1:
        parser.error("--detection-confidence must be above 0 and at most 1")
    folder = models_folder(args.models)
    try:
        faces = Faces(folder, args.detection_confidence)
        calibration = load_calibration(folder)
    except Exception as e:
        print(f"facematch: the models could not be loaded from {folder}: {e}", file=sys.stderr)
        return 2
    report = compare(args.master, args.candidate, faces, calibration)
    print(json.dumps(report) if args.json else text_report(report))
    return 0


if __name__ == "__main__":
    sys.exit(main())
