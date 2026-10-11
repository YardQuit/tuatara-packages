#!/usr/bin/env bash
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Make facematch/build/facematch-x86_64.AppImage: the facematch engine with
# all it needs in one file, nothing installed: a relocatable Python
# (python-build-standalone), InsightFace with its dependencies, ONNX Runtime
# and OpenCV, the models (YuNet from OpenCV Zoo, MIT; AuraFace-v1's
# glintr100.onnx from fal, Apache 2.0) with their licences, and
# calibration.json. Built in an Ubuntu 24.04 podman container, so that it
# runs where glibc is 2.39 or later. What is downloaded is kept in
# facematch/build/cache/ for the next build.
#
# Usage: engine/build-appimage.sh            the AppImage (calibration.json must exist)
#        engine/build-appimage.sh --env      only the container's Python with its
#                                            packages and the models, for calibrate.py
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
build="$here/../build"
cache="$build/cache"
mkdir -p "$cache/models"
container=facematch-build
image=docker.io/library/ubuntu:24.04
python_url="https://github.com/astral-sh/python-build-standalone/releases/download/20241016/cpython-3.12.7+20241016-x86_64-unknown-linux-gnu-install_only.tar.gz"
appimagetool="$here/../../../tuatara/build/appimage-tools/appimagetool-x86_64.AppImage"
[ -x "$appimagetool" ] || appimagetool="$cache/appimagetool-x86_64.AppImage"

fetch() { [ -s "$2" ] || curl -sSfL -o "$2.part" "$1" && { [ -s "$2" ] || mv "$2.part" "$2"; }; }
fetch "$python_url" "$cache/python.tar.gz"
fetch https://github.com/opencv/opencv_zoo/raw/main/models/face_detection_yunet/face_detection_yunet_2023mar.onnx \
      "$cache/models/face_detection_yunet_2023mar.onnx"
fetch https://raw.githubusercontent.com/opencv/opencv_zoo/main/models/face_detection_yunet/LICENSE "$cache/models/LICENSE-YuNet"
fetch https://huggingface.co/fal/AuraFace-v1/resolve/main/glintr100.onnx "$cache/models/glintr100.onnx"
fetch https://huggingface.co/fal/AuraFace-v1/resolve/main/LICENSE.md "$cache/models/LICENSE-AuraFace.md"
[ -x "$appimagetool" ] || { fetch https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-x86_64.AppImage "$appimagetool"; chmod +x "$appimagetool"; }

if ! podman container exists "$container"; then
  podman run -d --name "$container" -v "$cache:/cache:z" "$image" sleep infinity >/dev/null
  podman exec "$container" sh -c 'DEBIAN_FRONTEND=noninteractive apt-get update -qq &&
    DEBIAN_FRONTEND=noninteractive apt-get install -y -qq build-essential >/dev/null'
fi
podman start "$container" >/dev/null
# the Python and its packages, made once: InsightFace builds its C parts here
podman exec "$container" bash -c '
  set -e
  if [ ! -x /opt/AppDir/usr/python/bin/python3 ]; then
    mkdir -p /opt/AppDir/usr
    tar -xzf /cache/python.tar.gz -C /opt/AppDir/usr
  fi
  py=/opt/AppDir/usr/python/bin/python3
  # numpy held at 1.26 for every package, so that what is compiled matches what runs
  echo numpy==1.26.4 > /tmp/constraints.txt
  export PIP_CONSTRAINT=/tmp/constraints.txt NO_ALBUMENTATIONS_UPDATE=1
  $py -m pip install -q --no-cache-dir numpy==1.26.4 onnxruntime==1.19.2 opencv-python-headless==4.10.0.84 cython setuptools wheel
  # InsightFace builds its C parts with the system compiler, against that numpy
  if ! $py -c "import insightface" 2>/dev/null; then
    CC=gcc CXX=g++ $py -m pip install -q --no-cache-dir --no-build-isolation insightface==0.7.3
    CC=gcc CXX=g++ $py -m pip install -q --no-cache-dir --no-build-isolation --force-reinstall --no-deps insightface==0.7.3
  fi
  $py -c "import insightface, onnxruntime, cv2, numpy"'
[ "${1:-}" = --env ] && { echo "the engine Python is ready in the container $container, at /opt/AppDir/usr/python"; exit 0; }

[ -s "$here/calibration.json" ] || { echo "no engine/calibration.json: run calibrate.py first (see engine/README)" >&2; exit 1; }
# the engine copied in afresh each build, never read through a mount
podman exec "$container" rm -rf /engine-now
podman cp "$here" "$container:/engine-now"
podman exec "$container" bash -c '
  set -e
  app=/opt/AppDir
  mkdir -p $app/usr/share/facematch/models $app/usr/share/facematch/licenses
  cp /engine-now/facematch.py $app/usr/share/facematch/
  cp /engine-now/calibration.json /cache/models/face_detection_yunet_2023mar.onnx /cache/models/glintr100.onnx $app/usr/share/facematch/models/
  cp /cache/models/LICENSE-YuNet /cache/models/LICENSE-AuraFace.md $app/usr/share/facematch/licenses/
  cat > $app/AppRun <<"EOF"
#!/bin/sh
here="$(dirname "$(readlink -f "$0")")"
export APPDIR="$here" NO_ALBUMENTATIONS_UPDATE=1 OMP_NUM_THREADS=4 MALLOC_ARENA_MAX=2
exec "$here/usr/python/bin/python3" -I -s -W ignore::FutureWarning "$here/usr/share/facematch/facematch.py" "$@"
EOF
  chmod +x $app/AppRun
  printf "[Desktop Entry]\nType=Application\nName=facematch\nExec=facematch\nIcon=facematch\nTerminal=true\nCategories=Utility;\n" > $app/facematch.desktop
  rm -rf /tmp/AppDir && cp -a $app /tmp/AppDir'
rm -rf "$build/AppDir"
podman cp "$container:/tmp/AppDir" "$build/AppDir"
magick -size 256x256 xc:'#2f4f4f' -fill white -gravity center -pointsize 96 -annotate 0 'fm' "$build/AppDir/facematch.png" 2>/dev/null \
  || convert -size 256x256 xc:'#2f4f4f' "$build/AppDir/facematch.png"
ARCH=x86_64 "$appimagetool" --appimage-extract-and-run --no-appstream "$build/AppDir" "$build/facematch-x86_64.AppImage" >/dev/null
echo "$build/facematch-x86_64.AppImage"
