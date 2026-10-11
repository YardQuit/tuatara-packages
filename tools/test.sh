#!/usr/bin/env bash
# tools/test.sh --- Run every package's tests.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Run every package's tests against a Tuatara source tree: $TUATARA_SOURCE,
# else ../tuatara beside this repository. In Tuatara's toolbox when there is
# one, as Tuatara's own tests run.
# Usage: tools/test.sh
set -euo pipefail
cd "$(dirname "$0")/.."
TUATARA_SOURCE="$(realpath "${TUATARA_SOURCE:-../tuatara}")"
export TUATARA_SOURCE
. "$TUATARA_SOURCE/tools/inside.sh"
inside env TUATARA_SOURCE="$TUATARA_SOURCE" sbcl --dynamic-space-size 4GB --non-interactive --load test/run.lisp
