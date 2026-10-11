;;;; test/run.lisp --- Command-line test runner: sbcl --non-interactive --load test/run.lisp
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loads Tuatara and its tests from the source tree $TUATARA_SOURCE names
;;;; (tools/test.sh sets it), then each package of this repository, compiled
;;;; as load-package compiles it, and its tests (NAME/test.lisp). Exits 0 when
;;;; every test passes, 1 otherwise.

(defvar *packages-root* (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*)))

(defvar *tuatara-source*
  (let ((given (sb-ext:posix-getenv "TUATARA_SOURCE")))
    (if (and given (plusp (length given)))
        (pathname (concatenate 'string (string-right-trim "/" given) "/"))
        (merge-pathnames "../tuatara/" *packages-root*))))

(load (merge-pathnames "tools/quicklisp.lisp" *tuatara-source*))
(push *tuatara-source* asdf:*central-registry*)
(ql:quickload :tuatara/tests :silent t)

(defvar *package-names*
  (loop for folder in (uiop:subdirectories *packages-root*)
        for name = (car (last (pathname-directory folder)))
        when (probe-file (merge-pathnames (concatenate 'string name ".lisp") folder))
          collect name))

(dolist (name *package-names*)
  (let ((folder (merge-pathnames (concatenate 'string name "/") *packages-root*)))
    (let ((tuatara.defaults::*package-folder* folder))
      (tuatara-user:load-package-file (concatenate 'string name ".lisp")))
    (let ((tests (merge-pathnames "test.lisp" folder)))
      (when (probe-file tests) (load tests)))))

(let ((failed nil))
  (dolist (name *package-names*)
    (let ((test (find-symbol (string-upcase name) :tuatara.tests)))
      (when test
        (unless (eql (parachute:status (parachute:test test :report 'parachute:plain)) :passed)
          (setf failed t)))))
  (tuatara.tests::remove-test-directories)
  (sb-ext:exit :code (if failed 1 0)))
