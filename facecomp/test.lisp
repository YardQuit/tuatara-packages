;;;; facecomp/test.lisp --- The facecomp package: faces compared by facecomp, shut in, on copies of the photos.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test facecomp
  :description "The facecomp package: facecomp run shut in on private copies of the photos, named so that no photo's name or folder reaches it; its report shown with the photos' own paths, the match coloured by confidence; its settings passed as it reads them, a wrong one refused by name; why there is no report said.")

(defun facecomp-stand-in (dir given &key fail)
  "Write in DIR a stand-in for facecomp: it fails unless it sees the master's
copy and does not see GIVEN, the folder of the photos, then prints a report
naming the copies, with its arguments, or with FAIL says it found no face; its path."
  (let ((path (namestring (merge-pathnames (if fail "failing/facecomp" "bin/facecomp") dir))))
    (ensure-directories-exist path)
    (alexandria:write-string-into-file
     (format nil "#!/bin/sh
[ -f /tmp/photos/master-1.jpg ] || { echo 'no copy of /tmp/photos/master-1.jpg' >&2; exit 1; }
[ -e '~a' ] && { echo 'the photos folder is seen' >&2; exit 1; }
~:[~;echo 'master photo: no face detected in /tmp/photos/master-1.jpg' >&2; exit 1
~]echo 'a line before the report'
printf '{\"masters\":[\"/tmp/photos/master-1.jpg\",\"/tmp/photos/master-2.jpg\"],\"backend\":\"sface\",\"threshold\":0.316,\"embedding_dimensions\":128,\"enrolment_agreement\":0.21,'
printf '\"results\":[{\"photo\":\"/tmp/photos/photo-1.png\",\"faces_detected\":2,\"similarity\":0.7123,\"match_percent\":92.3,\"confidence\":\"Very likely\"},'
printf '{\"photo\":\"/tmp/photos/photo-2\",\"faces_detected\":1,\"similarity\":0.1,\"match_percent\":12.0,\"confidence\":\"Very unlikely\"}],'
printf '\"errors\":[\"/tmp/photos/photo-3.jpg: no face detected\"],\"arguments\":\"%s\"}\\n' \"$*\"
" (string-right-trim "/" (namestring given)) fail)
     path)
    (sb-posix:chmod path #o755)
    path))

(define-test (facecomp compared-shut-in)
  (with-test-directory (dir)
    (let* ((given (merge-pathnames "case/" dir))
           ;; names taken literally, as Tuatara's files take them: a * is a character
           (photos (mapcar (lambda (name)
                             (let ((path (concatenate 'string (namestring given) name)))
                               (tuatara.io:ensure-directories path)
                               (tuatara.io:replace-file path "not really a picture")
                               path))
                           '("alice.jpg" "alice two.jpg" "crowd *.png" "no-ending" "dark.jpg")))
           (tuatara-user:*facecomp-program* (facecomp-stand-in dir given))
           (tuatara-user:*facecomp-backend* :arcface)
           (tuatara-user:*facecomp-max* 5))
      (unless (eq :ready (tuatara.io:sandbox-status))
        (skip "no sandbox here"))
      (let ((job (tuatara.defaults::make-facecomp-job nil (subseq photos 0 2) (subseq photos 2) 0.8)))
        (tuatara.defaults::facecomp-work job)
        (is eq nil (tuatara.defaults::facecomp-job-failure job))
        (let ((report (tuatara.defaults::facecomp-job-report job)))
          (true report)
          ;; its arguments: the copies by their own names, the settings as it reads them
          (let ((arguments (tuatara.text:json-text report "arguments")))
            (true (search "--master /tmp/photos/master-1.jpg /tmp/photos/master-2.jpg" arguments))
            (true (search "--backend arcface" arguments))
            (true (search "--detection-confidence 0.8000" arguments))
            (true (search "--max 5" arguments))
            (true (search "--json --slave /tmp/photos/photo-1.png /tmp/photos/photo-2 /tmp/photos/photo-3.jpg" arguments))
            (false (search "alice" arguments))
            (false (search "*" arguments)))
          ;; the report: the photos' own paths, the closest first, the match coloured, the agreement warned of
          (multiple-value-bind (text spans buttons) (tuatara.defaults::facecomp-report-text report (tuatara.defaults::facecomp-job-names job))
            (flet ((has (words) (true (search words text) "~s in~%~a" words text)))
              (has "Masters: 2 photos of one person")
              (has (tuatara.command:abbreviated-path (first photos)))
              (has "Agreement: 0.2100")
              (has "Below the 0.316 cutoff")
              (has "Model: sface, 128 numbers a face, cutoff 0.316")
              (has (format nil "~a: no face detected" (tuatara.command:abbreviated-path (fifth photos))))
              (has (format nil "~a~%  92.3% match, Very likely   similarity 0.7123   the best of 2 faces in the photo"
                           (tuatara.command:abbreviated-path (third photos))))
              (has (format nil "~a~%  12.0% match, Very unlikely" (tuatara.command:abbreviated-path (fourth photos))))
              (false (search "/tmp/photos" text)))
            (true (find :info spans :key #'third))
            (true (find :error spans :key #'third))
            (is = 4 (length buttons))))
        ;; the copies gone, the photos as they were
        (is equal nil (ignore-errors (tuatara.io:directory-entries (namestring (merge-pathnames "facecomp/" (tuatara.io:cache-directory))))))
        (is string= "not really a picture" (tuatara.io:file-text (third photos))))
      ;; why there is no report, in the photos' own names
      (let ((job (tuatara.defaults::make-facecomp-job nil (list (first photos)) (list (second photos)) nil))
            (tuatara-user:*facecomp-program* (facecomp-stand-in dir given :fail t)))
        (tuatara.defaults::facecomp-work job)
        (is eq nil (tuatara.defaults::facecomp-job-report job))
        (is string= (format nil "master photo: no face detected in ~a" (tuatara.command:abbreviated-path (first photos)))
            (tuatara.defaults::facecomp-job-failure job))))))

(define-test (facecomp settings-checked)
  (with-test-directory (dir)
    (flet ((refused (setting value words)
             (progv (list setting) (list value)
               (true (search words (handler-case (progn (tuatara.defaults::facecomp-check-settings nil) "")
                                     (error (e) (princ-to-string e))))))))
      (refused 'tuatara-user:*facecomp-threshold* 1 "*facecomp-threshold* must be above 0 and below 1")
      (refused 'tuatara-user:*facecomp-max* 0 "*facecomp-max* must be a whole number above 0")
      (refused 'tuatara-user:*facecomp-backend* :other "*facecomp-backend* must be :sface, :arcface or nil")
      (refused 'tuatara-user:*facecomp-detector-model* (namestring (merge-pathnames "gone.onnx" dir)) "which is not there"))
    (true (search "must be above 0 and at most 1"
                  (handler-case (progn (tuatara.defaults::facecomp-check-settings 1.5) "") (error (e) (princ-to-string e)))))
    ;; the recognition model follows the backend, each model shown in a folder of its own
    (let ((sface (namestring (merge-pathnames "m/sface.onnx" dir)))
          (arcface (namestring (merge-pathnames "n/arcface.onnx" dir))))
      (dolist (f (list sface arcface)) (alexandria:write-string-into-file "x" (ensure-directories-exist f)))
      (let ((tuatara-user:*facecomp-encoder-model* sface) (tuatara-user:*facecomp-arcface-model* arcface))
        (let ((tuatara-user:*facecomp-backend* :arcface))
          (is equal '("--encoder-model" "/tmp/models/1/arcface.onnx") (tuatara.defaults::facecomp-models)))
        (let ((tuatara-user:*facecomp-backend* nil))
          (is equal '("--encoder-model" "/tmp/models/1/sface.onnx") (tuatara.defaults::facecomp-models)))))
    ;; a copy's name: its kind, its number and a plain ending, nothing else of the photo's name
    (is string= "photo-3.jpg" (tuatara.defaults::facecomp-copy-name "photo" 3 "/x/My Photo*.JPG"))
    (is string= "photo-1" (tuatara.defaults::facecomp-copy-name "photo" 1 "/x/name.a b"))))
