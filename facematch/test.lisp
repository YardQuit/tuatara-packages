;;;; facematch/test.lisp --- The facematch package: candidates ranked by probability, chosen by marks in rimu.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test facematch
  :description "The facematch package: master photos marked M in rimu (in any list), candidates marked as for any operation; the engine run shut in on private copies named so that no photo's name or folder reaches it; the ranking shown with the photos' own paths, each with its probability and the ODNI's words, coloured; an AppImage told apart from another program.")

(defun facematch-stand-in (dir given)
  "Write in DIR a stand-in for the engine: it fails unless it sees the first
master's copy and does not see GIVEN, the folder of the photos, then prints a
ranking naming the copies, with its arguments; its path."
  (let ((path (namestring (merge-pathnames "engine/facematch" dir))))
    (ensure-directories-exist path)
    (alexandria:write-string-into-file
     (format nil "#!/bin/sh
[ -f /tmp/photos/master-1.jpg ] || { echo 'no copy of /tmp/photos/master-1.jpg' >&2; exit 1; }
[ -e '~a' ] && { echo 'the photos folder is seen' >&2; exit 1; }
printf '{\"masters\":[\"/tmp/photos/master-1.jpg\"],\"model\":\"AuraFace-v1\",\"detector\":\"YuNet 2023mar\",\"calibration\":\"LFW, 6000 labelled pairs at even odds\",'
printf '\"results\":[{\"photo\":\"/tmp/photos/candidate-2.jpg\",\"faces\":1,\"similarity\":0.61,\"probability\":97.2,\"label\":\"Almost certain\"},'
printf '{\"photo\":\"/tmp/photos/candidate-1.png\",\"faces\":3,\"similarity\":0.05,\"probability\":3.1,\"label\":\"Almost no chance\"}],'
printf '\"errors\":[],\"arguments\":\"%s\"}\\n' \"$*\"
" (string-right-trim "/" (namestring given)))
     path)
    (sb-posix:chmod path #o755)
    path))

(define-test (facematch marked-in-rimu)
  (with-test-directory (dir)
    (let* ((given (merge-pathnames "case/" dir))
           (photos (mapcar (lambda (name)
                             (let ((path (concatenate 'string (namestring given) name)))
                               (tuatara.io:ensure-directories path)
                               (tuatara.io:replace-file path "not really a picture")
                               path))
                           '("alice.jpg" "crowd *.png" "zed.jpg")))
           (program tuatara-user:*facematch-program*))
      (unless (eq :ready (tuatara.io:sandbox-status))
        (skip "no sandbox here"))
      ;; set, not bound: the comparison runs in a thread of its own
      (setf tuatara-user:*facematch-program* (facematch-stand-in dir given))
      (unwind-protect
      ;; C-c m marks the master, m the candidates, C-c f ranks them
      (multiple-value-bind (f editor)
          (rimu-session given (list "g f a l i c e RET C-c m g f c r o w d RET m g f z e d RET m C-c f" (wait-for-buffer "*facematch*"))
                        :width 140 :height 30)
        (declare (ignore f editor))
        (let* ((ranking (tuatara.buffer:find-buffer "*facematch*"))
               (text (and ranking (tuatara.buffer:buffer-string ranking))))
          (true ranking "~a" (let ((m (tuatara.buffer:find-buffer "*Messages*"))) (and m (tuatara.buffer:buffer-string m))))
          (flet ((has (words) (true (and text (search words text)) "~s in~%~a" words text)))
            (has (format nil "Master photo~%  ~a" (tuatara.command:abbreviated-path (first photos))))
            (has "Model: AuraFace-v1, faces found by YuNet 2023mar")
            (has "Probabilities: calibrated on LFW, 6000 labelled pairs at even odds")
            (has (format nil "  1.  97.2%  Almost certain     similarity 0.6100  ~a" (tuatara.command:abbreviated-path (third photos))))
            (has (format nil "  2.   3.1%  Almost no chance   similarity 0.0500  ~a  (the likeliest of 3 faces)"
                         (tuatara.command:abbreviated-path (second photos))))
            (false (search "/tmp/photos" text)))))
        (setf tuatara-user:*facematch-program* program)))))

(define-test (facematch arguments-and-names)
  ;; the engine's arguments: each master, the run's confidence, then the candidates
  (is equal '("--master" "/tmp/photos/master-1.jpg" "--master" "/tmp/photos/master-2.png" "--detection-confidence" "0.8000"
              "--json" "--candidate" "/tmp/photos/candidate-1.jpg")
      (tuatara.defaults::facematch-arguments '("/tmp/photos/master-1.jpg" "/tmp/photos/master-2.png") '("/tmp/photos/candidate-1.jpg") 0.8))
  ;; a copy's name: its kind, its number and a plain ending, nothing else of the photo's name
  (is string= "candidate-2.jpg" (tuatara.defaults::facematch-copy-name "candidate" 2 "/x/Crowd [1]*.JPG"))
  (is string= "master-1" (tuatara.defaults::facematch-copy-name "master" 1 "/x/name.a b"))
  ;; the ODNI's words coloured by how probable
  (is eq :info (tuatara.defaults::facematch-label-face 97.2))
  (is eq :warning (tuatara.defaults::facematch-label-face 50))
  (is eq :error (tuatara.defaults::facematch-label-face 3.1))
  ;; an AppImage told apart by its header, not its name
  (with-test-directory (dir)
    (let ((appimage (namestring (merge-pathnames "engine.bin" dir)))
          (plain (namestring (merge-pathnames "engine.AppImage" dir))))
      (with-open-file (out appimage :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (coerce #(#x7f 69 76 70 2 1 1 0 65 73 2 0 0 0 0 0) '(vector (unsigned-byte 8))) out))
      (with-open-file (out plain :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (coerce #(#x7f 69 76 70 2 1 1 0 0 0 0 0 0 0 0 0) '(vector (unsigned-byte 8))) out))
      (true (tuatara.defaults::facematch-appimage-p appimage))
      (false (tuatara.defaults::facematch-appimage-p plain)))))
