;;;; facematch/test.lisp --- The facematch package: candidates ranked by probability, chosen by marks in rimu.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test facematch
  :description "The facematch package: master photos marked M in rimu (in any list), candidates marked as for any operation; the engine run shut in on private copies named so that no photo's name or folder reaches it; the ranking shown with the photos' own paths, each with its probability and the ODNI's words, coloured.")

(defun facematch-stand-in (dir given)
  "Write in DIR a stand-in for the engine: it fails unless it sees the first
master's copy and does not see GIVEN, the folder of the photos, then prints a
ranking naming the copies, with its arguments; its path."
  (let ((path (namestring (merge-pathnames "engine/facematch" dir))))
    (ensure-directories-exist path)
    (alexandria:write-string-into-file
     (format nil "#!/bin/sh
[ -f /tmp/files/master-1.jpg ] || { echo 'no copy of /tmp/files/master-1.jpg' >&2; exit 1; }
[ -e '~a' ] && { echo 'the photos folder is seen' >&2; exit 1; }
printf '{\"masters\":[\"/tmp/files/master-1.jpg\"],\"model\":\"AuraFace-v1\",\"detector\":\"YuNet 2023mar\",\"calibration\":\"LFW, 6000 labelled pairs at even odds\",'
printf '\"results\":[{\"photo\":\"/tmp/files/candidate-2.jpg\",\"faces\":1,\"similarity\":0.61,\"probability\":97.2,\"label\":\"Almost certain\"},'
printf '{\"photo\":\"/tmp/files/candidate-1.png\",\"faces\":3,\"similarity\":0.05,\"probability\":3.1,\"label\":\"Almost no chance\"}],'
printf '\"errors\":[],\"arguments\":\"%s\"}\\n' \"$*\"
" (string-right-trim "/" (namestring given)))
     path :if-exists :supersede)
    (sb-posix:chmod path #o755)
    path))

(defun facematch-listed (name &optional (tries 1000))
  "A turn waiting, without holding up the editor, until the rimu list called
NAME is there and has its files listed, or TRIES turns have gone."
  (lambda ()
    (let ((list (tuatara.buffer:find-buffer name)))
      (unless (or (<= tries 1)
                  (and list (tuatara.buffer:buffer-local list 'tuatara.defaults::rimu-listing)
                       (null (tuatara.buffer:buffer-local list 'tuatara.defaults::rimu-search))))
        (sleep 0.01)
        (tuatara.frontend.fake:fake-queue-action (tuatara.command:editor-frontend tuatara.command:*editor*)
                                                 (facematch-listed name (1- tries)))))))

(defun waiting-until (test &optional (tries 1000))
  "A turn waiting, without holding up the editor, until TEST, a function of
no arguments, gives true, or TRIES turns have gone."
  (lambda ()
    (when (and (> tries 1) (not (funcall test)))
      (sleep 0.01)
      (tuatara.frontend.fake:fake-queue-action (tuatara.command:editor-frontend tuatara.command:*editor*)
                                               (waiting-until test (1- tries))))))

(defun facematch-done ()
  "A turn waiting, without holding up the editor, until no comparison of faces runs."
  (waiting-until (lambda () (not (find-if #'tuatara.defaults::facematch-job-p tuatara.runtime:*jobs*)))))

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
      ;; C-c f m marks the master, m the candidates, C-c f f ranks them in a rimu list of their own
      (tuatara.io:replace-file (concatenate 'string (namestring given) "notes.txt") "not in the comparison")
      (unwind-protect
           (multiple-value-bind (f editor)
               (rimu-session given (list "g f a l i c e RET C-c f m g f c r o w d RET m g f z e d RET m C-c f f" (facematch-listed "*facematch*") :tick)
                             :width 160 :height 30)
             (let* ((ranking (tuatara.buffer:find-buffer "*facematch*"))
                    (lines (and ranking (uiop:split-string (tuatara.buffer:buffer-string ranking) :separator '(#\Newline))))
                    (master (position-if (lambda (l) (search "alice.jpg" l)) lines))
                    (first (position-if (lambda (l) (search "zed.jpg" l)) lines))
                    (second (position-if (lambda (l) (search "crowd *.png" l)) lines)))
               (declare (ignore editor))
               (true ranking "~a" (let ((m (tuatara.buffer:find-buffer "*Messages*"))) (and m (tuatara.buffer:buffer-string m))))
               (true (tuatara.defaults::rimu-list-p ranking))
               (true (tuatara.io:safe-visit-p ranking))
               ;; the masters first, then a line apart, the candidates ranked, each with its ranking beside it
               (true (and master first second (< master first second)) "~{~a~%~}" lines)
               (true (search "master" (nth master lines)))
               (true (search "97.2%  Almost certain" (nth first lines)))
               (true (search "similarity 0.6100" (nth first lines)))
               (true (search "3.1%  Almost no chance" (nth second lines)))
               (true (search "likeliest of 3 faces" (nth second lines)))
               (false (find-if (lambda (l) (search "notes.txt" l)) lines))
               (true (tuatara.frontend.fake:fake-frame-showing f "2 candidates ranked against 1 master photo"))
               (true (tuatara.frontend.fake:fake-frame-showing f "probabilities calibrated on LFW"))
               (false (find-if (lambda (l) (search "/tmp/files" l)) lines))))
        (setf tuatara-user:*facematch-program* program))
      ;; C-c f r: the same comparison, the ranking shown in the case folder's own list beside the
      ;; names, the other lines taken out, no ranked list made; C-c f r again, back
      (setf tuatara-user:*facematch-program* (facematch-stand-in dir given))
      (unwind-protect
           (progn
             (multiple-value-bind (f editor)
                 (rimu-session given (list "g f a l i c e RET C-c f m g f c r o w d RET m g f z e d RET m C-c f r"
                                           (facematch-done) :tick :tick)
                               :width 160 :height 30)
               (let ((lines (rimu-lines editor)))
                 (true (find-if (lambda (l) (and (search "zed.jpg" l) (search "97.2%  Almost certain" l) (search "similarity 0.6100" l)))
                                lines)
                       "~{~a~%~}" lines)
                 (true (find-if (lambda (l) (and (search "alice.jpg" l) (search "master" l))) lines))
                 (false (find-if (lambda (l) (search "notes.txt" l)) lines))
                 ;; the candidates unmarked, the master's M kept
                 (false (find-if (lambda (l) (uiop:string-prefix-p "* " l)) lines))
                 (true (find-if (lambda (l) (and (uiop:string-prefix-p "M " l) (search "alice.jpg" l))) lines)))
               (true (tuatara.frontend.fake:fake-frame-showing f "3 photos ranked here, the other lines taken out"))
               ;; no ranked list made: that is C-c f f's
               (false (tuatara.buffer:find-buffer "*facematch*")))
             (multiple-value-bind (f editor)
                 (rimu-session given (list "g f a l i c e RET C-c f m g f z e d RET m C-c f r" (facematch-done)
                                           ;; the ranking shown before it is hidden again
                                           (waiting-until (lambda () (search "ranked here" (or (tuatara.command:editor-message tuatara.command:*editor*) ""))))
                                           "C-c f r"
                                           ;; and the list laid out again
                                           (waiting-until (lambda () (search "notes.txt" (tuatara.buffer:buffer-string (tuatara.defaults::current-buffer))))))
                               :width 160 :height 30)
               (declare (ignore f))
               (let ((lines (rimu-lines editor)))
                 (true (find-if (lambda (l) (search "notes.txt" l)) lines) "~a~%~{~a~%~}" (tuatara.command:editor-message editor) lines)
                 (false (find-if (lambda (l) (search "Almost certain" l)) lines))))
             ;; candidates are those marked in the directory shown: one kept marked elsewhere is not taken
             (tuatara.io:ensure-directories (concatenate 'string (namestring given) "other/x.jpg"))
             (tuatara.io:replace-file (concatenate 'string (namestring given) "other/x.jpg") "not really a picture")
             (let ((f (rimu-session given "g f o t h e r RET RET g f x RET m h g f a l i c e RET C-c f m g f z e d RET m C-c f f"
                                    :width 160 :height 30)))
               (true (tuatara.frontend.fake:fake-frame-showing f "Comparing 1 candidate with 1 master photo"))))
        (setf tuatara-user:*facematch-program* program)))))

(define-test (facematch arguments-and-faces)
  ;; the engine's arguments: each master, the run's confidence, then the candidates
  (is equal '("--master" "/tmp/files/master-1.jpg" "--master" "/tmp/files/master-2.png" "--detection-confidence" "0.8000"
              "--json" "--candidate" "/tmp/files/candidate-1.jpg")
      (tuatara.defaults::facematch-arguments '("/tmp/files/master-1.jpg" "/tmp/files/master-2.png") '("/tmp/files/candidate-1.jpg") 0.8))
  ;; the ODNI's words coloured by how probable
  (is eq :info (tuatara.defaults::facematch-label-face 97.2))
  (is eq :warning (tuatara.defaults::facematch-label-face 50))
  (is eq :error (tuatara.defaults::facematch-label-face 3.1)))

(define-test (facematch ranking-read)
  ;; the masters first, then the candidates ranked, one not compared after them with why, each once
  (let ((names (make-hash-table :test 'equal))
        (report (tuatara.text:json-read "{\"masters\":[\"/tmp/files/master-1.jpg\"],
\"results\":[{\"photo\":\"/tmp/files/candidate-2.jpg\",\"probability\":60.0,\"label\":\"Likely\"}],
\"errors\":[\"/tmp/files/candidate-1.png: no face found\",\"/tmp/files/candidate-1.png: still none\",\"/tmp/files/master-1.jpg: two faces\"]}")))
    (setf (gethash "/tmp/files/master-1.jpg" names) "/case/alice.jpg"
          (gethash "/tmp/files/candidate-1.png" names) "/case/crowd.png"
          (gethash "/tmp/files/candidate-2.jpg" names) "/case/zed.jpg")
    (multiple-value-bind (ranking groups) (tuatara.defaults::facematch-ranking report names)
      (is equal '(("/case/alice.jpg") ("/case/zed.jpg" "/case/crowd.png")) groups)
      (is string= "not compared: still none" (car (gethash "/case/crowd.png" ranking)))
      (is string= "not compared: two faces" (car (gethash "/case/alice.jpg" ranking)))
      (true (search "60.0%  Likely" (car (first (gethash "/case/zed.jpg" ranking))))))))
