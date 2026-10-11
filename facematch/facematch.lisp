;;;; facematch/facematch.lisp --- How probable it is that candidate photos show the person of master photos: facematch.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package, not part of the editor: loaded by (load-package "facematch")
;;;; in the init file. It drives the facematch engine (engine/ here, built as
;;;; one AppImage by engine/build-appimage.sh): InsightFace with AuraFace-v1
;;;; and YuNet, its similarities made probabilities by a curve fitted on LFW.
;;;;
;;;; Photos are chosen in rimu, its preview showing each: \[facematch-mark-master]
;;;; marks a master photo of the person looked for (M, in any list: masters
;;;; often stand in a folder of their own), m marks candidates as for any
;;;; operation, and \[facematch-compare] compares every master photo marked
;;;; with the candidates marked in the list, in the background. *facematch*
;;;; ranks the candidates, the most probable first: each with the probability
;;;; that it shows the same person, the ODNI's words for it (Almost certain
;;;; ... Almost no chance), and the similarity behind it.
;;;;
;;;; The photos may be evidence, so the engine runs shut in the sandbox on
;;;; private copies of them under names of its own, with no network and
;;;; nothing of yours but its own files, read only. An AppImage of it is
;;;; unpacked once, outside the sandbox (it is your own program), into the
;;;; editor's cache, and run from there read only: no run can change it for
;;;; the next.

(in-package #:tuatara.defaults)

;;; Settings

(defvar tuatara-user::*facematch-program* "facematch"
  "The facematch engine: a name looked for in the system's own folders of
programs, or the full path of one anywhere, its AppImage too
\(\"~/Applications/facematch-x86_64.AppImage\").")

(defvar tuatara-user::*facematch-detection-confidence* nil
  "How sure the face detector must be, above 0 and at most 1, for a face to
count; NIL for the engine's own, 0.7. \\[universal-argument] \\[facematch-compare] asks
for one run's.")

(defvar tuatara-user::*facematch-seconds* 900
  "The most seconds one comparison may take before the engine is stopped.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(tuatara-user::*facematch-program* tuatara-user::*facematch-detection-confidence* tuatara-user::*facematch-seconds*)
          :tuatara-user))

;;; The engine

(defun facematch-program ()
  "The full path of the engine (*FACEMATCH-PROGRAM*), or NIL when it is not
there: a name looked for where the sandbox finds programs, a path taken as it is."
  (let ((name tuatara-user:*facematch-program*))
    (if (find #\/ name)
        (let ((path (tuatara.io:native (tuatara.io:expand-path name))))
          (and (eq :regular (tuatara.io:path-kind path)) path))
        (tuatara.io:sandbox-find-program name))))

(defun facematch-appimage-p (path)
  "True when the file at PATH is an AppImage: an ELF program marked AI 2."
  (let ((head (ignore-errors (tuatara.io:file-head path 12))))
    (and head (>= (length head) 11)
         (tuatara.io:octets-at-p head '(#x7f 69 76 70))
         (= (aref head 8) 65) (= (aref head 9) 73) (= (aref head 10) 2))))

(defun facematch-unpacked (program cancel)
  "The folder where the AppImage PROGRAM is unpacked, unpacking it the first
time (run with --appimage-extract, outside the sandbox: your own program),
once for each version of it, the older ones taken away. CANCEL, a cons, a
job's, is heeded once it is done."
  (let* ((stamp (tuatara.io:path-stamp program))
         (root (format nil "~afacematch/" (tuatara.io:native (tuatara.io:cache-directory))))
         (folder (format nil "~aengine-~d-~d/" root (tuatara.io:stamp-size stamp) (tuatara.io:stamp-mtime stamp)))
         (unpacked (concatenate 'string folder "squashfs-root/")))
    (unless (eq :regular (tuatara.io:path-kind (concatenate 'string unpacked "AppRun")))
      (dolist (old (ignore-errors (tuatara.io:directory-entries root)))
        (when (uiop:string-prefix-p "engine-" (first old))
          (ignore-errors (tuatara.io:delete-tree (concatenate 'string root (first old))))))
      (tuatara.io:ensure-private-directory folder)
      (multiple-value-bind (output ended-well)
          (tuatara.io:program-output (list program "--appimage-extract") :capture nil :seconds 600 :directory folder)
        (declare (ignore output))
        (when (car cancel) (ignore-errors (tuatara.io:delete-tree folder)) (tuatara.runtime:job-check-stop))
        (unless (and ended-well (eq :regular (tuatara.io:path-kind (concatenate 'string unpacked "AppRun"))))
          (ignore-errors (tuatara.io:delete-tree folder))
          (error "~a could not be unpacked" (abbreviated-path program)))))
    unpacked))

;;; A comparison, in the background

(defstruct (facematch-job (:include tuatara.runtime:job) (:constructor make-facematch-job (buffer masters candidates confidence)) (:copier nil))
  "The photos CANDIDATES compared with the master photos MASTERS by the
engine, with CONFIDENCE for this run or NIL, a job for BUFFER: REPORT, what
it said, read JSON; NAMES, each copy's name as it saw it with the photo's
path; FAILURE, why there is no report."
  masters candidates confidence report names failure)

(setf (documentation 'facematch-job-p 'function) "True when OBJECT is a FACEMATCH-JOB.")

(tuatara.runtime:define-job-words facematch-job
  :doing "Comparing faces"
  :description (format nil "Comparing ~d candidate~:p with ~d master photo~:p"
                       (length (facematch-job-candidates job)) (length (facematch-job-masters job)))
  :verb "compare the faces"
  :left-words "nothing compared")

(defun facematch-copy-name (kind n path)
  "The name the copy of the photo at PATH goes by: KIND (master or candidate)
and N, with PATH's ending when it is a short one of letters and digits."
  (let ((ending (tuatara.io:native-file-type path)))
    (format nil "~a-~d~@[.~(~a~)~]" kind n
            (and ending (<= 1 (length ending) 5) (every #'alphanumericp ending) ending))))

(defun facematch-arguments (masters candidates confidence)
  "The engine's arguments comparing CANDIDATES with MASTERS, the names of
their copies as the sandbox shows them, with CONFIDENCE for the run."
  (append (loop for m in masters append (list "--master" m))
          (and confidence (list "--detection-confidence" (format nil "~,4f" (float confidence 1d0))))
          (list "--json" "--candidate")
          candidates))

(defun facematch-original-names (text names)
  "TEXT with each copy's name in it (NAMES, each (COPY . PATH)) written as the
photo's own path, ~ for your home folder."
  (loop for (copy . path) in names
        do (setf text (cl-ppcre:regex-replace-all (cl-ppcre:quote-meta-chars copy) text
                                                  (lambda (match) (declare (ignore match)) (abbreviated-path path))
                                                  :simple-calls t)))
  text)

(defun facematch-work (job)
  "What a comparison does: copies the photos into a folder of its own, runs
the engine on them shut in (an AppImage from its unpacked folder), and keeps
its report, or why there is none."
  (let* ((program (or (facematch-program)
                      (error "The facematch engine is not there: build or install it, or give its path in *facematch-program*")))
         (cancel (tuatara.runtime:job-cancel job))
         (folder (format nil "~afacematch/run-~36r/" (tuatara.io:native (tuatara.io:cache-directory)) (random (expt 36 8))))
         (names '()))
    (multiple-value-bind (run shown)
        (if (facematch-appimage-p program)
            (let ((unpacked (facematch-unpacked program cancel)))
              (values (concatenate 'string unpacked "AppRun") (list (string-right-trim "/" unpacked))))
            (values program (and (find #\/ tuatara-user:*facematch-program*)
                                 (list (string-right-trim "/" (tuatara.io:native (tuatara.io:parent-path program)))))))
      (tuatara.runtime:job-check-stop)
      (tuatara.io:ensure-private-directory folder)
      (unwind-protect
           (flet ((copies (kind paths)
                    (loop for path in paths
                          for n from 1
                          for name = (facematch-copy-name kind n path)
                          do (unless (tuatara.io:copy-regular-file path (concatenate 'string folder name) :cancel cancel)
                               (tuatara.runtime:job-check-stop)
                               (error "~a could not be copied for the engine" (abbreviated-path path)))
                             (push (cons (format nil "/tmp/photos/~a" name) path) names)
                          collect (format nil "/tmp/photos/~a" name))))
             (let ((masters (copies "master" (facematch-job-masters job)))
                   (candidates (copies "candidate" (facematch-job-candidates job))))
               (setf (facematch-job-names job) names)
               (multiple-value-bind (output outcome said)
                   (tuatara.io:sandbox-run run (facematch-arguments masters candidates (facematch-job-confidence job))
                                           :directories (cons (cons (string-right-trim "/" folder) "/tmp/photos") shown)
                                           :seconds tuatara-user:*facematch-seconds* :memory (* 6 1024 1024 1024)
                                           :cancel cancel)
                 (tuatara.runtime:job-check-stop)
                 (let* ((text (and output (sb-ext:octets-to-string output :external-format '(:utf-8 :replacement #\?))))
                        (start (and text (position #\{ text)))
                        (report (and start (ignore-errors (tuatara.text:json-read text :start start)))))
                   (if (and report (eq outcome :done))
                       (setf (facematch-job-report job) report)
                       (setf (facematch-job-failure job)
                             (case outcome
                               (:unavailable (format nil "The engine runs only shut in the sandbox, and there is none here: ~a" said))
                               (:timeout (format nil "The engine was stopped after ~d seconds (*facematch-seconds*)" tuatara-user:*facematch-seconds*))
                               (t (facematch-original-names
                                   (string-trim '(#\Space #\Newline) (if (plusp (length said)) said "The engine ended without a report"))
                                   names)))))))))
        (ignore-errors (tuatara.io:delete-tree folder))))))

;;; The report

(define-major-mode facematch-mode special-mode
  "The ranking of a comparison of faces (\\[facematch-compare]): the master
photos and how alike they are, then each candidate, the most probable first,
with the probability that it shows the same person, the ODNI's words for it
and the similarity. \\<facematch-mode>\\[push-button] on a photo opens it safely; \\[quit-window]
closes the ranking."
  :lighter "facematch" :interactive nil)

(defun facematch-label-face (percent)
  "The face a candidate PERCENT probable is shown in: Very likely and above,
Likely to Even chance, and below."
  (cond ((>= percent 80) :info)
        ((>= percent 45) :warning)
        (t :error)))

(defun facematch-report-text (report names)
  "The text of the ranking REPORT, the engine's read JSON, its photos' names
\(NAMES, each (COPY . PATH)) written as theirs, as three values: the text,
its spans, each (START END FACE), and its buttons, each (START END ACTION),
in octets."
  (let ((out (make-string-output-stream)) (at 0) (spans '()) (buttons '()))
    (labels ((put (text &optional face)
               (let ((octets (length (tuatara.text:string-octets text))))
                 (when face (push (list at (+ at octets) face) spans))
                 (write-string text out)
                 (incf at octets)))
             (items (key) (coerce (tuatara.text:json-items report key) 'list))
             (photo (copy)
               ;; the photo's own path, a button opening it safely
               (let ((file (or (cdr (assoc copy names :test #'string=)) copy)) (start at))
                 (put (abbreviated-path file))
                 (push (list start at (lambda () (visit-path file :safe t))) buttons))))
      (let ((masters (items "masters"))
            (agreement (tuatara.text:json-get report "agreement")))
        (put (format nil "~:[Master photo~;~:*~d master photos, averaged into one template~]~%"
                     (and (rest masters) (length masters))))
        (dolist (m masters)
          (put "  ") (photo m) (put (string #\Newline)))
        (when (realp agreement)
          (let ((percent (tuatara.text:json-get report "agreement_probability")))
            (put (format nil "Agreement: the least alike two masters are ~,4f alike~@[, ~,1f% probably one person~]~%"
                         agreement (and (realp percent) percent)))
            (when (and (realp percent) (< percent 50))
              (put (format nil "  These may not all be the same person: a stray one pulls the template away~%") :warning)))))
      (put (format nil "Model: ~@[~a~]~@[, faces found by ~a~]~%Probabilities: calibrated on ~a~%"
                   (tuatara.text:json-text report "model") (tuatara.text:json-text report "detector")
                   (or (tuatara.text:json-text report "calibration") "nothing said")))
      (let ((errors (items "errors")))
        (when errors
          (put (string #\Newline))
          (dolist (e errors) (put (format nil "~a~%" (one-line (facematch-original-names e names))) :warning))))
      (put (string #\Newline))
      (let ((results (items "results")))
        (if (null results)
            (put (format nil "No candidate could be compared~:[~;: see why above~].~%" (items "errors")))
            (loop for r in results
                  for rank from 1
                  do (let ((percent (tuatara.text:json-get r "probability"))
                           (similarity (tuatara.text:json-get r "similarity"))
                           (faces (tuatara.text:json-integer r "faces")))
                       (put (format nil "~3d. " rank))
                       (when (realp percent)
                         (put (format nil "~5,1f%  ~17a" percent (or (tuatara.text:json-text r "label") ""))
                              (facematch-label-face percent)))
                       (put (format nil "~@[  similarity ~,4f~]  " (and (realp similarity) similarity)))
                       (photo (tuatara.text:json-text r "photo"))
                       (put (format nil "~@[  (the likeliest of ~d faces)~]~%" (and faces (> faces 1) faces))))))))
    (values (get-output-stream-string out) (nreverse spans) (nreverse buttons))))

(defmethod tuatara.runtime:job-take-in ((job facematch-job))
  "Show the ranking in *facematch*, or say why there is none."
  (cond ((facematch-job-report job)
         (let ((buffer (buffer-in-mode "*facematch*" 'facematch-mode)))
           (multiple-value-bind (text spans buttons) (facematch-report-text (facematch-job-report job) (facematch-job-names job))
             (set-made-text buffer text :spans spans :buttons buttons))
           (setf (tuatara.buffer:buffer-point buffer) 0)
           (display-buffer buffer)
           (message "Faces compared: ~d candidate~:p ranked" (length (tuatara.text:json-items (facematch-job-report job) "results")))))
        ((facematch-job-failure job) (message "~a" (facematch-job-failure job)))
        (t (message "~a" (tuatara.runtime:job-ending-words job)))))

;;; Choosing the photos in rimu

(defparameter *facematch-master-mark* #\M
  "The mark of a master photo in a rimu list.")

(define-command facematch-mark-master ()
  "Mark the photo on the line of this rimu list as a master photo (M), one of
the person looked for, and go to the next line; \\<rimu-mode>\\[rimu-unmark] takes the mark away.
Masters may be marked in any list: \\[facematch-compare] takes them all."
  (if (rimu-list-p (current-buffer))
      (rimu-mark-and-go-on *facematch-master-mark*)
      (message "Master photos are marked in a rimu list (\\[rimu])")))

(defun facematch-masters ()
  "The master photos marked (M) in every rimu list, in the order shown."
  (remove-duplicates (loop for buffer in tuatara.buffer:*buffers*
                           when (rimu-list-p buffer)
                             append (loop for entry in (rimu-chosen buffer *facematch-master-mark*)
                                          when (eq (rimu-leads-to entry) :regular)
                                            collect (tuatara.io:native (rimu-path (tuatara.buffer:buffer-local buffer 'rimu-directory) entry))))
                     :test #'string= :from-end t))

(defun facematch-read-confidence ()
  "Ask for this run's detection confidence, 0.8 offered."
  (let ((value (parse-decimal (string-trim " " (read-string "Detection confidence for this run: " :initial "0.8")))))
    (unless (and value (< 0 value) (<= value 1))
      (error "The detection confidence must be above 0 and at most 1"))
    value))

(define-command facematch-compare ()
  "Rank the candidate photos marked (m) in this rimu list by how probable it
is that each shows the person of the master photos marked (\\<rimu-mode>\\[facematch-mark-master], M,
in any list): in *facematch*, the most probable first, each with its
probability, the ODNI's words for it (Almost certain, Very likely, Likely,
Even chance, Unlikely, Very unlikely, Almost no chance) and the similarity.
With \\[universal-argument], asks for a detection confidence for this run (0.8, the
strict one). The engine runs shut in the sandbox on copies of the photos, in
the background; the photos are only read."
  (let* ((buffer (current-buffer))
         (confidence-asked (editor-prefix-argument *editor*))
         (masters (facematch-masters))
         (candidates (and (rimu-list-p buffer)
                          (remove-if (lambda (path) (member path masters :test #'string=))
                                     (rimu-chosen-paths buffer :files-only t)))))
    (cond ((not (rimu-list-p buffer)) (message "Candidates are marked in a rimu list (\\[rimu]), and compared from it"))
          ((null masters) (message "No master photo is marked: \\<rimu-mode>\\[facematch-mark-master] marks one (M)"))
          ((null candidates) (message "No candidate is marked here: \\<rimu-mode>\\[rimu-mark] marks one"))
          ((null (facematch-program))
           (message "The facematch engine is not there: build or install it, or give its path in *facematch-program*"))
          (t (let ((confidence (if confidence-asked (facematch-read-confidence) tuatara-user:*facematch-detection-confidence*)))
               (when (and confidence (not (and (realp confidence) (< 0 confidence) (<= confidence 1))))
                 (error "*facematch-detection-confidence* must be above 0 and at most 1, not ~s" confidence))
               (tuatara.runtime:start-job-thread (make-facematch-job buffer masters candidates confidence) #'facematch-work
                                                 :name "tuatara facematch")
               (message "Comparing ~d candidate~:p with ~d master photo~:p..." (length candidates) (length masters)))))))

;;; Loading

(defun facematch-health-words ()
  "What compares faces by probability here, as \\[check-health] says it."
  (let ((program (facematch-program)))
    (format nil "Faces ranked by probability by ~:[none: build the facematch engine, or give its path in *facematch-program*~;~:*~a, shut in~]"
            (and program (abbreviated-path program)))))

(defun install-facematch ()
  "Bind facematch's keys in rimu, and have \\[check-health] say what it runs."
  (define-keys *rimu-keymap* '(("C-c m" facematch-mark-master) ("C-c f" facematch-compare)))
  (commands-for-mode 'rimu-mode "a rimu list, with the facematch package" 'facematch-mark-master 'facematch-compare)
  (pushnew 'facematch-health-words *health-decoder-functions*))

(install-facematch)
