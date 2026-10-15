;;;; facecomp/facecomp.lisp --- Faces compared across photos by the facecomp program: M-x facecomp-compare.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package, not part of the editor: loaded by (load-package "facecomp") in
;;;; the init file. Made from the Emacs front end of facecomp
;;;; (github.com/yardquit/facecomp), by the same author.
;;;;
;;;; facecomp, a program of its own, finds the faces in a master photo, or in
;;;; several photos of one person averaged into one template, and in each
;;;; photo compared with it, and says how alike they are: a similarity, a
;;;; percentage and a confidence label (Almost certain, Very likely, ...).
;;;; Photos are chosen in rimu: \[facecomp-mark-master] marks a master photo
;;;; (M, in any list), m marks the photos to compare as for any operation, and
;;;; \[facecomp-compare] runs facecomp on them in the background and shows its
;;;; report in *facecomp*.
;;;;
;;;; The photos may be evidence and a picture may be made to harm the program
;;;; reading it, so facecomp runs shut in the sandbox: it sees private copies
;;;; of the photos under names of their own (photo names and folders never
;;;; reach it, nor a glob pattern in a name), the models it is given and its
;;;; own folder, all read only, and no network. An AppImage of it is
;;;; unpacked once, outside the sandbox (it is your own program), into the
;;;; editor's cache, and run from there read only: no run can change it for
;;;; the next. The copies go when it is done; the photos are only read.

(defpackage #:facecomp
  (:use #:cl #:tuatara)
  (:documentation "Faces compared across photos by the facecomp program: M-x facecomp-compare."))

(in-package #:facecomp)

;;; Settings

(define-package-setting *facecomp-program* "facecomp"
  "The facecomp program: a name looked for in the system's own folders of
programs, or the full path of one anywhere, an AppImage too
\(\"~/Applications/facecomp-x86_64.AppImage\").")

(define-package-setting *facecomp-detector-model* nil
  "The face detector's model file, OpenCV Zoo's face_detection_yunet_2023mar.onnx;
NIL for the one facecomp knows itself, as an AppImage of it does.")

(define-package-setting *facecomp-encoder-model* nil
  "The SFace recognition model, OpenCV Zoo's face_recognition_sface_2021dec.onnx,
used while *FACECOMP-BACKEND* is SFace; NIL for the one facecomp knows itself.")

(define-package-setting *facecomp-arcface-model* nil
  "The ArcFace recognition model, ONNX Model Zoo's arcfaceresnet100-11-int8.onnx,
used while *FACECOMP-BACKEND* is :ARCFACE; NIL for the one facecomp knows itself.")

(define-package-setting *facecomp-backend* nil
  "The model faces are made numbers with: :SFACE (128 numbers a face) or
:ARCFACE (512, a little more accurate); NIL for facecomp's own choice, SFace.
Leave *FACECOMP-THRESHOLD* NIL with it, so that the cutoff of the model in use
is taken: the two models' cutoffs differ, 0.316 and 0.239.")

(define-package-setting *facecomp-threshold* nil
  "The similarity, above 0 and below 1, at or above which two faces count as
one person's; NIL, as recommended, for the cutoff of the model in use
\(*FACECOMP-BACKEND*). A cutoff of the other model is not refused: it quietly
scores every comparison wrong.")

(define-package-setting *facecomp-max* nil
  "The most photos the report lists, the closest first; NIL for all of them.")

(define-package-setting *facecomp-detection-confidence* nil
  "How sure the detector must be, above 0 and at most 1, for a face to count:
lower finds faces in hard photos, higher keeps out what is no face (0.8 never
took a non-face in facecomp's measurements); NIL for facecomp's own, 0.7.
\\[universal-argument] \\[facecomp-compare] asks for one run's.")

(define-package-setting *facecomp-seconds* 600
  "The most seconds one comparison may take before facecomp is stopped.")

;;; The program and its arguments

(defun facecomp-number (value)
  "VALUE, a setting's number, written as facecomp reads one."
  (format nil "~,4f" (float value 1d0)))

(defun facecomp-check-settings (confidence)
  "Signal an error naming the setting when one is set to what facecomp would
refuse: *FACECOMP-THRESHOLD*, CONFIDENCE (the run's detection confidence),
*FACECOMP-MAX*, *FACECOMP-BACKEND*, or a model that is not there."
  (let ((threshold *facecomp-threshold*) (most *facecomp-max*))
    (when (and threshold (not (and (realp threshold) (< 0 threshold 1))))
      (error "*facecomp-threshold* must be above 0 and below 1, not ~s" threshold))
    (when (and confidence (not (and (realp confidence) (< 0 confidence) (<= confidence 1))))
      (error "The detection confidence must be above 0 and at most 1, not ~s" confidence))
    (when (and most (not (and (integerp most) (plusp most))))
      (error "*facecomp-max* must be a whole number above 0, not ~s" most))
    (unless (member *facecomp-backend* '(nil :sface :arcface))
      (error "*facecomp-backend* must be :sface, :arcface or nil, not ~s" *facecomp-backend*))
    (dolist (setting '(*facecomp-detector-model* *facecomp-encoder-model* *facecomp-arcface-model*))
      (let ((file (symbol-value setting)))
        (when (and file (not (eq :regular (path-kind (native (expand-path file))))))
          (error "~(~a~) names ~a, which is not there" (symbol-name setting) file))))))

(defun facecomp-models ()
  "The model files facecomp is given, as two values: the arguments naming
them as the sandbox shows them, and the files to show, each (FILE . WHERE).
The recognition model is the one *FACECOMP-BACKEND* uses."
  (let ((arguments '()) (files '()) (n 0))
    (flet ((model (flag file)
             (when file
               (let* ((path (native (expand-path file)))
                      (where (format nil "/tmp/models/~d/~a" (incf n) (native-file-name path))))
                 (push (cons path where) files)
                 (setf arguments (append arguments (list flag where)))))))
      (model "--detector-model" *facecomp-detector-model*)
      ;; never the other model's: its numbers would be compared against the wrong cutoff
      (model "--encoder-model" (if (eq *facecomp-backend* :arcface)
                                   *facecomp-arcface-model*
                                   *facecomp-encoder-model*)))
    (values arguments files)))

(defun facecomp-arguments (masters photos confidence models)
  "The arguments comparing MASTERS with PHOTOS, the names of their copies as
the sandbox shows them, with MODELS (FACECOMP-MODELS' arguments), CONFIDENCE
for the run and the settings."
  (append (list "--master") masters
          models
          (and *facecomp-backend* (list "--backend" (string-downcase (symbol-name *facecomp-backend*))))
          (and *facecomp-threshold* (list "--threshold" (facecomp-number *facecomp-threshold*)))
          (and confidence (list "--detection-confidence" (facecomp-number confidence)))
          (and *facecomp-max* (list "--max" (princ-to-string *facecomp-max*)))
          (list "--json" "--slave")
          photos))

;;; A comparison, in the background

(defstruct (facecomp-job (:include job) (:constructor make-facecomp-job (buffer masters photos confidence)) (:copier nil))
  "MASTERS (photos of one person) being compared with PHOTOS by facecomp,
with CONFIDENCE for this run or NIL, a job for BUFFER: REPORT, what facecomp
said, read JSON; NAMES, the table of its copies' names (RUN-ON-COPIES);
SHOWN, the report's pieces (FACECOMP-REPORT-PIECES); FAILURE, why there is
no report."
  masters photos confidence report names shown failure)

(setf (documentation 'facecomp-job-p 'function) "True when OBJECT is a FACECOMP-JOB.")

(define-job-words facecomp-job
  :doing "Comparing faces"
  :description (format nil "Comparing ~d photo~:p with ~a" (length (facecomp-job-photos job))
                         (native-file-name (first (facecomp-job-masters job))))
  :verb "compare the faces"
  :left-words "nothing compared")

(defun facecomp-work (job)
  "What a comparison does: runs facecomp shut in on copies of the photos
\(RUN-ON-COPIES-FOR-REPORT), and keeps its report, or why there is none."
  (multiple-value-bind (models model-files) (facecomp-models)
    (multiple-value-bind (report names failure)
        (run-on-copies-for-report *facecomp-program*
                                  (list (cons "master" (facecomp-job-masters job)) (cons "photo" (facecomp-job-photos job)))
                                  (lambda (masters photos) (facecomp-arguments masters photos (facecomp-job-confidence job) models))
                                  :label "facecomp" :seconds-setting '*facecomp-seconds*
                                  :missing "facecomp is not there: install it, or give its path in *facecomp-program*"
                                  :directories model-files :memory (* 4 1024 1024 1024) :cancel (job-cancel job))
      (setf (facecomp-job-names job) names
            (facecomp-job-failure job) failure)
      (when report
        (setf (facecomp-job-report job) report
              (facecomp-job-shown job) (facecomp-report-pieces report names))))))

;;; The report

(defun facecomp-show-photo (path)
  "Show the photo at PATH beside the report, as its picture, drawn in the
sandbox (IMAGE-VIEW-VISIT); what is no picture it can draw, opened safely."
  (let ((show (lambda (buffer) (pop-to-buffer buffer :other t))))
    (or (image-view-visit path show)
        (visit-path path :safe t :show show))))

(defun facecomp-report-pieces (report names)
  "The pieces (as SHOW-HELP-PIECES takes them) of the report REPORT, facecomp's
read JSON, its photos' names (NAMES, as RUN-ON-COPIES gives them) written as
theirs, each photo a link showing it."
  (let ((pieces '()))
    (labels ((put (text &optional face) (push (if face (cons text face) text) pieces))
             (items (key) (coerce (json-items report key) 'list))
             (photo (copy)
               (let ((file (gethash copy names copy)))
                 (push (list (abbreviated-path file) :link (lambda () (facecomp-show-photo file))) pieces))))
      (let ((masters (items "masters"))
            (threshold (json-get report "threshold"))
            (agreement (json-get report "enrolment_agreement")))
        (if (rest masters)
            (put (format nil "Masters: ~d photos of one person, averaged into one template~%" (length masters)))
            (put "Master: "))
        (dolist (m masters)
          (when (rest masters) (put "  "))
          (photo m)
          (put (string #\Newline)))
        (when (realp agreement)
          (put (format nil "Agreement: ~,4f, the least alike two masters are~%" agreement))
          (when (and (realp threshold) (< agreement threshold))
            (put (format nil "  Below the ~,3f cutoff: these may not all be the same person~%" threshold) :warning)))
        (put (format nil "Model: ~@[~a~]~@[, ~d numbers a face~]~@[, cutoff ~,3f~]~%"
                     (json-text report "backend") (json-integer report "embedding_dimensions")
                     (and (realp threshold) threshold))))
      (let ((errors (items "errors")))
        (when errors
          (put (string #\Newline))
          (dolist (e errors)
            (put (format nil "~a~%" (one-line (copies-original-names e names))) :warning))))
      (put (string #\Newline))
      (let ((results (items "results")))
        (if (null results)
            (put (format nil "No photo could be compared~:[~;: see why above~].~%" (items "errors")))
            (dolist (r results)
              (let ((percent (json-get r "match_percent"))
                    (similarity (json-get r "similarity"))
                    (faces (json-integer r "faces_detected")))
                (photo (json-text r "photo"))
                (put (format nil "~%  "))
                (when (realp percent)
                  (put (format nil "~,1f% match, ~a" percent (or (json-text r "confidence") "")) (likelihood-face percent)))
                (put (format nil "~@[   similarity ~,4f~]~@[   the best of ~d faces in the photo~]~2%"
                             (and (realp similarity) similarity) (and faces (> faces 1) faces))))))))
    (nreverse pieces)))

(defmethod job-take-in ((job facecomp-job))
  "Show facecomp's report in *facecomp*, or say why there is none."
  (cond ((facecomp-job-report job)
         (show-help-pieces (facecomp-job-shown job) :name "*facecomp*")
         (message "Faces compared: ~d photo~:p" (length (json-items (facecomp-job-report job) "results"))))
        ((facecomp-job-failure job) (message "~a" (facecomp-job-failure job)))
        (t (message "~a" (job-ending-words job)))))

;;; Choosing the photos in rimu

(defparameter *facecomp-master-mark* #\M
  "The mark of a master photo in a rimu list.")

(define-command facecomp-mark-master ()
  "Mark the photo on the line of this rimu list as a master photo (M), one of
the person the photos are compared with, and go to the next line;
\\<rimu-mode>\\[list-unmark-entry] takes the mark away. Masters may be marked in
any list: \\[facecomp-compare] takes them all."
  (if (rimu-list-p (current-buffer))
      (rimu-mark-and-go-on *facecomp-master-mark*)
      (message "Master photos are marked in a rimu list (\\[rimu])")))

(defun facecomp-masters ()
  "The master photos marked (M) in every rimu list and every directory it has
shown, in the order shown."
  (remove-duplicates (loop for buffer in (buffer-list)
                           when (rimu-list-p buffer)
                             append (rimu-chosen-paths buffer :files-only t :mark *facecomp-master-mark*))
                     :test #'string= :from-end t))

(define-command facecomp-compare ()
  "Compare the faces in the photos marked (m) in the directory this rimu list
shows with the face of the master photos marked (\\<rimu-mode>\\[facecomp-mark-master], M, in
any list), several photos of one person averaged into one template, by
facecomp, and show how alike each is in *facecomp*: a match, a confidence
label (Almost certain, Very likely, ...) and the similarity. With
\\[universal-argument], asks for a detection confidence for this run (0.8 is
the strict one). facecomp runs shut in the sandbox on copies of the photos, in
the background; the photos are only read."
  (let* ((buffer (current-buffer))
         (confidence-asked (prefix-argument))
         (masters (facecomp-masters))
         ;; the photos marked in the directory shown, not those kept marked elsewhere
         (photos (remove-if (lambda (path) (member path masters :test #'string=))
                            (rimu-chosen-paths buffer :files-only t :here-only t))))
    (cond ((not (rimu-list-p buffer)) (message "Photos are marked in a rimu list (\\[rimu]), and compared from it"))
          ((null masters) (message "No master photo is marked: \\<rimu-mode>\\[facecomp-mark-master] marks one (M)"))
          ((null photos) (message "No photo to compare is marked in this directory: \\<rimu-mode>\\[list-mark-entry] marks one"))
          ((null (user-program-path *facecomp-program*))
           (message "facecomp is not there: install it, or give its path in *facecomp-program*"))
          (t (let ((confidence (if confidence-asked
                                   (read-decimal-number "Detection confidence for this run (~a): " 0.8
                                                        :valid (lambda (n) (and (plusp n) (<= n 1)))
                                                        :refusal "Not a number above 0 and at most 1.")
                                   *facecomp-detection-confidence*)))
               (facecomp-check-settings confidence)
               (start-job-thread (make-facecomp-job buffer masters photos confidence) #'facecomp-work
                                 :name "tuatara facecomp")
               (message "Comparing ~d photo~:p with ~d master photo~:p..." (length photos) (length masters)))))))

;;; Loading

(defun facecomp-health-words ()
  "What compares faces here, as \\[check-health] says it."
  (let ((program (user-program-path *facecomp-program*)))
    (program-health-words "Faces in photos compared" (and program (abbreviated-path program))
                          "install facecomp, or give its path in *facecomp-program*")))

(defun install-facecomp ()
  "Bind facecomp's keys in rimu, and have \\[check-health] say what compares
faces here."
  ;; under C-c f, as facematch's: the mark of a master photo is the same M for both
  (define-keys *rimu-keymap* '(("C-c f m" facecomp-mark-master) ("C-c f c" facecomp-compare)))
  (label-keys *rimu-keymap* '(("C-c f" "faces")))
  (commands-for-mode 'rimu-mode "a rimu list, with the facecomp package" 'facecomp-mark-master 'facecomp-compare)
  (pushnew 'facecomp-health-words *health-decoder-functions*))

(install-facecomp)
