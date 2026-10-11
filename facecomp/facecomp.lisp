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
;;;; M-x facecomp-compare asks for the photos, or takes the files marked in a
;;;; rimu list, runs facecomp in the background and shows its report in
;;;; *facecomp*.
;;;;
;;;; The photos may be evidence and a picture may be made to harm the program
;;;; reading it, so facecomp runs shut in the sandbox: it sees private copies
;;;; of the photos under names of their own (photo names and folders never
;;;; reach it, nor a glob pattern in a name), the models it is given and its
;;;; own folder, all read only, and no network. An AppImage of it is
;;;; unpacked once, outside the sandbox (it is your own program), into the
;;;; editor's cache, and run from there read only: no run can change it for
;;;; the next. The copies go when it is done; the photos are only read.

(in-package #:tuatara.defaults)

;;; Settings

(defvar tuatara-user::*facecomp-program* "facecomp"
  "The facecomp program: a name looked for in the system's own folders of
programs, or the full path of one anywhere, an AppImage too
\(\"~/Applications/facecomp-x86_64.AppImage\").")

(defvar tuatara-user::*facecomp-detector-model* nil
  "The face detector's model file, OpenCV Zoo's face_detection_yunet_2023mar.onnx;
NIL for the one facecomp knows itself, as an AppImage of it does.")

(defvar tuatara-user::*facecomp-encoder-model* nil
  "The SFace recognition model, OpenCV Zoo's face_recognition_sface_2021dec.onnx,
used while *FACECOMP-BACKEND* is SFace; NIL for the one facecomp knows itself.")

(defvar tuatara-user::*facecomp-arcface-model* nil
  "The ArcFace recognition model, ONNX Model Zoo's arcfaceresnet100-11-int8.onnx,
used while *FACECOMP-BACKEND* is :ARCFACE; NIL for the one facecomp knows itself.")

(defvar tuatara-user::*facecomp-backend* nil
  "The model faces are made numbers with: :SFACE (128 numbers a face) or
:ARCFACE (512, a little more accurate); NIL for facecomp's own choice, SFace.
Leave *FACECOMP-THRESHOLD* NIL with it, so that the cutoff of the model in use
is taken: the two models' cutoffs differ, 0.316 and 0.239.")

(defvar tuatara-user::*facecomp-threshold* nil
  "The similarity, above 0 and below 1, at or above which two faces count as
one person's; NIL, as recommended, for the cutoff of the model in use
\(*FACECOMP-BACKEND*). A cutoff of the other model is not refused: it quietly
scores every comparison wrong.")

(defvar tuatara-user::*facecomp-max* nil
  "The most photos the report lists, the closest first; NIL for all of them.")

(defvar tuatara-user::*facecomp-detection-confidence* nil
  "How sure the detector must be, above 0 and at most 1, for a face to count:
lower finds faces in hard photos, higher keeps out what is no face (0.8 never
took a non-face in facecomp's measurements); NIL for facecomp's own, 0.7.
\\[universal-argument] \\[facecomp-compare] asks for one run's.")

(defvar tuatara-user::*facecomp-seconds* 600
  "The most seconds one comparison may take before facecomp is stopped.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(tuatara-user::*facecomp-program* tuatara-user::*facecomp-detector-model* tuatara-user::*facecomp-encoder-model*
            tuatara-user::*facecomp-arcface-model* tuatara-user::*facecomp-backend* tuatara-user::*facecomp-threshold*
            tuatara-user::*facecomp-max* tuatara-user::*facecomp-detection-confidence* tuatara-user::*facecomp-seconds*)
          :tuatara-user))

;;; The program and its arguments

(defun facecomp-number (value)
  "VALUE, a setting's number, written as facecomp reads one."
  (format nil "~,4f" (float value 1d0)))

(defun facecomp-check-settings (confidence)
  "Signal an error naming the setting when one is set to what facecomp would
refuse: *FACECOMP-THRESHOLD*, CONFIDENCE (the run's detection confidence),
*FACECOMP-MAX*, *FACECOMP-BACKEND*, or a model that is not there."
  (let ((threshold tuatara-user:*facecomp-threshold*) (most tuatara-user:*facecomp-max*))
    (when (and threshold (not (and (realp threshold) (< 0 threshold 1))))
      (error "*facecomp-threshold* must be above 0 and below 1, not ~s" threshold))
    (when (and confidence (not (and (realp confidence) (< 0 confidence) (<= confidence 1))))
      (error "The detection confidence must be above 0 and at most 1, not ~s" confidence))
    (when (and most (not (and (integerp most) (plusp most))))
      (error "*facecomp-max* must be a whole number above 0, not ~s" most))
    (unless (member tuatara-user:*facecomp-backend* '(nil :sface :arcface))
      (error "*facecomp-backend* must be :sface, :arcface or nil, not ~s" tuatara-user:*facecomp-backend*))
    (dolist (setting '(tuatara-user::*facecomp-detector-model* tuatara-user::*facecomp-encoder-model* tuatara-user::*facecomp-arcface-model*))
      (let ((file (symbol-value setting)))
        (when (and file (not (eq :regular (tuatara.io:path-kind (tuatara.io:native (tuatara.io:expand-path file))))))
          (error "~(~a~) names ~a, which is not there" (symbol-name setting) file))))))

(defun facecomp-models ()
  "The model files facecomp is given, as two values: the arguments naming
them as the sandbox shows them, and the files to show, each (FILE . WHERE).
The recognition model is the one *FACECOMP-BACKEND* uses."
  (let ((arguments '()) (files '()) (n 0))
    (flet ((model (flag file)
             (when file
               (let* ((path (tuatara.io:native (tuatara.io:expand-path file)))
                      (where (format nil "/tmp/models/~d/~a" (incf n) (tuatara.io:native-file-name path))))
                 (push (cons path where) files)
                 (setf arguments (append arguments (list flag where)))))))
      (model "--detector-model" tuatara-user:*facecomp-detector-model*)
      ;; never the other model's: its numbers would be compared against the wrong cutoff
      (model "--encoder-model" (if (eq tuatara-user:*facecomp-backend* :arcface)
                                   tuatara-user:*facecomp-arcface-model*
                                   tuatara-user:*facecomp-encoder-model*)))
    (values arguments files)))

(defun facecomp-arguments (masters photos confidence models)
  "The arguments comparing MASTERS with PHOTOS, the names of their copies as
the sandbox shows them, with MODELS (FACECOMP-MODELS' arguments), CONFIDENCE
for the run and the settings."
  (append (list "--master") masters
          models
          (and tuatara-user:*facecomp-backend* (list "--backend" (string-downcase (symbol-name tuatara-user:*facecomp-backend*))))
          (and tuatara-user:*facecomp-threshold* (list "--threshold" (facecomp-number tuatara-user:*facecomp-threshold*)))
          (and confidence (list "--detection-confidence" (facecomp-number confidence)))
          (and tuatara-user:*facecomp-max* (list "--max" (princ-to-string tuatara-user:*facecomp-max*)))
          (list "--json" "--slave")
          photos))

;;; A comparison, in the background

(defstruct (facecomp-job (:include tuatara.runtime:job) (:constructor make-facecomp-job (buffer masters photos confidence)) (:copier nil))
  "MASTERS (photos of one person) being compared with PHOTOS by facecomp,
with CONFIDENCE for this run or NIL, a job for BUFFER: REPORT, what facecomp
said, read JSON; NAMES, the table of its copies' names (RUN-ON-COPIES);
SHOWN, the report's text, spans and buttons (FACECOMP-REPORT-TEXT); FAILURE,
why there is no report."
  masters photos confidence report names shown failure)

(setf (documentation 'facecomp-job-p 'function) "True when OBJECT is a FACECOMP-JOB.")

(tuatara.runtime:define-job-words facecomp-job
  :doing "Comparing faces"
  :description (format nil "Comparing ~d photo~:p with ~a" (length (facecomp-job-photos job))
                       (tuatara.io:native-file-name (first (facecomp-job-masters job))))
  :verb "compare the faces"
  :left-words "nothing compared")

(defun facecomp-work (job)
  "What a comparison does: runs facecomp shut in on copies of the photos
\(RUN-ON-COPIES), and keeps its report, or why there is none."
  (let ((program (or (user-program-path tuatara-user:*facecomp-program*)
                     (error "facecomp is not there: install it, or give its path in *facecomp-program*"))))
    (multiple-value-bind (models model-files) (facecomp-models)
      (multiple-value-bind (output outcome said names)
          (run-on-copies program (list (cons "master" (facecomp-job-masters job)) (cons "photo" (facecomp-job-photos job)))
                         (lambda (masters photos) (facecomp-arguments masters photos (facecomp-job-confidence job) models))
                         :directories model-files
                         :seconds tuatara-user:*facecomp-seconds* :memory (* 4 1024 1024 1024)
                         :cancel (tuatara.runtime:job-cancel job))
        (tuatara.runtime:job-check-stop)
        (setf (facecomp-job-names job) names)
        (let ((report (and (eq outcome :done) (copies-output-json output))))
          (if report
              (setf (facecomp-job-report job) report
                    (facecomp-job-shown job) (multiple-value-list (facecomp-report-text report names)))
              (setf (facecomp-job-failure job)
                    (run-on-copies-failure "facecomp" outcome said names 'tuatara-user:*facecomp-seconds*))))))))

;;; The report

(defun facecomp-percent-face (percent)
  "The face a match of PERCENT is shown in: Very likely and above, Likely to
Even chance, and below."
  (cond ((>= percent 80) :info)
        ((>= percent 45) :warning)
        (t :error)))

(define-major-mode facecomp-mode special-mode
  "The report of a comparison of faces (\\[facecomp-compare]): the master
photos, how alike they are when several, and each photo compared, closest
first, with its match, confidence and similarity. \\<facecomp-mode>\\[push-button] on a
photo, or a click on it, shows the picture beside it; \\[quit-window] closes the report."
  :lighter "facecomp" :interactive nil)

(defun facecomp-show-photo (path)
  "Show the photo at PATH beside the report, as its picture, drawn in the
sandbox (IMAGE-VIEW-VISIT); what is no picture it can draw, opened safely."
  (let ((show (lambda (buffer) (pop-to-buffer buffer :other t))))
    (or (image-view-visit path show)
        (visit-path path :safe t :show show))))

(defun facecomp-report-text (report names)
  "The text of the report REPORT, facecomp's read JSON, its photos' names
\(NAMES, as RUN-ON-COPIES gives them) written as theirs, as three values: the text,
its spans, each (START END FACE), and its buttons, each (START END ACTION),
in octets."
  (let ((out (make-string-output-stream)) (at 0) (spans '()) (buttons '()))
    (labels ((put (text &optional face)
               (let ((octets (length (tuatara.text:string-octets text))))
                 (when face (push (list at (+ at octets) face) spans))
                 (write-string text out)
                 (incf at octets)))
             (path (copy) (gethash copy names copy))
             (items (key) (coerce (tuatara.text:json-items report key) 'list))
             (photo (copy)
               ;; the photo's own path, a button opening it safely
               (let* ((file (path copy)) (start at))
                 (put (abbreviated-path file) :link)
                 (push (list start at (lambda () (facecomp-show-photo file))) buttons))))
      (let ((masters (items "masters"))
            (threshold (tuatara.text:json-get report "threshold"))
            (agreement (tuatara.text:json-get report "enrolment_agreement")))
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
                     (tuatara.text:json-text report "backend") (tuatara.text:json-integer report "embedding_dimensions")
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
              (let ((percent (tuatara.text:json-get r "match_percent"))
                    (similarity (tuatara.text:json-get r "similarity"))
                    (faces (tuatara.text:json-integer r "faces_detected")))
                (photo (tuatara.text:json-text r "photo"))
                (put (format nil "~%  "))
                (when (realp percent)
                  (put (format nil "~,1f% match, ~a" percent (or (tuatara.text:json-text r "confidence") "")) (facecomp-percent-face percent)))
                (put (format nil "~@[   similarity ~,4f~]~@[   the best of ~d faces in the photo~]~2%"
                             (and (realp similarity) similarity) (and faces (> faces 1) faces))))))))
    (values (get-output-stream-string out) (nreverse spans) (nreverse buttons))))

(defmethod tuatara.runtime:job-take-in ((job facecomp-job))
  "Show facecomp's report in *facecomp*, or say why there is none."
  (cond ((facecomp-job-report job)
         (let ((buffer (buffer-in-mode "*facecomp*" 'facecomp-mode)))
           (destructuring-bind (text spans buttons) (facecomp-job-shown job)
             (set-made-text buffer text :spans spans :buttons buttons))
           (setf (tuatara.buffer:buffer-point buffer) 0)
           (display-buffer buffer)
           (message "Faces compared: ~d photo~:p" (length (tuatara.text:json-items (facecomp-job-report job) "results")))))
        ((facecomp-job-failure job) (message "~a" (facecomp-job-failure job)))
        (t (message "~a" (tuatara.runtime:job-ending-words job)))))

;;; Choosing the photos

(defun facecomp-read-photo (prompt &optional directory)
  "Ask for a photo with PROMPT, from DIRECTORY when given; its full path,
refused when it is no file."
  (let* ((typed (if directory
                    (read-path prompt :initial (abbreviated-path directory))
                    (read-file-path prompt)))
         (path (and typed (plusp (length typed)) (tuatara.io:native (tuatara.io:expand-path typed)))))
    (unless (and path (eq :regular (tuatara.io:path-kind path)))
      (error "~a is no file" (or typed "Nothing")))
    path))

(defun facecomp-read-several (first-prompt question nth-prompt &optional directory)
  "Ask for photos one at a time: the first with FIRST-PROMPT, from DIRECTORY
when given, then, while QUESTION (given how many so far) is answered yes,
another with NTH-PROMPT (given its number), from DIRECTORY or the first one's
folder. Their full paths, in order."
  (let* ((first (facecomp-read-photo first-prompt directory))
         (directory (or directory (tuatara.io:parent-path first)))
         (photos (list first)))
    (loop for n from 1
          while (yes-or-no-p* (format nil question n))
          do (push (facecomp-read-photo (format nil nth-prompt (1+ n)) directory) photos))
    (nreverse photos)))

(defun facecomp-choose-masters (marked)
  "Ask which of the files MARKED are master photos, their names separated by
commas, the first offered; the rest are the photos compared. Two values: the
masters and the photos."
  (let* ((names (mapcar #'tuatara.io:native-file-name marked))
         (typed (read-string (format nil "Master photos of the ~d marked, separated by commas: " (length marked))
                             :initial (first names)))
         (chosen (remove "" (mapcar (lambda (s) (string-trim " " s)) (uiop:split-string typed :separator ",")) :test #'string=))
         (masters (mapcar (lambda (name)
                            (let ((at (position name names :test #'string=)))
                              (if at (nth at marked) (error "~a is not one of the marked files" name))))
                          (or chosen (list (first names))))))
    (values masters (remove-if (lambda (path) (member path masters :test #'string=)) marked))))

(defun facecomp-read-confidence ()
  "Ask for this run's detection confidence, 0.8 offered."
  (let ((value (parse-decimal (string-trim " " (read-string "Detection confidence for this run: " :initial "0.8")))))
    (unless (and value (< 0 value) (<= value 1))
      (error "The detection confidence must be above 0 and at most 1"))
    value))

(define-command facecomp-compare ()
  "Compare the faces in photos with the face of a master photo, or of several
photos of one person averaged into one template, by facecomp, and show how
alike each is in *facecomp*: a match, a confidence label (Almost certain, Very
likely, ...) and the similarity. In a rimu list with two or more files marked
in the directory it shows, asks which of them are masters (several separated
by commas) and compares the rest; elsewhere asks for the masters, then for the
photos, one at a time. With
\\[universal-argument], asks for a detection confidence for this run (0.8 is
the strict one). facecomp runs shut in the sandbox on copies of the photos, in
the background; the photos are only read."
  (let* ((buffer (current-buffer))
         ;; the photos marked in the directory shown, not those kept marked elsewhere
         (marked (rimu-chosen-paths buffer :files-only t :here-only t))
         (confidence-asked (editor-prefix-argument *editor*)))
    (multiple-value-bind (masters photos)
        (if (>= (length marked) 2)
            (facecomp-choose-masters marked)
            ;; several masters of the same person match more reliably together
            (let ((masters (facecomp-read-several "Master photo: " "Add another photo of the same person (~d so far)?"
                                                  "Master photo ~d: ")))
              (values masters (facecomp-read-several "Photo to compare: " "Add another photo to compare (~d so far)?"
                                                     "Photo ~d: " (tuatara.io:parent-path (first masters))))))
      (let ((confidence (if confidence-asked (facecomp-read-confidence) tuatara-user:*facecomp-detection-confidence*)))
        (cond ((null photos) (message "No photo to compare with the master"))
              ((null (user-program-path tuatara-user:*facecomp-program*))
               (message "facecomp is not there: install it, or give its path in *facecomp-program*"))
              (t (facecomp-check-settings confidence)
                 (tuatara.runtime:start-job-thread (make-facecomp-job buffer masters photos confidence) #'facecomp-work
                                                   :name "tuatara facecomp")
                 (message "Comparing ~d photo~:p with ~a..." (length photos) (tuatara.io:native-file-name (first masters)))))))))

;;; Loading

(defun facecomp-health-words ()
  "What compares faces here, as \\[check-health] says it."
  (let ((program (user-program-path tuatara-user:*facecomp-program*)))
    (format nil "Faces in photos compared by ~:[none: install facecomp, or give its path in *facecomp-program*~;~:*~a, shut in~]"
            (and program (abbreviated-path program)))))

(defun install-facecomp ()
  "Make the report's photos buttons, and have \\[check-health] say what
compares faces here."
  (buttons-in-mode 'facecomp-mode "the report of faces compared (\\[facecomp-compare])")
  (pushnew 'facecomp-health-words *health-decoder-functions*))

(install-facecomp)
