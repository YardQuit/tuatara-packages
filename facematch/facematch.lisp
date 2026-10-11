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
;;;; with the candidates marked in the list, in the background. The ranking
;;;; is a rimu list of its own, *facematch*: the masters, then the candidates
;;;; the most probable first, its preview showing each, a column beside each
;;;; name giving the probability that it shows the same person, the ODNI's
;;;; words for it (Almost certain ... Almost no chance) and the similarity
;;;; behind it. \[facematch-rank-here] compares the same way, but shows the
;;;; ranking in the list itself, beside the names, the files not compared
;;;; taken out of it, the candidates' marks taken away. Candidates are those
;;;; marked in the directory the list shows; masters may be marked anywhere.
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

;;; A comparison, in the background

(defstruct (facematch-job (:include tuatara.runtime:job) (:constructor make-facematch-job (buffer masters candidates confidence &optional here)) (:copier nil))
  "The photos CANDIDATES compared with the master photos MASTERS by the
engine, with CONFIDENCE for this run or NIL, a job for BUFFER: HERE when the
ranking is to show in BUFFER itself, the ranked list kept in the background;
REPORT, what it said, read JSON; RANKING and GROUPS, its ranking as
FACEMATCH-RANKING gives them; FAILURE, why there is no report."
  masters candidates confidence here report ranking groups failure)

(setf (documentation 'facematch-job-p 'function) "True when OBJECT is a FACEMATCH-JOB.")

(tuatara.runtime:define-job-words facematch-job
  :doing "Comparing faces"
  :description (format nil "Comparing ~d candidate~:p with ~d master photo~:p"
                       (length (facematch-job-candidates job)) (length (facematch-job-masters job)))
  :verb "compare the faces"
  :left-words "nothing compared")

(defun facematch-arguments (masters candidates confidence)
  "The engine's arguments comparing CANDIDATES with MASTERS, the names of
their copies as the sandbox shows them, with CONFIDENCE for the run."
  (append (loop for m in masters append (list "--master" m))
          (and confidence (list "--detection-confidence" (format nil "~,4f" (float confidence 1d0))))
          (list "--json" "--candidate")
          candidates))

(defun facematch-work (job)
  "What a comparison does: runs the engine shut in on copies of the photos
\(RUN-ON-COPIES), and keeps its ranking, or why there is none."
  (let ((program (or (user-program-path tuatara-user:*facematch-program*)
                     (error "The facematch engine is not there: build or install it, or give its path in *facematch-program*"))))
    (multiple-value-bind (output outcome said names)
        (run-on-copies program (list (cons "master" (facematch-job-masters job)) (cons "candidate" (facematch-job-candidates job)))
                       (lambda (masters candidates) (facematch-arguments masters candidates (facematch-job-confidence job)))
                       :seconds tuatara-user:*facematch-seconds* :memory (* 16 1024 1024 1024)
                       :cancel (tuatara.runtime:job-cancel job))
      (tuatara.runtime:job-check-stop)
      (let ((report (and (eq outcome :done) (copies-output-json output))))
        (if (null report)
            (setf (facematch-job-failure job) (run-on-copies-failure "The engine" outcome said names 'tuatara-user:*facematch-seconds*))
            (multiple-value-bind (ranking groups) (facematch-ranking report names)
              (if (every #'null groups)
                  (setf (facematch-job-failure job)
                        (format nil "Nothing compared: ~{~a~^; ~}"
                                (map 'list (lambda (e) (copies-original-names e names)) (tuatara.text:json-items report "errors"))))
                  (setf (facematch-job-report job) report
                        (facematch-job-ranking job) ranking
                        (facematch-job-groups job) (remove nil groups)))))))))

;;; The ranking, in rimu

(defvar *facematch-ranking* (make-hash-table :test 'equal)
  "The ranking of the last comparison: each photo's full name to what the
ranking column shows beside it (FACEMATCH-RANKING).")

(defun facematch-label-face (percent)
  "The face a candidate PERCENT probable is shown in: Very likely and above,
Likely to Even chance, and below."
  (cond ((>= percent 80) :info)
        ((>= percent 45) :warning)
        (t :error)))

(defun facematch-ranking-cells (directory entries)
  "The ranking column's texts beside ENTRIES of DIRECTORY: each photo's
probability, the ODNI's words and the similarity, master for a master photo,
why one was not compared; nothing for a file that was not in the comparison."
  (map 'simple-vector (lambda (entry) (gethash (rimu-path directory entry) *facematch-ranking*)) entries))

(defun facematch-ranking-shown-p (buffer)
  "True when the rimu list BUFFER shows the ranking column: the ranked list of
a comparison, or a list the ranking was asked for in (\\<rimu-mode>\\[facematch-rank-here])."
  (tuatara.buffer:buffer-local buffer 'facematch-ranking))

(defun facematch-ranking-words (report)
  "The line over the ranked list saying what was compared and how."
  (let ((masters (length (tuatara.text:json-items report "masters")))
        (agreement (tuatara.text:json-get report "agreement"))
        (together (tuatara.text:json-get report "agreement_probability")))
    (format nil "~d candidate~:p ranked against ~d master photo~:p~@[ (~a)~]: ~@[~a~]~@[ finding faces by ~a~], probabilities calibrated on ~a"
            (length (tuatara.text:json-items report "results")) masters
            (and (realp agreement)
                 (format nil "the least alike two ~,4f alike, ~,1f% one person~:[~;: they may not all be the same person~]"
                         agreement together (and (realp together) (< together 50))))
            (tuatara.text:json-text report "model") (tuatara.text:json-text report "detector")
            (or (tuatara.text:json-text report "calibration") "nothing said"))))

(defun facematch-ranking (report names)
  "The ranking of REPORT, the engine's read JSON, its copies' names taken as
their photos' (NAMES, as RUN-ON-COPIES gives them), as two values: a table of
each photo's full name to what the ranking column shows beside it
\(*FACEMATCH-RANKING*), and a list of the masters and of the candidates in
ranked order, those not compared after, as paths."
  (let ((ranking (make-hash-table :test 'equal)) (masters '()) (ranked '()) (left-out '()))
    (flet ((path (copy) (gethash copy names copy)))
      (loop for m across (tuatara.text:json-items report "masters")
            do (let ((photo (path m)))
                 (push photo masters)
                 (setf (gethash photo ranking) (cons "master" :rimu-marked-other))))
      (loop for r across (tuatara.text:json-items report "results")
            do (let ((photo (path (tuatara.text:json-text r "photo")))
                     (percent (tuatara.text:json-get r "probability"))
                     (similarity (tuatara.text:json-get r "similarity"))
                     (faces (tuatara.text:json-integer r "faces")))
                 (push photo ranked)
                 (setf (gethash photo ranking)
                       (list (cons (format nil "~5,1f%  ~17a" (if (realp percent) percent 0)
                                           (or (tuatara.text:json-text r "label") ""))
                                   (facematch-label-face (if (realp percent) percent 0)))
                             (cons (format nil "~@[ similarity ~,4f~]~@[  likeliest of ~d faces~]"
                                           (and (realp similarity) similarity) (and faces (> faces 1) faces))
                                   nil)))))
      ;; each photo the engine could not compare, as "COPY: why"
      (loop for e across (tuatara.text:json-items report "errors")
            do (let* ((colon (search ": " e))
                      (photo (and colon (gethash (subseq e 0 colon) names))))
                 (when photo
                   (unless (gethash photo ranking) (push photo left-out))
                   (setf (gethash photo ranking)
                         (cons (format nil "not compared: ~a" (one-line (subseq e (+ colon 2)))) :warning)))))
      (values ranking (list (nreverse masters) (nconc (nreverse ranked) (nreverse left-out)))))))

(defmethod tuatara.runtime:job-take-in ((job facematch-job))
  "Show the ranking as a rimu list, the masters first, then the candidates
ranked, each with its ranking beside it; or say why there is none."
  (let ((report (facematch-job-report job)))
    (cond (report
           (let ((here (and (facematch-job-here job) (live-buffer-p (facematch-job-buffer job)) (facematch-job-buffer job)))
                 (old (tuatara.buffer:find-buffer "*facematch*")))
             (setf *facematch-ranking* (facematch-job-ranking job))
             ;; a ranked list of an earlier comparison would show this one's ranking beside its photos
             (when old (tuatara.buffer:kill-buffer old))
             (if here
                 (facematch-show-ranking-in here (facematch-job-candidates job))
                 (let ((list (rimu-show-paths (facematch-job-groups job) :name "*facematch*"
                                                                         :words (facematch-ranking-words report) :safe t)))
                   (setf (tuatara.buffer:buffer-local list 'facematch-ranking) t
                         (tuatara.buffer:buffer-local list 'rimu-details) nil)
                   (message "Faces compared: ~d candidate~:p ranked; the preview shows each"
                            (length (tuatara.text:json-items report "results")))))))
          ((facematch-job-failure job) (message "~a" (facematch-job-failure job)))
          (t (message "~a" (tuatara.runtime:job-ending-words job))))))

(defun facematch-show-ranking-in (buffer candidates)
  "Show the last comparison's ranking beside the photos of the rimu list
BUFFER, the lines of the other files taken out, as \\<rimu-mode>\\[rimu-take-out-lines] takes them,
and the marks of CANDIDATES, the photos compared, taken away."
  (let ((directory (tuatara.buffer:buffer-local buffer 'rimu-directory))
        (marks (rimu-marks buffer))
        (out (make-hash-table :test 'equalp)) (shown 0))
    (dolist (path candidates)
      (when (eql (gethash path marks) #\*) (remhash path marks)))
    (rimu-each-line buffer (lambda (line entry)
                             (declare (ignore line))
                             (if (gethash (rimu-path directory entry) *facematch-ranking*)
                                 (incf shown)
                                 (setf (gethash (tuatara.io:listed-octets entry) out) t))))
    (setf (tuatara.buffer:buffer-local buffer 'facematch-ranking) t
          (tuatara.buffer:buffer-local buffer 'rimu-taken-out) out)
    ;; read again: a column's texts are found as the directory is read
    (rimu-update buffer :reread t)
    (message "~d photo~:p ranked here, the other lines taken out; \\<rimu-mode>\\[facematch-rank-here] shows the list as it was" shown)))

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
  "The master photos marked (M) in every rimu list and every directory it has
shown, in the order shown."
  (remove-duplicates (loop for buffer in tuatara.buffer:*buffers*
                           when (rimu-list-p buffer)
                             append (rimu-chosen-paths buffer :files-only t :mark *facematch-master-mark*))
                     :test #'string= :from-end t))

(defun facematch-read-confidence ()
  "Ask for this run's detection confidence, 0.8 offered."
  (let ((value (parse-decimal (string-trim " " (read-string "Detection confidence for this run: " :initial "0.8")))))
    (unless (and value (< 0 value) (<= value 1))
      (error "The detection confidence must be above 0 and at most 1"))
    value))

(defun facematch-start (here)
  "Compare the master photos marked (M, in any rimu list) with the candidates
marked (m) in the directory the current rimu list shows, in the background;
with HERE the ranking is to show in this list, else in a list of its own."
  (let* ((buffer (current-buffer))
         (confidence-asked (editor-prefix-argument *editor*))
         (masters (facematch-masters))
         (directory (tuatara.buffer:buffer-local buffer 'rimu-directory))
         (candidates (and (rimu-list-p buffer)
                          (loop for entry in (rimu-chosen buffer #\*)
                                for path = (tuatara.io:native (rimu-path directory entry))
                                when (and (eq (rimu-leads-to entry) :regular) (not (member path masters :test #'string=)))
                                  collect path))))
    (cond ((not (rimu-list-p buffer)) (message "Candidates are marked in a rimu list (\\[rimu]), and compared from it"))
          ((null masters) (message "No master photo is marked: \\<rimu-mode>\\[facematch-mark-master] marks one (M)"))
          ((null candidates) (message "No candidate is marked in this directory: \\<rimu-mode>\\[rimu-mark] marks one"))
          ((null (user-program-path tuatara-user:*facematch-program*))
           (message "The facematch engine is not there: build or install it, or give its path in *facematch-program*"))
          (t (let ((confidence (if confidence-asked (facematch-read-confidence) tuatara-user:*facematch-detection-confidence*)))
               (when (and confidence (not (and (realp confidence) (< 0 confidence) (<= confidence 1))))
                 (error "*facematch-detection-confidence* must be above 0 and at most 1, not ~s" confidence))
               (tuatara.runtime:start-job-thread (make-facematch-job buffer masters candidates confidence here) #'facematch-work
                                                 :name "tuatara facematch")
               (message "Comparing ~d candidate~:p with ~d master photo~:p..." (length candidates) (length masters)))))))

(define-command facematch-compare ()
  "Rank the candidate photos marked (m) in the directory this rimu list shows
by how probable it is that each shows the person of the master photos marked
\(\\<rimu-mode>\\[facematch-mark-master], M, in any list), in a rimu list of their own, *facematch*:
the masters, then the candidates, the most probable first, each with its
probability, the ODNI's words for it (Almost certain, Very likely, Likely,
Even chance, Unlikely, Very unlikely, Almost no chance) and the similarity,
the preview showing each. With \\[universal-argument], asks for a detection
confidence for this run (0.8, the strict one). The engine runs shut in the
sandbox on copies of the photos, in the background; the photos are only read."
  (facematch-start nil))

(define-command facematch-rank-here ()
  "Rank the candidates as \\[facematch-compare] does, and show the ranking in
this rimu list, beside the photos' names, the lines of the files not compared
taken out, as \\<rimu-mode>\\[rimu-take-out-lines] takes them, the candidates' marks taken away
\(\\[facematch-compare] shows the ranking in a list of its own).
Where the ranking shows already, show the list as it was instead."
  (let ((buffer (current-buffer)))
    (if (and (rimu-list-p buffer) (tuatara.buffer:buffer-local buffer 'facematch-ranking))
        (progn (setf (tuatara.buffer:buffer-local buffer 'facematch-ranking) nil
                     (tuatara.buffer:buffer-local buffer 'rimu-taken-out) nil)
               (rimu-update buffer :reread t)
               (message "Ranking hidden"))
        (facematch-start t))))

;;; Loading

(defun facematch-health-words ()
  "What compares faces by probability here, as \\[check-health] says it."
  (let ((program (user-program-path tuatara-user:*facematch-program*)))
    (format nil "Faces ranked by probability by ~:[none: build the facematch engine, or give its path in *facematch-program*~;~:*~a, shut in~]"
            (and program (abbreviated-path program)))))

(defun install-facematch ()
  "Bind facematch's keys in rimu, give rimu the ranking column, and have
\\[check-health] say what it runs."
  ;; under C-c f, a letter rimu leaves free: the one-letter keys of a list are under C-c too
  (define-keys *rimu-keymap* '(("C-c f m" facematch-mark-master) ("C-c f f" facematch-compare) ("C-c f r" facematch-rank-here)))
  (label-keys *rimu-keymap* '(("C-c f" "faces")))
  (set-rimu-column :facematch #'facematch-ranking-cells :place :before :shown 'facematch-ranking-shown-p
                   :documentation "The ranking of the last comparison of faces (facematch).")
  (commands-for-mode 'rimu-mode "a rimu list, with the facematch package" 'facematch-mark-master 'facematch-compare 'facematch-rank-here)
  (pushnew 'facematch-health-words *health-decoder-functions*))

(install-facematch)
