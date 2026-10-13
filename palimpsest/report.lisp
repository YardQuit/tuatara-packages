;;;; palimpsest/report.lisp --- The report on a PDF: its summary, findings and revisions, each revision written out with its log.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x palimpsest examines a PDF in the background and shows what it found
;;;; in a buffer of its own, read only: a summary, the findings strongest
;;;; first, then each revision from the original to the latest with what it
;;;; added, changed and freed and how its pages' text and drawing changed.
;;;; Each revision is written as a PDF of its own (the file's octets up to
;;;; its %%EOF line) beside a log saying how it was found and every sign in
;;;; it, with the offsets and octets that show it, so that the same can be
;;;; done again by hand with a hex editor. They are written to a folder of
;;;; the package's own in the cache directory, for you alone; the PDF itself
;;;; is only read.

(in-package #:palimpsest)

(define-package-setting *palimpsest-largest-written* (* 2 1024 1024 1024)
  "The most octets the revisions of one PDF, each written out as a PDF of its
own, may take together: 2 GB as standard. Revisions past it are listed as not
written, as they are when the drive would keep less than 1 GB free.")

(define-package-setting *palimpsest-largest-shown* (* 1024 1024)
  "The most octets of an object's stream shown by \\<palimpsest-mode>\\[palimpsest-show-object]: 1 MB as standard.")

(defparameter *palimpsest-version* "1.0"
  "The version of the palimpsest package, as its logs name it.")

(define-major-mode palimpsest-mode special-mode
  "The report on a PDF examined for signs of editing (\\[palimpsest]), read
only: a summary, the findings (strong signs of editing, then those worth a
look, then those for the record), and each revision from the original to the
latest. \\<palimpsest-mode>\\[push-button] on a revision's PDF opens it in Tuatara's PDF view, on its
log opens the log, on an object's number shows the object; \\[forward-button] and
\\[backward-button] go from one of these to the next. \\[palimpsest-show-object] shows an object's dictionary and
stream (as text, or in hex when it is not text; always in hex with
\\[universal-argument]); \\[palimpsest-save] saves the revisions, their logs and the report to a
folder of your choosing; \\[quit-window] closes. Closing the report deletes its own copies of
the revisions, asking first when they were not saved."
  :lighter "Palimpsest" :interactive nil)

(define-major-mode palimpsest-object-mode special-mode
  "An object of a PDF examined by \\[palimpsest], read only: where it is in the
file, its dictionary as written, and its stream decoded, as text or in hex.
\\<palimpsest-object-mode>\\[quit-window] closes."
  :lighter "PDF object" :interactive nil)

(defvar *palimpsest-reports* '()
  "The buffers showing a report on a PDF.")

(defstruct (analysis (:copier nil))
  "What examining a PDF found: PATH and NAME of the file, SIZE, SHA-256; PDF,
its structure (PDF); ACCOUNTS, each revision compared with the one before;
FINDINGS; FOLDER, where its revisions were written; FILES, each revision's
\(PDF-PATH LOG-PATH SHA-256), or (NIL NIL WHY) when not written; SHOWN, the
report's text, faces and buttons (as PIECES-TEXT gives them), made in the
background; SAVED, the folder its revisions, logs and report were saved to,
or NIL while they are not."
  path name size sha256 pdf (accounts '()) (findings '()) folder (files '()) shown saved)

(setf (documentation 'analysis-p 'function) "True when OBJECT is an ANALYSIS.")

;;; The work in the background

(defstruct (palimpsest-job (:include job) (:constructor make-palimpsest-job (buffer path folder)) (:copier nil))
  "The PDF at PATH being examined for BUFFER, its revisions written to
FOLDER, a job: PHASE, what it does now, PROGRESS, how far it has got from 0
to 1; ANALYSIS, what it found."
  path folder (phase "Reading the PDF") (progress 0) analysis)

(setf (documentation 'palimpsest-job-p 'function) "True when OBJECT is a PALIMPSEST-JOB.")

(define-job-words palimpsest-job
  :doing (palimpsest-job-phase job)
  :description (format nil "Examining ~a for signs of editing" (native-file-name (palimpsest-job-path job)))
  :verb "examine the PDF"
  :left-words "nothing found")

(defmethod job-percent ((job palimpsest-job))
  "How far examining the PDF has got."
  (running-percent (round (* 1000 (palimpsest-job-progress job))) 1000))

(defun stage (job phase from to)
  "A function of a fraction, moving JOB's progress from FROM to TO, its phase PHASE."
  (setf (palimpsest-job-phase job) phase (palimpsest-job-progress job) from)
  (lambda (fraction) (setf (palimpsest-job-progress job) (+ from (* (- to from) (min 1 (max 0 fraction)))))))

(defun write-file-octets (path octets end)
  "Write the first END of OCTETS as a new file at PATH, readable by you alone and written by nothing again (0400)."
  (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :element-type '(unsigned-byte 8) :if-exists :error)
    (write-sequence octets out :end end))
  (sb-posix:chmod path #o400))

(defun write-file-text (path text)
  "Write TEXT as a new file at PATH in UTF-8, readable by you alone and written by nothing again (0400)."
  (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :external-format :utf-8 :if-exists :error)
    (write-string text out))
  (sb-posix:chmod path #o400))

(defun examine (job)
  "What a job examining a PDF does: the file read whole and summed, its
structure read, each revision compared with the one before, every sign
found, and each revision written out with its log."
  (let* ((path (palimpsest-job-path job))
         (octets (call-with-file path (lambda (fd name size) (docx-file-octets fd name size *palimpsest-largest-file*))))
         (analysis (make-analysis :path path :name (native-file-name path) :size (length octets) :folder (palimpsest-job-folder job))))
    (funcall (stage job "Summing the PDF" 0.02 0.05) 0)
    (setf (analysis-sha256 analysis) (file-sha256 path :cancel (job-cancel job)))
    (job-check-stop)
    (let* ((pdf (read-pdf octets :path path :name (analysis-name analysis) :progress (stage job "Reading its saves" 0.05 0.35))))
      (setf (analysis-pdf analysis) pdf)
      (let ((compare (stage job "Comparing its saves" 0.35 0.6)) (revisions (pdf-revisions pdf)))
        (setf (analysis-accounts analysis)
              (loop for revision across revisions for k from 1
                    collect (prog1 (compare-revision pdf revision) (funcall compare (/ k (length revisions)))))))
      (setf (analysis-findings analysis) (all-findings pdf (analysis-accounts analysis) (stage job "Looking for signs of editing" 0.6 0.85)))
      (write-revisions job analysis)
      (setf (analysis-shown analysis) (multiple-value-list (pieces-text (report-pieces analysis)))
            (palimpsest-job-analysis job) analysis))))

(defun write-revisions (job analysis)
  "Write each revision of ANALYSIS's PDF to its folder as revision-N.pdf,
with revision-N.log beside it, as far as *PALIMPSEST-LARGEST-WRITTEN* and the
free space allow."
  (let* ((pdf (analysis-pdf analysis)) (folder (analysis-folder analysis)) (written 0)
         (progress (stage job "Writing its revisions" 0.85 0.99))
         (revisions (pdf-revisions pdf)))
    (setf (analysis-files analysis)
          (loop for revision across revisions for k from 1
                collect (progn
                          (job-check-stop)
                          (funcall progress (/ k (length revisions)))
                          (let ((end (revision-end revision)) (free (ignore-errors (free-bytes folder))))
                            (cond ((> (+ written end) *palimpsest-largest-written*)
                                   (list nil nil "not written: the revisions together would pass *palimpsest-largest-written*"))
                                  ((and free (< free (+ end (expt 2 30))))
                                   (list nil nil "not written: the drive of the cache directory would keep less than 1 GB free"))
                                  (t (incf written end)
                                     (let ((file (path-join folder (format nil "revision-~d.pdf" k)))
                                           (log (path-join folder (format nil "revision-~d.log" k))))
                                       (write-file-octets file (pdf-octets pdf) end)
                                       (let ((sum (file-sha256 file :cancel (job-cancel job))))
                                         (write-file-text log (revision-log analysis revision sum))
                                         (list file log sum)))))))))))

;;; The log of a revision

(defun strength-words (strength)
  "STRENGTH, a finding's, in words."
  (ecase strength (:strong "Strong sign of editing") (:look "Worth a look") (:record "For the record")))

(defun revision-log (analysis revision sum)
  "The log of REVISION of ANALYSIS's PDF, its own file's SHA-256 being SUM:
how it was found, every sign in it with its evidence and the steps to see it
by hand, what it adds, changes and frees, and how it differs from the
revision before. The same file gives the same log but for its date line."
  (let* ((pdf (analysis-pdf analysis)) (octets (pdf-octets pdf)) (k (1+ (revision-index revision)))
         (before (and (> k 1) (aref (pdf-revisions pdf) (- k 2))))
         (account (nth (revision-index revision) (analysis-accounts analysis))))
    (with-output-to-string (s)
      (format s "Palimpsest log of revision ~d of ~a~%~a~%~%" k (analysis-name analysis) (make-string 60 :initial-element #\=))
      (format s "Source file:  ~a~%Size:         ~:d octets (~a)~%SHA-256:      ~a~%" (analysis-name analysis) (analysis-size analysis)
              (hex-offset (analysis-size analysis)) (or (analysis-sha256 analysis) "(could not be summed)"))
      (format s "Written:      ~a~%" (format-time "%F %T %z" (get-universal-time)))
      (format s "Written by:   palimpsest ~a, a third-party package for the Tuatara editor~%~%" *palimpsest-version*)
      ;; how it was found
      (format s "How this revision was found~%---------------------------~%")
      (format s "Its %%EOF is at ~a; its line ends at ~a.~%" (offset-words (revision-eof revision)) (offset-words (revision-end revision)))
      (if (revision-startxref-at revision)
          (format s "Its startxref keyword is at ~a and names ~:[nothing that can be read~;~:*offset ~a~].~%"
                  (offset-words (revision-startxref-at revision)) (and (revision-startxref revision) (offset-words (revision-startxref revision))))
          (format s "It has no startxref keyword.~%"))
      (format s "Its cross-reference section ~:[could not be found: its objects were found by scanning its octets~;~:*is ~a at ~a~].~%"
              (case (revision-xref-kind revision)
                (:table "a table (the keyword xref)") (:stream "a stream") (:hybrid "a table with a stream beside it (/XRefStm)") (t nil))
              (and (revision-xref-at revision) (offset-words (revision-xref-at revision))))
      (when (revision-trailer-at revision)
        (format s "Its trailer is at ~a:~%" (offset-words (revision-trailer-at revision)))
        (dolist (line (octets-evidence "  the trailer" octets (revision-trailer-at revision)
                                       (min (revision-eof revision) (+ (revision-trailer-at revision) 240))))
          (format s "  ~a~%" line)))
      (let ((prev (revision-prev revision)))
        (if prev
            (let ((named (find prev (pdf-revisions pdf) :key #'revision-xref-at)))
              (format s "Its trailer's /Prev is ~d (~a), ~:[which is the cross-reference section of no revision~;~:*the cross-reference section of ~a~].~%"
                      prev (hex-offset prev) (and named (revision-name named))))
            (format s "Its trailer names no /Prev~:[~; (it is the original)~].~%" (= k 1))))
      (dolist (problem (revision-problems revision)) (format s "Not as it should be: ~a.~%" problem))
      (format s "~%This revision is the source file's octets from 0x00000000 to ~a, the end of its %%EOF line included: ~:d octets.~%"
              (hex-offset (1- (revision-end revision))) (revision-end revision))
      (format s "revision-~d.pdf SHA-256: ~a~%" k (or sum "(could not be summed)"))
      (format s "To check: copy the source file's first ~:d octets into a new file; its SHA-256 is the one above.~%~%" (revision-end revision))
      ;; the signs
      (let ((signs (remove-if-not (lambda (f) (eql (finding-revision f) (revision-index revision))) (analysis-findings analysis))))
        (format s "Signs found in this revision: ~d~%------------------------------~%" (length signs))
        (loop for f in signs for n from 1
              do (format s "~%[~d] ~a: ~a~%" n (strength-words (finding-strength f)) (finding-words f))
                 (when (finding-page f) (format s "    Page: ~d~%" (finding-page f)))
                 (when (finding-objects f) (format s "    Objects: ~{~d~^, ~}~%" (subseq (finding-objects f) 0 (min 50 (length (finding-objects f))))))
                 (when (finding-rule f) (format s "    Rule: ~a~%" (finding-rule f)))
                 (when (finding-details f)
                   (format s "    Details:~%")
                   (dolist (d (finding-details f)) (format s "      ~a~%" d)))
                 (when (finding-evidence f)
                   (format s "    Evidence:~%")
                   (dolist (e (finding-evidence f)) (format s "      ~a~%" e)))
                 (when (finding-steps f)
                   (format s "    Steps to see it by hand:~%")
                   (loop for step in (finding-steps f) for m from 1 do (format s "      ~d. ~a~%" m step)))))
      ;; the objects
      (format s "~%Objects~%-------~%")
      (flet ((place (r n)
               (let ((o (revision-object pdf r n)))
                 (cond ((null o) "not in the file")
                       ((pobj-packed-in o) (format nil "in object stream ~d at ~a of its decoded octets" (pobj-packed-in o) (offset-words (pobj-at o))))
                       (t (format nil "at ~a" (offset-words (pobj-at o))))))))
        (format s "Added (~d):~%" (length (revision-added revision)))
        (dolist (n (revision-added revision))
          (format s "  object ~d ~a~@[, ~a~]~%" n (place revision n) (role-words pdf revision n)))
        (format s "Changed (~d):~%" (length (revision-changed revision)))
        (dolist (n (revision-changed revision))
          (format s "  object ~d now ~a, before ~a~@[, ~a~]~%" n (place revision n) (place before n) (role-words pdf revision n)))
        (when (revision-same revision)
          (format s "Written again unchanged (~d):~%" (length (revision-same revision)))
          (dolist (n (revision-same revision)) (format s "  object ~d now ~a, before ~a~%" n (place revision n) (place before n))))
        (format s "Freed (~d):~%" (length (revision-freed revision)))
        (dolist (n (revision-freed revision))
          (format s "  object ~d, before ~a~@[, ~a~]~%" n (place before n) (role-words pdf before n))))
      ;; against the revision before
      (when before
        (format s "~%Changes against revision ~d~%--------------------------~%" (1- k))
        (if (null (account-changes account))
            (format s "No page draws anything different.~%")
            (dolist (change (account-changes account))
              (let ((page (page-change-page change)))
                (format s "~%Page ~d~:[ (new in this revision)~;~]:~%" (1+ (page-index page)) (page-change-old change))
                (dolist (line (page-change-text change))
                  (format s "  text ~:[-~;+~] ~a~%" (eq (first line) :added) (second line)))
                (dolist (m (page-change-added change))
                  (format s "  drawn now: ~a~%" (mark-words page m))
                  (dolist (e (mark-evidence pdf revision m)) (format s "    ~a~%" e)))
                (dolist (m (page-change-removed change))
                  (format s "  drawn before: ~a~%" (mark-words (or (page-change-old change) page) m))
                  (dolist (e (mark-evidence pdf before m)) (format s "    ~a~%" e))))))
        (dolist (page (account-gone account))
          (format s "~%Page ~d of revision ~d is gone.~%" (1+ (page-index page)) (1- k)))))))

;;; The report

(defun summary-verdict (findings)
  "The verdict of FINDINGS, in words."
  (cond ((find :strong findings :key #'finding-strength) "Strong signs that the file was edited after it was made.")
        ((find :look findings :key #'finding-strength) "Signs worth a look, none of them strong.")
        (t "No sign of editing found.")))

(defun object-pieces (analysis revision numbers &optional (most 60))
  "Pieces naming the objects NUMBERS of REVISION, each a button showing it, at most MOST of them."
  (let ((pieces '()))
    (loop for n in numbers for k from 0
          while (< k most)
          do (when (plusp k) (push ", " pieces))
             (push (list (princ-to-string n) :link (let ((n n)) (lambda () (show-object analysis revision n)))) pieces))
    (when (> (length numbers) most) (push (format nil " and ~:d more" (- (length numbers) most)) pieces))
    (nreverse pieces)))

(defun report-pieces (analysis)
  "The pieces of the report on ANALYSIS: its summary, findings and revisions."
  (let* ((pdf (analysis-pdf analysis)) (last (last-revision pdf)) (findings (analysis-findings analysis))
         (pieces '()))
    (labels ((put (text &optional face action) (push (if action (list text face action) (if face (cons text face) text)) pieces))
             (line (label value &optional face)
               (when value (put (format nil "  ~12a" label) :type) (put (format nil "~a~%" value) face)))
             (heading (text) (put (format nil "~%~a~%" text) :heading)))
      (put (format nil "Palimpsest: ~a~%" (analysis-name analysis)) :heading)
      (put (format nil "Read from the file's octets alone: nothing in it was run, and the file was not changed.~%"))
      (put (format nil "~a~%" (substitute-command-keys "\\<palimpsest-mode>\\[push-button] on a link opens it; \\[palimpsest-show-object] shows an object.")) :comment)
      (heading "* Summary")
      (line "File" (abbreviated-path (analysis-path analysis)))
      (line "Size" (format nil "~:d octets (~a), ~a" (analysis-size analysis) (hex-offset (analysis-size analysis)) (octets-words (analysis-size analysis))))
      (line "SHA-256" (or (analysis-sha256 analysis) "could not be summed"))
      (line "PDF version" (or (pdf-version pdf) "not given"))
      (line "Revisions" (format nil "~d: the original~[~; and 1 later save~:;~:* and ~d later saves~]" (length (pdf-revisions pdf)) (1- (length (pdf-revisions pdf)))))
      (line "Pages" (princ-to-string (length (pages-of pdf last))))
      (line "Verdict" (summary-verdict findings) (if (find :strong findings :key #'finding-strength) :warning :strong))
      (line "Findings" (format nil "~d strong sign~:p of editing, ~d worth a look, ~d for the record"
                               (count :strong findings :key #'finding-strength) (count :look findings :key #'finding-strength)
                               (count :record findings :key #'finding-strength)))
      (flet ((date (key) (let ((d (info-text pdf last key))) (and d (pdf-date-words d)))))
        (line "Made" (date "/CreationDate"))
        (line "Changed" (date "/ModDate")))
      (line "Saved to" (and (analysis-saved analysis) (abbreviated-path (analysis-saved analysis))))
      (line "Creator" (info-text pdf last "/Creator"))
      (line "Producer" (info-text pdf last "/Producer"))
      (dolist (note (reverse (pdf-notes pdf))) (line "Not read" note :warning))
      ;; findings
      (heading "* Findings")
      (when (null findings) (put (format nil "  None.~%")))
      (dolist (strength *strengths*)
        (let ((some (remove strength findings :key #'finding-strength :test-not #'eq)))
          (when some
            (heading (format nil "** ~a (~d)" (ecase strength (:strong "Strong signs of editing") (:look "Worth a look") (:record "For the record")) (length some)))
            (dolist (f some)
              (put "- ")
              (put (format nil "~a~%" (finding-words f)) (if (eq strength :strong) :warning nil))
              (let ((revision (aref (pdf-revisions pdf) (finding-revision f))))
                (put (format nil "    ~a~@[, page ~d~]" (revision-name revision) (finding-page f)) :comment)
                (when (finding-objects f)
                  (put ", objects " :comment)
                  (setf pieces (revappend (object-pieces analysis revision (finding-objects f) 20) pieces)))
                (put (string #\Newline)))
              (dolist (d (finding-details f)) (put (format nil "    ~a~%" d) :comment))))))
      ;; revisions
      (heading "* Revisions")
      (loop for revision across (pdf-revisions pdf)
            for account in (analysis-accounts analysis)
            for (file log why) in (analysis-files analysis)
            do (heading (format nil "** ~@(~a~): ~a to ~a, ~:d octets" (revision-name revision)
                                (hex-offset (revision-start revision)) (hex-offset (1- (revision-end revision)))
                                (- (revision-end revision) (revision-start revision))))
               (put (format nil "  ~12a" "Written as") :type)
               (if file
                   (progn (put (native-file-name file) :link (let ((file file)) (lambda () (visit-path file))))
                          (put "  its log ")
                          (put (native-file-name log) :link (let ((log log)) (lambda () (visit-path log))))
                          (put (string #\Newline)))
                   (put (format nil "~a~%" why) :warning))
               (line "Index" (format nil "~a~@[ at ~a~]~@[, startxref at ~a~]"
                                     (case (revision-xref-kind revision) (:table "a cross-reference table") (:stream "a cross-reference stream")
                                       (:hybrid "a table and a stream (hybrid)") (t "none: objects found by scanning"))
                                     (and (revision-xref-at revision) (hex-offset (revision-xref-at revision)))
                                     (and (revision-startxref-at revision) (hex-offset (revision-startxref-at revision)))))
               (multiple-value-bind (info number) (info-of pdf revision)
                 (declare (ignore info))
                 (line "Trailer" (format nil "~@[/Info object ~d~]~@[, /ID <~{~a~^> <~}>~]~@[, /Prev ~a~]" number
                                         (let ((id (listed (dget (revision-trailer revision) "/ID")))) (and id (every #'stringp id) (mapcar #'id-hex id)))
                                         (and (revision-prev revision) (hex-offset (revision-prev revision))))))
               (line "Claims" (let ((made (info-text pdf revision "/CreationDate")) (changed (info-text pdf revision "/ModDate"))
                                    (producer (info-text pdf revision "/Producer")))
                                (and (or made changed producer)
                                     (format nil "~@[made ~a~]~@[, changed ~a~]~@[, by ~a~]" (and made (pdf-date-words made)) (and changed (pdf-date-words changed)) producer))))
               (dolist (part (list (list "Adds" (revision-added revision) revision) (list "Changes" (revision-changed revision) revision)
                                   (list "Frees" (revision-freed revision) (if (plusp (revision-index revision)) (aref (pdf-revisions pdf) (1- (revision-index revision))) revision))))
                 (destructuring-bind (label numbers in) part
                   (when numbers
                     (put (format nil "  ~12a" label) :type)
                     (put (format nil "~:d object~:p: " (length numbers)))
                     (setf pieces (revappend (object-pieces analysis in numbers) pieces))
                     (put (string #\Newline)))))
               (dolist (problem (revision-problems revision)) (line "Not right" problem :warning))
               (when (account-changes account)
                 (line "Pages" (format nil "changed: ~{~d~^, ~}" (mapcar (lambda (c) (1+ (page-index (page-change-page c)))) (account-changes account)))))
               (dolist (change (account-changes account))
                 (let ((page (page-change-page change)))
                   (when (page-change-text change)
                     (put (format nil "  Page ~d, its text:~%" (1+ (page-index page))) :strong)
                     (loop for (kind text) in (page-change-text change) repeat 60
                           do (put (format nil "    ~:[-~;+~] ~a~%" (eq kind :added) (shorten text 120)) (if (eq kind :added) :type :comment))))
                   (when (or (page-change-added change) (page-change-removed change))
                     (put (format nil "  Page ~d, what it draws:~%" (1+ (page-index page))) :strong)
                     (loop for m in (page-change-added change) repeat 40
                           do (put (format nil "    + ~a~%" (mark-words page m)) :type))
                     (loop for m in (page-change-removed change) repeat 40
                           do (put (format nil "    - ~a~%" (mark-words (or (page-change-old change) page) m)) :comment)))))
               (dolist (page (account-gone account))
                 (put (format nil "  Page ~d of the revision before is gone.~%" (1+ (page-index page))) :warning))))
    (nreverse pieces)))

(defun report-render (buffer analysis)
  "Show ANALYSIS in BUFFER, its report: its text and buttons, point at its start."
  (setf (buffer-local buffer 'analysis) analysis)
  (destructuring-bind (text spans buttons) (or (analysis-shown analysis) (multiple-value-list (pieces-text (report-pieces analysis))))
    (set-made-text buffer text :spans spans :buttons buttons)
    (setf (buffer-point buffer) 0)))

(defmethod job-take-in ((job palimpsest-job))
  "Show what examining the PDF found, or say why it could not be examined."
  (show-reading-result job (palimpsest-job-path job)
                       (lambda (buffer) (report-render buffer (palimpsest-job-analysis job)))
                       "~a could not be examined: ~a~%"))

(defun palimpsest-buffer (native)
  "The buffer of the report on the PDF at NATIVE: the one there is, else a
new one, the PDF examined for it in the background (EXAMINE)."
  (let ((name (native-file-name native)))
    (reading-buffer '*palimpsest-reports* (lambda (b) (buffer-local b 'palimpsest-path)) native
                    (format nil "*Palimpsest: ~a*" name) (format nil "Examining ~a for signs of editing...~%" name) 'palimpsest-mode
                    (lambda (buffer)
                      (let ((folder (new-private-folder (private-cache-directory "palimpsest/"))))
                        (setf (buffer-local buffer 'palimpsest-path) native
                              (buffer-local buffer 'palimpsest-folder) folder)
                        (start-job-thread (make-palimpsest-job buffer native folder) #'examine :name "tuatara palimpsest"))))))

(defun palimpsest-close-query (buffer)
  "Kill-buffer query: true to close BUFFER, a report on a PDF, at once when
its revisions and logs were saved (\\<palimpsest-mode>\\[palimpsest-save]) or none were written;
else asked first, as closing deletes them."
  (let ((analysis (buffer-local buffer 'analysis)))
    (or (null analysis) (analysis-saved analysis) (null (buffer-local buffer 'palimpsest-folder))
        (notany #'first (analysis-files analysis))
        (yes-or-no-p* "Revisions and logs not saved: close and delete them?"))))

(defun palimpsest-forget (buffer)
  "Kill-buffer function: when BUFFER is a report on a PDF, delete the
revisions and logs it wrote, in the background, once its job, stopped as the
buffer goes, has ended."
  (let ((folder (buffer-local buffer 'palimpsest-folder))
        (job (find-if #'palimpsest-job-p (jobs-of buffer))))
    (when folder
      (setf (buffer-local buffer 'palimpsest-folder) nil)
      (make-guarded-thread
       (lambda ()
         (loop while (and job (job-running-p job)) do (sleep 0.2))
         (ignore-errors (delete-tree folder)))
       :name "tuatara palimpsest deleted"))))

;;; Saving the revisions, their logs and the report

(defun folder-name (path)
  "PATH, a folder's native name, ending in one /."
  (concatenate 'string (string-right-trim "/" path) "/"))

(defun inside-folder-p (path folder)
  "True when PATH is FOLDER or within it, both native names."
  (let ((path (folder-name path)) (folder (folder-name folder)))
    (and (<= (length folder) (length path)) (string= folder path :end2 (length folder)))))

(defun saved-names (analysis)
  "The files saving ANALYSIS makes, each (SOURCE NAME SUM): each revision's PDF
and log written (SUM, a PDF's SHA-256, or NIL to be worked out), then the
report (SOURCE NIL)."
  (append (loop for (file log sum) in (analysis-files analysis)
                when file append (list (list file (native-file-name file) sum) (list log (native-file-name log) nil)))
          (list (list nil "report.txt" nil))))

(defun save-refusal (analysis folder)
  "Why the revisions, logs and report of ANALYSIS cannot be saved to FOLDER,
in words; NIL when they can."
  (let ((evidence (parent-path (analysis-path analysis)))
        (cache (private-cache-directory "palimpsest/")))
    (cond ((notany #'first (analysis-files analysis)) "no revision was written to save")
          ((string= (folder-name folder) (folder-name evidence))
           "it is the folder of the PDF examined: nothing is written beside the evidence")
          ((inside-folder-p folder cache) "it is within palimpsest's own folder in the cache, which is cleared")
          ((and (path-kind folder) (not (eq :directory (path-kind folder)))) "it is not a folder")
          (t (let ((there (loop for (nil name) in (saved-names analysis)
                                when (path-kind (path-join (folder-name folder) name) :follow nil) collect name)))
               (and there (format nil "~{~a~^, ~} ~:[is~;are~] there already: nothing is written over" there (rest there))))))))

(defun save-copies (analysis folder &key (cancel (list nil)))
  "Copy ANALYSIS's revisions and logs to FOLDER (made when it is not there),
and write the report there as report.txt, ending with where each copy is and
its SHA-256, each checked against its original. Returns what was saved, each
\(NAME SHA-256); an error naming the first copy whose sum differs."
  (let ((folder (folder-name folder)) (saved '()))
    (ensure-directories-exist (sb-ext:parse-native-namestring folder))
    (loop for (source name sum) in (saved-names analysis)
          when source
            do (let ((target (path-join folder name)))
                 (unless (copy-regular-file source target :cancel cancel) (job-check-stop))
                 (let ((original (or sum (file-sha256 source :cancel cancel)))
                       (copy (file-sha256 target :cancel cancel)))
                   (unless (and original (equal original copy))
                     (error "the copy of ~a at ~a does not have its original's SHA-256" name target))
                   (push (list name copy) saved))))
    (let ((report (path-join folder "report.txt")))
      (write-file-text report
                       (with-output-to-string (s)
                         (write-string (first (analysis-shown analysis)) s)
                         (format s "~%* Saved~%~%Saved to ~a on ~a.~%Each copy's SHA-256, checked against its original:~%~%"
                                 folder (format-time "%F %T %z" (get-universal-time)))
                         (loop for (name sum) in (reverse saved) do (format s "  ~a  ~a~a~%" sum folder name))))
      (push (list "report.txt" (file-sha256 report :cancel cancel)) saved))
    (setf (analysis-saved analysis) folder)
    (nreverse saved)))

(defstruct (palimpsest-save-job (:include job) (:constructor make-palimpsest-save-job (buffer analysis folder)) (:copier nil))
  "Saving the revisions, logs and report of ANALYSIS to FOLDER for BUFFER, a job; SAVED, what was saved (SAVE-COPIES)."
  analysis folder saved)

(setf (documentation 'palimpsest-save-job-p 'function) "True when OBJECT is a PALIMPSEST-SAVE-JOB.")

(define-job-words palimpsest-save-job
  :doing "Saving the revisions"
  :description (format nil "Saving the revisions and logs to ~a" (palimpsest-save-job-folder job))
  :verb "save the revisions"
  :left-words "some copies may have been made")

(defmethod job-take-in ((job palimpsest-save-job))
  "Say what was saved, its sums checked, and show the report again with where; or why it was not."
  (let ((analysis (palimpsest-save-job-analysis job)) (buffer (job-buffer job)))
    (if (palimpsest-save-job-saved job)
        (progn
          (message "Saved ~d files to ~a, each copy's SHA-256 checked against its original"
                   (length (palimpsest-save-job-saved job)) (abbreviated-path (palimpsest-save-job-folder job)))
          (when (live-buffer-p buffer)
            (setf (analysis-shown analysis) (multiple-value-list (pieces-text (report-pieces analysis))))
            (let ((point (buffer-point buffer)))
              (report-render buffer analysis)
              (setf (buffer-point buffer) (min point (buffer-size buffer))))))
        (message "Not saved: ~a" (job-ending-words job)))))

(define-command palimpsest-save ()
  "Save the revisions of the PDF the report is on, each with its log, and
the report itself as report.txt, to a folder asked for (made when it is not
there): the work to keep, as the report's own copies go when it is closed.
Each copy's SHA-256 is checked against its original, and report.txt ends
with where each copy is and its sum. Never the PDF's own folder, nor the
package's own folder in the cache; a file there already is never written over: nothing is saved,
and which are there is said. Copied in the background."
  (let* ((buffer (current-buffer))
         (analysis (buffer-local buffer 'analysis)))
    (if (null analysis)
        (message "The report is not ready yet")
        (let ((folder (read-native-path "Save the revisions and logs to folder: ")))
          (when folder
            (let ((refusal (save-refusal analysis folder)))
              (if refusal
                  (message "Not saved to ~a: ~a" (abbreviated-path folder) refusal)
                  (start-job-thread (make-palimpsest-save-job buffer analysis folder)
                                    (lambda (job)
                                      (setf (palimpsest-save-job-saved job)
                                            (save-copies analysis folder :cancel (job-cancel job))))
                                    :name "tuatara palimpsest saving"))))))))

;;; Objects shown

(defun hex-dump (octets &key (start 0) (end (length octets)))
  "OCTETS from START below END as a hex dump: each line an offset, sixteen octets in hex and as text."
  (with-output-to-string (s)
    (loop for at from start below end by 16
          do (let ((stop (min end (+ at 16))))
               (format s "~8,'0x  ~{~2,'0x ~}~va |~a|~%" at (coerce (subseq octets at stop) 'list)
                       (* 3 (- 16 (- stop at))) "" (text-words octets at stop 16))))))

(defun text-like-p (octets)
  "True when the first 4 KB of OCTETS look like text: nearly all printable ASCII, tabs and line ends."
  (let ((n (min 4096 (length octets))))
    (or (zerop n)
        (>= (count-if (lambda (o) (or (<= 32 o 126) (member o '(9 10 13)))) octets :end n) (* 0.95 n)))))

(defun object-text (analysis revision number &key hex)
  "The text showing the object NUMBER as REVISION of ANALYSIS's PDF has it:
where it is, what it is, its dictionary as written and its stream decoded,
as text or (with HEX, or when it is not text) as a hex dump."
  (let* ((pdf (analysis-pdf analysis)) (octets (pdf-octets pdf))
         (object (revision-object pdf revision number)))
    (with-output-to-string (s)
      (format s "Object ~d of ~a, as ~a has it~%~%" number (analysis-name analysis) (revision-name revision))
      (if (null object)
          (format s "It is not in this revision.~%")
          (progn
            (dolist (line (object-evidence pdf revision number)) (format s "~a~%" line))
            (let ((role (role-words pdf revision number))) (when role (format s "It is ~a.~%" role)))
            (format s "~%Its dictionary, as written:~%~%")
            (if (pobj-packed-in object)
                (multiple-value-bind (pairs data) (packed-objects pdf (revision-object pdf revision (pobj-packed-in object)))
                  (declare (ignore pairs))
                  (format s "~a~%" (octets-latin-1 data :start (pobj-at object) :end (min (length data) (pobj-end object) (+ (pobj-at object) 65536)))))
                (format s "~a~%" (octets-latin-1 octets :start (pobj-at object)
                                                        :end (min (or (pobj-stream-start object) (pobj-end object)) (+ (pobj-at object) 65536)))))
            (when (pobj-stream-start object)
              (multiple-value-bind (data why) (stream-data pdf object)
                (format s "~%Its stream: ~:d octets in the file, ~a to ~a; filters: ~a; ~:[~:d octets decoded~;~:*~a~*~].~%~%"
                        (- (pobj-stream-end object) (pobj-stream-start object))
                        (hex-offset (pobj-stream-start object)) (hex-offset (pobj-stream-end object)) (filter-names object)
                        (and why (if (member why *picture-filters* :test #'equal) (format nil "picture data (~a), not decoded" why) why))
                        (length (or data #())))
                (when data
                  (let ((end (min (length data) *palimpsest-largest-shown*)))
                    (if (or hex (not (text-like-p data)))
                        (write-string (hex-dump data :end end) s)
                        (format s "~a~%" (octets-latin-1 data :end end)))
                    (when (< end (length data))
                      (format s "~%... ~:d octets more (*palimpsest-largest-shown*)~%" (- (length data) end))))))))))))

(defun show-object (analysis revision number &key hex)
  "Show the object NUMBER as REVISION of ANALYSIS's PDF has it in a buffer of its own, read only (OBJECT-TEXT)."
  (let* ((name (format nil "*Palimpsest: ~a object ~d, revision ~d*" (analysis-name analysis) number (1+ (revision-index revision))))
         (buffer (or (find-buffer name) (make-buffer name))))
    (set-major-mode buffer 'palimpsest-object-mode)
    (setf (buffer-local buffer :read-only) t)
    (set-made-text buffer (object-text analysis revision number :hex hex))
    (setf (buffer-point buffer) 0)
    (pop-to-buffer buffer)
    buffer))

(define-command palimpsest-show-object ()
  "Show an object of the PDF the report is on, read only: its place in the
file, its dictionary as written and its stream decoded (at most
*palimpsest-largest-shown*), as text, or as a hex dump when it is not text or
with \\[universal-argument]. Asks for its number, offering the object whose number is at
point, and shows it as the latest revision has it (the revision at point, on
an object named under a revision)."
  (let ((analysis (buffer-local (current-buffer) 'analysis)))
    (if (null analysis)
        (message "The report is not ready yet")
        (let* ((pdf (analysis-pdf analysis))
               (point (buffer-point (current-buffer)))
               (line (buffer-line-text (current-buffer) (buffer-line-start (current-buffer) point)))
               (at-point (let ((start (buffer-line-start (current-buffer) point)))
                           (multiple-value-bind (s e) (cl-ppcre:scan "\\d+" line :start (max 0 (min (length line) (- point start 6))))
                             (and s (<= s (- point start) (1+ e)) (parse-integer line :start s :end e)))))
               (number (read-whole-number "Object number~@[ (~d)~]: " at-point :valid (lambda (n) (>= n 0)) :refusal "Not an object number."))
               (revision (or (report-revision-at analysis) (last-revision pdf))))
          (when number
            (show-object analysis revision number :hex (prefix-argument)))))))

(defun report-revision-at (analysis)
  "The revision whose part of the report point is in, or NIL when it is in none."
  (let* ((buffer (current-buffer))
         (text (buffer-string buffer :start 0 :end (buffer-point buffer)))
         (at (search "** Revision " text :from-end t)))
    (when (and at (search "* Revisions" text))
      (let ((k (parse-integer text :start (+ at 12) :junk-allowed t)))
        (and k (<= 1 k (length (pdf-revisions (analysis-pdf analysis)))) (aref (pdf-revisions (analysis-pdf analysis)) (1- k)))))))

;;; Asking for a PDF

(defun pdf-path-p (path)
  "True when the file at PATH begins as a PDF does (%PDF- in its first 1,024 octets)."
  (let ((head (ignore-errors (file-head path 1024))))
    (and head (search (load-time-value (bytes "%PDF-")) head) t)))

(define-command palimpsest ()
  "Examine a PDF for signs of editing, from its octets alone: every revision
from the original to the latest, what each added, changed and freed and how
its pages changed, and every sign of editing or forgery that examiners know,
each with its evidence. The file the current buffer shows when it is a PDF
\(a PDF's view, a visited PDF, a PDF on rimu's line), else, or with
\\[universal-argument], one asked for. The report is made in the background. Nothing in
the PDF is run and the file is never changed; each revision is written, with
a log of how to find its signs by hand, to a folder for you alone."
  (let* ((shown (and (not (prefix-argument)) (shown-file (current-buffer))))
         (path (if (and shown (pdf-path-p (native (expand-path shown)))) shown (read-file-path "PDF to examine: "))))
    (when path
      (let ((native (native (expand-path path))))
        (cond ((not (eq :regular (ignore-errors (path-kind native)))) (message "~a is not a file" (abbreviated-path native)))
              ((not (pdf-path-p native)) (message "~a is not a PDF: it does not begin with %PDF-" (abbreviated-path native)))
              (t (show-buffer (palimpsest-buffer native))))))))
