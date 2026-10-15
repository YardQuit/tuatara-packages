;;;; garamond/garamond.lisp --- The spaces between sentences, kept by the document itself: garamond.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package, not part of the editor: loaded by (load-package "garamond") in
;;;; the init file. Made from the Emacs package garamond
;;;; (github.com/yardquit/garamond).
;;;;
;;;; One space or two after a full stop is a property of a document, so the
;;;; document says which in its own Local Variables: block,
;;;;
;;;;   # Local Variables:
;;;;   # sentence-end-double-space: t
;;;;   # End:
;;;;
;;;; and a file that says nothing is left alone. M-x garamond-set-spacing says
;;;; which a buffer uses and offers to write it in the file; M-x
;;;; garamond-adjust-spacing does the same and makes the text match; M-x
;;;; garamond-mode, on by itself in a file that says, makes one space typed
;;;; after a sentence's end two, and the mode line says 2S or 1S. Filling and
;;;; the sentence commands follow the same spacing (*sentence-end-double-space*).
;;;;
;;;; A second space goes only where prose is written: in a text mode (Org,
;;;; Markdown, plain text and the rest), never in code, tables, links, maths or
;;;; a block's contents, as Org's parse or the language's syntax tree tells
;;;; them; in any other language, only in its comments, as its syntax tree
;;;; tells them. Where a language has a syntax tree and it is not ready, nothing
;;;; is doubled. A word whose full stop shortens it (e.g., Dr., Sept.) ends no
;;;; sentence: Tuatara's abbreviations (*sentence-abbreviations*) and your own.

(defpackage #:garamond
  (:use #:cl #:tuatara)
  (:documentation "The spaces between sentences, kept by the document itself."))

(in-package #:garamond)

;;; Settings

(define-package-setting *garamond-double-space-on-typing* t
  "True for \\[garamond-mode] to make one space typed after a sentence's end
two in a buffer whose sentences are two spaces apart; NIL to leave typing
alone, the spacing still said in the mode line, bracketed: (2S). Read afresh
at every space typed.")

(define-package-setting *garamond-persist* t
  "True for \\[garamond-set-spacing] and \\[garamond-adjust-spacing] to ask
whether to write the spacing in the file when the whole buffer is concerned
and the file does not say yet; NIL never to ask. A file that says already is
kept up to date without asking.")

(define-package-setting *garamond-follow-declarations* t
  "True for a file that says how far apart its sentences are (its Local
Variables: block naming sentence-end-double-space) to have \\[garamond-mode]
on as it is opened; NIL to leave it off until asked for.")

(define-package-setting *garamond-mode-line* t
  "True for the mode line to say a buffer's spacing while \\[garamond-mode]
is on in it: 2S for two spaces between sentences, 1S for one, bracketed, (2S),
while typing is left alone (*GARAMOND-DOUBLE-SPACE-ON-TYPING*); NIL for
nothing.")

(define-package-setting *garamond-unsuitable-modes* '(special-mode)
  "Major modes, those made from them included, \\[garamond-mode] never runs in:
the lists, help and terminals Tuatara makes, where nobody writes prose.")

;;; Where a sentence ends

(defun garamond-sentence-end-before (buffer position)
  "When the text of BUFFER before POSITION ends a sentence, a letter then . ?
! … or ‽ then any closing quotes or brackets: the position just after its
punctuation; else NIL. A list item's number (1. 10.) ends none, as no letter
comes before its full stop."
  (let ((p position))
    (loop while (and (plusp p) (sentence-closing-char-p (char-before-position buffer p)))
          do (setf p (buffer-forward-char buffer p -1)))
    (when (and (plusp p) (sentence-end-char-p (char-before-position buffer p)))
      (let ((before (buffer-forward-char buffer p -1)))
        (and (plusp before)
             (alpha-char-p (char-before-position buffer before))
             p)))))

;;; Typing

(defun garamond-on-p (buffer)
  "True when \\[garamond-mode] is on in BUFFER."
  (buffer-local buffer :garamond))

(defun garamond-refusal (buffer)
  "Why \\[garamond-mode] cannot run in BUFFER (its major mode is one of
*GARAMOND-UNSUITABLE-MODES*), or NIL when it can."
  (let ((mode (buffer-major-mode buffer)))
    (when (some (lambda (m) (typep mode m)) *garamond-unsuitable-modes*)
      (format nil "nobody writes prose in ~(~a~)" (type-of mode)))))

(defun garamond-typed (buffer char)
  "After CHAR was typed into BUFFER: when it is a space after a sentence's end
in prose, with \\[garamond-mode] on and the sentences two spaces apart, make
the spaces there two: one more after one, those past two taken away. Not
after an abbreviation (ABBREVIATION-BEFORE-P) or a list item's
number."
  (when (and (eql char #\Space)
             (eq buffer (current-buffer))
             (garamond-on-p buffer)
             *garamond-double-space-on-typing*
             (sentence-end-double-space-p buffer))
    (let* ((end (buffer-point buffer))
           (run (loop with p = end
                      while (and (plusp p) (eql (char-before-position buffer p) #\Space))
                      do (decf p)
                      finally (return p)))
           (stop (garamond-sentence-end-before buffer run)))
      (when (and stop
                 (not (abbreviation-before-p buffer stop))
                 (prose-p buffer (buffer-forward-char buffer stop -1)))
        (let ((spaces (- end run)))
          (cond ((= spaces 1)
                 (delete-spans buffer (list (cons end end)) :replacements '(" ") :group t)
                 (setf (buffer-point buffer) (1+ end)))
                ((> spaces 2)
                 (delete-spans buffer (list (cons (+ run 2) end)) :group t)
                 (setf (buffer-point buffer) (+ run 2)))))))))

(define-command garamond-mode ()
  "Make one space typed after a sentence's end two in the current buffer, or
stop. It acts only while the buffer's sentences are two spaces apart (the mode
line says 2S; 1S while they are one apart, the mode on and waiting), only in
prose: in a text mode outside code, tables, links and maths, in any other
language only in its comments; never after an abbreviation (e.g., Dr.) or a
list item's number. A run of more spaces typed is made two. On by itself in a
file that says how far apart its sentences are, and by \\[garamond-set-spacing]
and \\[garamond-adjust-spacing]. Never in the buffers of
*GARAMOND-UNSUITABLE-MODES*. With a count, on when it is positive, off when it
is zero or less."
  (let* ((b (current-buffer)) (on (switched-by-count (garamond-on-p b))))
    (when on
      (let ((refused (garamond-refusal b)))
        (when refused (error "Garamond will not run here: ~a" refused))))
    (setf (buffer-local b :garamond) on)
    (message "Garamond ~:[off~;on: ~:*~a~]"
             (and on (if (sentence-end-double-space-p b)
                         "two spaces between sentences"
                         "one space between sentences, so typing is left alone")))))

(defun garamond-mode-line (editor buffer)
  "Mode line function: while \\[garamond-mode] is on in BUFFER, its spacing,
2S or 1S, bracketed while typing is left alone; nothing with
*GARAMOND-MODE-LINE* NIL."
  (declare (ignore editor))
  (when (and *garamond-mode-line* (garamond-on-p buffer))
    (format nil "~:[(~a)~;~a~]" *garamond-double-space-on-typing*
            (if (sentence-end-double-space-p buffer) "2S" "1S"))))

;;; Saying which spacing a buffer uses

(defun garamond-declared-p (buffer)
  "True when BUFFER's file says how far apart its sentences are."
  (nth-value 1 (file-declares buffer "sentence-end-double-space")))

(defun garamond-read-spacing (whole)
  "The spacing for the command running, and whether to write it in the file,
as two values: the count when given, else asked (1 or 2, the buffer's own
offered); written when WHOLE (the whole buffer is concerned) and the buffer
has a file that says already, or *GARAMOND-PERSIST* and the answer is yes."
  (let* ((b (current-buffer))
         (now (if (sentence-end-double-space-p b) 2 1))
         (spaces (or (prefix-argument)
                     (read-whole-number "Spaces between sentences (1 or 2, ~d): " now
                                        :valid (lambda (n) (<= 1 n 2)) :refusal "1 or 2."))))
    (unless (member spaces '(1 2))
      (error "Sentences are 1 or 2 spaces apart, not ~a" spaces))
    (values spaces
            (and whole
                 (buffer-file b)
                 (or (garamond-declared-p b)
                     (and *garamond-persist*
                          (yes-or-no-p* "Write this spacing in the file?")))))))

(defun garamond-declare (buffer spaces persist)
  "Make BUFFER's sentences SPACES apart, 1 or 2, \\[garamond-mode] on unless it
cannot run there; with PERSIST and a file, write it in the file's Local
Variables: block (the buffer left modified). How the message saying so ends."
  (when (and persist (buffer-file buffer))
    (check-writable buffer))
  (setf (buffer-local buffer :sentence-end-double-space) (= spaces 2))
  (let ((refused (garamond-refusal buffer)))
    (unless refused (setf (buffer-local buffer :garamond) t))
    (concatenate 'string
                 (if refused (format nil "; typing is left alone, ~a" refused) "")
                 (cond ((not (and persist (buffer-file buffer))) "")
                       ((not *file-local-variables*) "; not written in the file, a file's own settings being off (*file-local-variables*)")
                       (t (set-file-local-setting buffer "sentence-end-double-space" (= spaces 2))
                          "; written in the file")))))

(define-command garamond-set-spacing ()
  "Say how many spaces, 1 or 2, are between the current buffer's sentences,
changing none of its text: filling, the sentence commands and typing
\(\\[garamond-mode], switched on) follow it. A count gives it (C-u 2), else
it is asked. When the buffer's file does not say yet, asks whether to write it
in the file's Local Variables: block, where the file keeps it from then on
\(*GARAMOND-PERSIST*); a file that says already is kept up to date. The
buffer is left modified, not saved. \\[garamond-adjust-spacing] makes the text
match as well."
  (multiple-value-bind (spaces persist) (garamond-read-spacing t)
    (let ((said (garamond-declare (current-buffer) spaces persist)))
      (message "This buffer has ~d space~:p between sentences~a" spaces said))))

(defun garamond-gap-runs (text)
  "The runs of spaces in TEXT after a sentence's end, before more text on the
same line, each (FROM . TO) in characters, in order: after . ? ! … or ‽
\(*SENTENCE-ENDERS*) and any closing quotes or brackets (*SENTENCE-CLOSERS*)."
  (let ((scanner (cl-ppcre:create-scanner (format nil "[~a][~a]*( +)(?=[^ \\t\\r\\n])"
                                                  (cl-ppcre:quote-meta-chars *sentence-enders*)
                                                  (cl-ppcre:quote-meta-chars *sentence-closers*))))
        (runs '()))
    (cl-ppcre:do-scans (start end group-starts group-ends scanner text)
      (push (cons (aref group-starts 0) (aref group-ends 0)) runs))
    (nreverse runs)))

(defun runs-octets (text runs start)
  "RUNS of TEXT, each (FROM . TO) in characters, in order, as (FROM . TO) in
octets of a buffer whose text from the octet START is TEXT."
  (let ((index 0) (octet start))
    (loop for (from . to) in runs
          do (incf octet (string-octet-length text :start index :end from))
          collect (cons octet (incf octet (string-octet-length text :start from :end to)))
          do (setf index to))))

(defun garamond-gaps (buffer start end spaces)
  "The runs of spaces of BUFFER from START to END after a sentence's end in
prose that are not as long as they should be, each (FROM TO . WIDE) in octets
\(GARAMOND-GAP-RUNS), WIDE the spaces it should have: SPACES, or one after an
abbreviation (ABBREVIATION-BEFORE-P: e.g., Dr.), whose full stop ends no
sentence; not after a list item's number, nor in code, tables, links or maths
\(PROSE-TEST, Org's parse read once)."
  (let ((text (buffer-string buffer :start start :end end))
        (prose-p (prose-test buffer)))
    (loop for (from . to) in (runs-octets text (garamond-gap-runs text) start)
          for stop = (garamond-sentence-end-before buffer from)
          for wide = (if (and stop (abbreviation-before-p buffer stop)) 1 spaces)
          when (and stop (/= (- to from) wide)
                    (funcall prose-p (buffer-forward-char buffer stop -1)))
          collect (list* from to wide))))

(define-command garamond-adjust-spacing ()
  "Make every run of spaces after a sentence's end in the selection, or the
whole buffer without one, 1 or 2 spaces, as one change, then say the buffer
uses that spacing as \\[garamond-set-spacing] does. A sentence's end here is
. ? ! … or ‽ after a letter, with any closing quotes or brackets, before more
text on the line. After an abbreviation's full stop (e.g., Dr.) there is always one
space, as filling leaves it. Line breaks, indentation, a list item's number and what is not
prose (code, tables, links, maths) are left as they are. A count gives the
spacing, else it is asked; whether to write it in the file is asked only for
the whole buffer."
  (let* ((b (current-buffer))
         (region (selected-region b))
         (start (if region (first region) 0))
         (end (if region (second region) (buffer-size b))))
    (check-writable b)
    (unless (too-large-here-p start end "Spacing sentences")
      (multiple-value-bind (spaces persist) (garamond-read-spacing (null region))
        (let* ((gaps (garamond-gaps b start end spaces))
               (after-abbreviations (count-if (lambda (gap) (/= (cddr gap) spaces)) gaps)))
          (when gaps
            (delete-spans b (mapcar (lambda (gap) (cons (first gap) (second gap))) gaps)
                          :replacements (mapcar (lambda (gap) (make-string (cddr gap) :initial-element #\Space)) gaps)))
          (let ((said (garamond-declare b spaces persist)))
            (message "~d sentence break~:p made ~d space~:p~[~:;, ~:*~d after an abbreviation one~]~a"
                     (- (length gaps) after-abbreviations) spaces after-abbreviations said)))))))

;;; A file that says

(defun garamond-heed-declaration (buffer)
  "As BUFFER's file is opened: \\[garamond-mode] on when the file says how far
apart its sentences are and *GARAMOND-FOLLOW-DECLARATIONS* is on, unless it
cannot run there."
  (when (and *garamond-follow-declarations* (garamond-declared-p buffer) (not (garamond-refusal buffer)))
    (setf (buffer-local buffer :garamond) t)))

(defun garamond-file-mode (buffer name value)
  "For a file's own settings: mode: garamond (-*- mode: org; mode: garamond -*-)
has \\[garamond-mode] on in BUFFER, unless it cannot run there; true when
taken."
  (when (and (string= name "mode") (stringp value) (string-equal value "garamond"))
    (unless (garamond-refusal buffer)
      (setf (buffer-local buffer :garamond) t))
    t))

(define-command garamond-follow-declarations-mode ()
  "Have a file that says how far apart its sentences are switch
\\[garamond-mode] on as it is opened, or stop (*GARAMOND-FOLLOW-DECLARATIONS*,
on as the package is loaded). With a count, on when it is positive, off when
it is zero or less."
  (setf *garamond-follow-declarations* (switched-by-count *garamond-follow-declarations*))
  (message "Files saying their sentence spacing ~:[no longer switch Garamond on~;switch Garamond on~]"
           *garamond-follow-declarations*))

;;; Why nothing happens

(defun garamond-diagnosis (buffer)
  "The likeliest reason for what Garamond does in BUFFER, in a sentence or two."
  (let ((declared (garamond-declared-p buffer)) (refused (garamond-refusal buffer)))
    (cond ((and (not (garamond-on-p buffer)) refused)
           (format nil "Garamond will not run here: ~a. There is nothing for it to do." refused))
          ((and (not (garamond-on-p buffer)) (not declared) (null (buffer-file buffer)))
           "This buffer has no file and nothing says its spacing: \\[garamond-set-spacing] says it here.")
          ((and (not (garamond-on-p buffer)) (not declared))
           "Nothing says this buffer's spacing, so Garamond leaves it alone. \\[garamond-set-spacing] says it and offers to write it in the file.")
          ((not (garamond-on-p buffer))
           (if *garamond-follow-declarations*
               "The file says its spacing, and Garamond was switched off since, or the file was opened before the package was loaded: \\[garamond-mode] switches it on."
               "The file says its spacing, but *garamond-follow-declarations* is off, so it is not followed: \\[garamond-follow-declarations-mode]."))
          ((not (sentence-end-double-space-p buffer))
           "Garamond is on, and this buffer's sentences are one space apart: typing is left alone.")
          ((not *garamond-double-space-on-typing*)
           "Garamond is on, and typing is left alone by *garamond-double-space-on-typing*.")
          ((not (prose-ready-p buffer))
           "Garamond is on, and this buffer's syntax tree is not ready, so no space is doubled until it is.")
          ((not (prose-buffer-p buffer))
           "Garamond is on: in this language a second space goes only in comments.")
          (t "Garamond is on: a space typed after a sentence's end in prose is made two."))))

(defun garamond-report (buffer)
  "The report \\[garamond-doctor] shows of BUFFER, written as help is (SHOW-HELP
with :FILL): the diagnosis a paragraph, the rest lines set in."
  (let ((in-force (length (sentence-abbreviations-in-force))))
    (with-output-to-string (out)
      (format out "Garamond in ~a~%~%This buffer~%" (buffer-name buffer))
      (format out "  major mode                  ~(~a~)~%" (type-of (buffer-major-mode buffer)))
      (format out "  says its spacing            ~:[no~;yes, in the file~]~%" (garamond-declared-p buffer))
      (format out "  spaces between sentences    ~d~:[  (*sentence-end-double-space*, not the file's)~;~]~%"
              (if (sentence-end-double-space-p buffer) 2 1) (garamond-declared-p buffer))
      (format out "  garamond-mode               ~:[off~;on~]~@[  (will not run here: ~a)~]~%"
              (garamond-on-p buffer) (garamond-refusal buffer))
      (format out "  a second space goes         ~a~%"
              (if (prose-buffer-p buffer) "in prose, not in code, tables, links or maths" "in comments only"))
      (format out "  the mode line says          ~a~%" (or (garamond-mode-line nil buffer) "nothing"))
      (format out "~%Settings~%")
      (format out "  *garamond-follow-declarations*     ~:[nil~;t~]~%" *garamond-follow-declarations*)
      (format out "  *garamond-double-space-on-typing*  ~:[nil~;t~]~%" *garamond-double-space-on-typing*)
      (format out "  *garamond-persist*                 ~:[nil~;t~]~%" *garamond-persist*)
      (format out "  *garamond-mode-line*               ~:[nil~;t~]~%" *garamond-mode-line*)
      (format out "  abbreviations in force             ~d (~d Tuatara's, ~d removed, ~d yours)~%"
              in-force (length *sentence-abbreviations*)
              (length *removed-sentence-abbreviations*) (length *extra-sentence-abbreviations*))
      (format out "~%Diagnosis~%~%~a~%" (garamond-diagnosis buffer)))))

(define-command garamond-doctor ()
  "Show in *Help* what Garamond does in the current buffer and why: whether
its file says its spacing, the spacing in force, whether \\[garamond-mode] is
on and where a second space goes, the settings, and the likeliest reason for
what is seen. \\[revert-buffer] there looks again."
  (let ((buffer (current-buffer)))
    (show-help (lambda ()
                 (if (live-buffer-p buffer)
                     (garamond-report buffer)
                     (format nil "Garamond~%~%The buffer asked about is gone.~%")))
               :fill t)))

;;; Loading

(defun install-garamond ()
  "Have typing make two spaces after a sentence's end where \\[garamond-mode]
says, a file that says its spacing switch it on, and the mode line show it."
  (pushnew 'garamond-typed *post-self-insert-functions*)
  (pushnew 'garamond-heed-declaration *file-local-taken-functions*)
  (pushnew 'garamond-file-mode *file-local-setting-functions*)
  (pushnew 'garamond-mode-line *mode-line-functions*))

(install-garamond)
