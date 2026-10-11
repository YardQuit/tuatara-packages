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
;;;; sentence.

(in-package #:tuatara.defaults)

;;; Settings

(defvar tuatara-user::*garamond-extra-abbreviations* '()
  "Abbreviations of your own whose full stop ends no sentence, added to
Tuatara's (*GARAMOND-ABBREVIATIONS*), each written as typed: '(\"Ph.D.\"
\"et al.\" \"Inc.\"). Read afresh at every space typed.")

(defvar tuatara-user::*garamond-removed-abbreviations* '()
  "Abbreviations of Tuatara's (*GARAMOND-ABBREVIATIONS*) that should end a
sentence after all, written as they are there: '(\"St.\" \"No.\").")

(defvar tuatara-user::*garamond-double-space-on-typing* t
  "True for \\[garamond-mode] to make one space typed after a sentence's end
two in a buffer whose sentences are two spaces apart; NIL to leave typing
alone, the spacing still said in the mode line, bracketed: (2S). Read afresh
at every space typed.")

(defvar tuatara-user::*garamond-persist* t
  "True for \\[garamond-set-spacing] and \\[garamond-adjust-spacing] to ask
whether to write the spacing in the file when the whole buffer is concerned
and the file does not say yet; NIL never to ask. A file that says already is
kept up to date without asking.")

(defvar tuatara-user::*garamond-follow-declarations* t
  "True for a file that says how far apart its sentences are (its Local
Variables: block naming sentence-end-double-space) to have \\[garamond-mode]
on as it is opened; NIL to leave it off until asked for.")

(defvar tuatara-user::*garamond-mode-line* t
  "True for the mode line to say a buffer's spacing while \\[garamond-mode]
is on in it: 2S for two spaces between sentences, 1S for one, bracketed, (2S),
while typing is left alone (*GARAMOND-DOUBLE-SPACE-ON-TYPING*); NIL for
nothing.")

(defvar tuatara-user::*garamond-unsuitable-modes* '(special-mode)
  "Major modes, those made from them included, \\[garamond-mode] never runs in:
the lists, help and terminals Tuatara makes, where nobody writes prose.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(tuatara-user::*garamond-extra-abbreviations* tuatara-user::*garamond-removed-abbreviations*
            tuatara-user::*garamond-double-space-on-typing* tuatara-user::*garamond-persist*
            tuatara-user::*garamond-follow-declarations* tuatara-user::*garamond-mode-line*
            tuatara-user::*garamond-unsuitable-modes*)
          :tuatara-user))

;;; Abbreviations

(defparameter *garamond-abbreviations*
  '("e.g." "i.e." "cf." "viz." "vs." "etc." "al." "et seq." "ibid." "q.v."
    "ca." "approx." "esp." "incl." "excl." "resp." "misc." "a.k.a." "ff."
    "p." "pp." "ed." "eds." "edn." "trans." "repr." "suppl."
    "No." "Nos." "Fig." "Figs." "Ch." "Chap." "Vol." "Vols." "Sec." "Secs."
    "Eq." "Eqs." "Ref." "Refs." "App."
    "Mr." "Mrs." "Ms." "Mx." "Messrs." "Dr." "Prof." "Rev." "Fr." "Msgr."
    "Sr." "Jr." "St." "Hon." "Pres." "Gov." "Sen." "Rep." "Amb."
    "Adm." "Capt." "Cmdr." "Col." "Cpl." "Det." "Gen." "Insp." "Lt."
    "Maj." "Pvt." "Sgt." "Supt."
    "Co." "Corp." "Bros." "Dept." "Univ." "Inst." "Mt." "Ft." "Ave."
    "Blvd." "Rd." "U.S." "U.K." "U.N." "D.C."
    "Jan." "Feb." "Mar." "Apr." "Jun." "Jul." "Aug." "Sep." "Sept."
    "Oct." "Nov." "Dec."
    "Mon." "Tue." "Tues." "Wed." "Weds." "Thu." "Thur." "Thurs." "Fri."
    "Sat." "Sun."
    "a.m." "p.m." "A.M." "P.M." "N.B." "P.S.")
  "Tuatara's abbreviations whose full stop ends no sentence as one space is
typed after it: Latin and editorial shorthand, bibliographic marks, forms of
address and rank, organisations and places, the months and days, the clock.
Degrees, company names and units (Ph.D., Inc., lb.) are not among them, as
they end sentences as often as not: *GARAMOND-EXTRA-ABBREVIATIONS* adds them,
*GARAMOND-REMOVED-ABBREVIATIONS* takes these out. Letter case counts, but one
beginning with a small letter counts capitalised too (e.g. and E.g.). Of an
abbreviation of several words, only the last may have a full stop.")

(defun garamond-abbreviations-in-force ()
  "The abbreviations that count: Tuatara's (*GARAMOND-ABBREVIATIONS*) less
*GARAMOND-REMOVED-ABBREVIATIONS*, then *GARAMOND-EXTRA-ABBREVIATIONS*; only
text that is not empty."
  (remove-if-not (lambda (a) (and (stringp a) (plusp (length a))))
                 (append (remove-if (lambda (a) (member a tuatara-user:*garamond-removed-abbreviations* :test #'equal))
                                    *garamond-abbreviations*)
                         tuatara-user:*garamond-extra-abbreviations*)))

(defvar *garamond-abbreviation-scanner* nil
  "The abbreviations in force as one pattern, with the three lists it was made
of and how many characters the longest reaches back, (SCANNER LISTS REACH);
made again when one of the lists is another.")

(defun garamond-abbreviation-scanner ()
  "Two values: a pattern matching text that ends in an abbreviation in force,
after a character that is not part of a word, and how many characters back
the longest reaches, that character included."
  (let ((lists (list *garamond-abbreviations* tuatara-user:*garamond-extra-abbreviations*
                     tuatara-user:*garamond-removed-abbreviations*)))
    (unless (and *garamond-abbreviation-scanner* (every #'eq lists (second *garamond-abbreviation-scanner*)))
      (let* ((in-force (garamond-abbreviations-in-force))
             (all (append in-force
                          (loop for a in in-force
                                when (lower-case-p (char a 0))
                                  collect (concatenate 'string (string (char-upcase (char a 0))) (subseq a 1))))))
        (setf *garamond-abbreviation-scanner*
              (list (and all (cl-ppcre:create-scanner
                              (format nil "\\W(?:~{~a~^|~})$" (mapcar #'cl-ppcre:quote-meta-chars all))))
                    lists
                    (1+ (reduce #'max all :key #'length :initial-value 0))))))
    (values (first *garamond-abbreviation-scanner*) (third *garamond-abbreviation-scanner*))))

(defun garamond-abbreviation-before-p (buffer position)
  "True when the text of BUFFER before POSITION, just after a full stop, ends
in an abbreviation in force (GARAMOND-ABBREVIATIONS-IN-FORCE) on its line."
  (multiple-value-bind (scanner reach) (garamond-abbreviation-scanner)
    (when scanner
      (let* ((line (tuatara.buffer:buffer-line-start buffer position))
             (from (loop with p = position
                         repeat reach
                         while (> p line)
                         do (setf p (tuatara.buffer:buffer-forward-char buffer p -1))
                         finally (return p)))
             (text (tuatara.buffer:buffer-string buffer :start from :end position)))
        (and (cl-ppcre:scan scanner (if (= from line) (concatenate 'string " " text) text)) t)))))

;;; Where a sentence ends

(defun garamond-sentence-end-before (buffer position)
  "When the text of BUFFER before POSITION ends a sentence, a letter then . ?
! … or ‽ then any closing quotes or brackets: the position just after its
punctuation; else NIL."
  (let ((p position))
    (loop while (and (plusp p) (sentence-closing-char-p (char-before-position buffer p)))
          do (setf p (tuatara.buffer:buffer-forward-char buffer p -1)))
    (when (and (plusp p) (sentence-end-char-p (char-before-position buffer p)))
      (let ((before (tuatara.buffer:buffer-forward-char buffer p -1)))
        (and (plusp before)
             (alpha-char-p (char-before-position buffer before))
             p)))))

(defun garamond-enumerator-p (buffer position)
  "True when the full stop of BUFFER before POSITION numbers a list item: only
spaces and digits before it on its line (1. 2. 10.)."
  (let ((line (tuatara.buffer:buffer-line-start buffer position)))
    (and (cl-ppcre:scan "^[ \\t]*[0-9]+\\.$" (tuatara.buffer:buffer-string buffer :start line :end position)) t)))

;;; Where prose is written

(defparameter *garamond-org-laid-out-types*
  '(src-block example-block export-block comment-block fixed-width
    table table-row table-cell
    link code verbatim inline-src-block
    latex-fragment latex-environment macro
    citation citation-reference footnote-reference timestamp
    keyword node-property property-drawer)
  "What Org's parse names the parts of a document that are laid out or read as
they are rather than written as prose: a second space never goes there.")

(defparameter *garamond-org-laid-out-blocks* '("src" "example" "export" "comment")
  "The Org blocks whose contents are not prose; a quote or verse block is.")

(defparameter *garamond-laid-out-names*
  "code|fence|table|link_destination|uri|url|math|formula|equation|verbatim|raw|literal|front_matter|metadata|html_block"
  "A pattern matching the names a language's syntax tree gives the parts of a
document that are not prose: code, tables, link targets, maths.")

(defun garamond-org-ranges (tree text)
  "The parts of TEXT, an Org document, and TREE, its parse, that are laid out
rather than written (*GARAMOND-ORG-LAID-OUT-TYPES*), and the blocks not
closed yet whose contents are not prose (*GARAMOND-ORG-LAID-OUT-BLOCKS*), to
the next headline or the end: each (START . END) in characters, in order."
  (let ((ranges '()) (open nil) (opened 0))
    (labels ((walk (node)
               (if (member (org-node-type node) *garamond-org-laid-out-types*)
                   (push (cons (org-node-begin node) (org-node-end node)) ranges)
                   (progn
                     (when (eq (org-node-type node) 'headline) (mapc #'walk (org-property node :title)))
                     (mapc #'walk (org-node-children node))))))
      (walk tree))
    (loop with from = 0
          do (multiple-value-bind (start end group-starts group-ends)
                 (cl-ppcre:scan "(?im)^(?:\\*+ |[ \\t]*#\\+(begin|end)_(\\S+))" text :start from)
               (unless start (return))
               (let ((kind (and (aref group-starts 0) (subseq text (aref group-starts 0) (aref group-ends 0))))
                     (name (and (aref group-starts 1) (string-downcase (subseq text (aref group-starts 1) (aref group-ends 1))))))
                 (cond ((null kind)
                        (when open (push (cons opened start) ranges))
                        (setf open nil))
                       ((string-equal kind "begin") (unless open (setf open name opened start)))
                       ((equal name open) (setf open nil))))
               (setf from (max end (1+ start)))))
    (when (and open (member open *garamond-org-laid-out-blocks* :test #'string=))
      (push (cons opened (length text)) ranges))
    (sort ranges #'< :key #'car)))

(defun garamond-in-ranges-p (ranges position)
  "True when POSITION is within one of RANGES, each (START . END), in order of
their starts."
  (loop for (start . end) in ranges
        while (<= start position)
        thereis (< position end)))

(defun garamond-half-typed-p (line-text)
  "True when LINE-TEXT, a line up to a full stop, leaves something open that
is not prose: an Org link with no ]] yet, a Markdown link's target with no )
yet, an odd number of backquotes (a code span), or an Org ~code~ or
=verbatim= begun after a blank or at the line's start and not closed yet."
  (or (cl-ppcre:scan "\\[\\[[^\\]]*$" line-text)
      (cl-ppcre:scan "\\]\\([^)]*$" line-text)
      (oddp (count #\` line-text))
      (cl-ppcre:scan "(?:^|[ \\t])(?:~[^ \\t~][^~]*|=[^ \\t=][^=]*)$" line-text)))

(defun garamond-org-section (buffer position)
  "The part of BUFFER's Org text Garamond reads around POSITION: from the
headline above it, or at most *ORG-PARSE-AT-ONCE* octets back, to the end of
POSITION's line, as two values: the text and the character of it POSITION is."
  (let* ((floor (max 0 (- position *org-parse-at-once*)))
         (start (loop with line = (tuatara.buffer:buffer-line-start buffer position)
                      do (when (or (<= line floor) (zerop line))
                           (return (tuatara.buffer:buffer-line-start buffer (max line floor))))
                         (when (and (eql (tuatara.buffer:buffer-char-at buffer line) #\*)
                                    (cl-ppcre:scan *org-outline-regex* (tuatara.buffer:buffer-line-text buffer line)))
                           (return line))
                         (setf line (tuatara.buffer:buffer-line-start buffer (1- line)))))
         (end (tuatara.buffer:buffer-line-end buffer position))
         (crlf (buffer-crlf-p buffer)))
    (flet ((text (from to) (let ((s (tuatara.buffer:buffer-string buffer :start from :end to))) (if crlf (org-strip-returns s) s))))
      (values (text start end) (length (text start position))))))

(defun garamond-org-text-prose-p (tree text index)
  "True when the character INDEX of TEXT, Org text, TREE its parse, is in
prose (GARAMOND-ORG-RANGES), its line before it leaving nothing open
\(GARAMOND-HALF-TYPED-P)."
  (not (or (garamond-in-ranges-p (garamond-org-ranges tree text) index)
           (garamond-half-typed-p (subseq text (tuatara.text:string-line-start text index) index)))))

(defun garamond-org-prose-p (buffer position)
  "True when POSITION of BUFFER, an Org buffer, is in prose, as the buffer's
parse tells when it is of the text as it is now, else the parse of the
section around POSITION (GARAMOND-ORG-SECTION)."
  (multiple-value-bind (tree text) (org-buffer-tree buffer)
    (if (and tree (eql (org-state-version (org-buffer-state buffer)) (tuatara.buffer:buffer-version buffer)))
        (garamond-org-text-prose-p tree text (org-byte->char buffer position))
        (multiple-value-bind (text index) (garamond-org-section buffer position)
          (garamond-org-text-prose-p (org-parse text) text index)))))

(defun garamond-text-mode-p (buffer)
  "True when BUFFER's major mode is one for text: text-mode and those made from
it, and fundamental-mode itself."
  (let ((mode (buffer-major-mode buffer)))
    (or (typep mode 'text-mode) (eq (type-of mode) 'fundamental-mode))))

(defun garamond-laid-out-node-p (syntax position)
  "True when POSITION of SYNTAX's text is inside a part its tree names as not
prose (*GARAMOND-LAID-OUT-NAMES*)."
  (some (lambda (node)
          (and (tuatara.syntax:tree-node-named node)
               (cl-ppcre:scan *garamond-laid-out-names* (tuatara.syntax:tree-node-name node))))
        (tuatara.syntax:node-ancestry syntax position (1+ position))))

(defun garamond-prose-p (buffer position)
  "True when POSITION of BUFFER is in prose, where a second space may go: in an
Org buffer as its parse tells (GARAMOND-ORG-PROSE-P); in another text mode
outside the code, tables, links and maths its syntax tree names, and anywhere
in one with no syntax tree; in any other language only inside a comment.
Where a syntax tree is needed and is not ready, NIL."
  (cond ((typep (buffer-major-mode buffer) 'org-mode) (garamond-org-prose-p buffer position))
        ((garamond-text-mode-p buffer)
         (or (null (tuatara.syntax:buffer-syntax buffer))
             (let ((syntax (ready-syntax buffer)))
               (and syntax
                    (not (garamond-laid-out-node-p syntax position))
                    (not (garamond-half-typed-p
                          (tuatara.buffer:buffer-string buffer :start (tuatara.buffer:buffer-line-start buffer position) :end position)))))))
        (t (let ((syntax (ready-syntax buffer)))
             (and syntax (eq (tuatara.syntax:text-kind-around syntax position) :comment))))))

;;; Typing

(defun garamond-on-p (buffer)
  "True when \\[garamond-mode] is on in BUFFER."
  (tuatara.buffer:buffer-local buffer :garamond))

(defun garamond-refusal (buffer)
  "Why \\[garamond-mode] cannot run in BUFFER (its major mode is one of
*GARAMOND-UNSUITABLE-MODES*), or NIL when it can."
  (let ((mode (buffer-major-mode buffer)))
    (when (some (lambda (m) (typep mode m)) tuatara-user:*garamond-unsuitable-modes*)
      (format nil "nobody writes prose in ~(~a~)" (type-of mode)))))

(defun garamond-typed (buffer char)
  "After CHAR was typed into BUFFER: when it is a space after a sentence's end
in prose, with \\[garamond-mode] on and the sentences two spaces apart, make
the spaces there two: one more after one, those past two taken away. Not
after an abbreviation (GARAMOND-ABBREVIATION-BEFORE-P) or a list item's
number."
  (when (and (eql char #\Space)
             (eq buffer (current-buffer))
             (garamond-on-p buffer)
             tuatara-user:*garamond-double-space-on-typing*
             (sentence-end-double-space-p buffer))
    (let* ((end (point))
           (run (loop with p = end
                      while (and (plusp p) (eql (char-before-position buffer p) #\Space))
                      do (decf p)
                      finally (return p)))
           (stop (garamond-sentence-end-before buffer run)))
      (when (and stop
                 (not (garamond-abbreviation-before-p buffer stop))
                 (not (garamond-enumerator-p buffer stop))
                 (garamond-prose-p buffer (tuatara.buffer:buffer-forward-char buffer stop -1)))
        (let ((spaces (- end run)))
          (cond ((= spaces 1)
                 (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit end end (tuatara.text:string-octets " "))) :group t)
                 (goto (1+ end)))
                ((> spaces 2)
                 (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit (+ run 2) end)) :group t)
                 (goto (+ run 2)))))))))

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
    (setf (tuatara.buffer:buffer-local b :garamond) on)
    (message "Garamond ~:[off~;on: ~:*~a~]"
             (and on (if (sentence-end-double-space-p b)
                         "two spaces between sentences"
                         "one space between sentences, so typing is left alone")))))

(defun garamond-mode-line (editor buffer)
  "Mode line function: while \\[garamond-mode] is on in BUFFER, its spacing,
2S or 1S, bracketed while typing is left alone; nothing with
*GARAMOND-MODE-LINE* NIL."
  (declare (ignore editor))
  (when (and tuatara-user:*garamond-mode-line* (garamond-on-p buffer))
    (format nil "~:[(~a)~;~a~]" tuatara-user:*garamond-double-space-on-typing*
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
         (spaces (or (editor-prefix-argument *editor*)
                     (read-whole-number "Spaces between sentences (1 or 2, ~d): " now
                                        :valid (lambda (n) (<= 1 n 2)) :refusal "1 or 2."))))
    (unless (member spaces '(1 2))
      (error "Sentences are 1 or 2 spaces apart, not ~a" spaces))
    (values spaces
            (and whole
                 (tuatara.buffer:buffer-file b)
                 (or (garamond-declared-p b)
                     (and tuatara-user:*garamond-persist*
                          (yes-or-no-p* "Write this spacing in the file?")))))))

(defun garamond-declare (buffer spaces persist)
  "Make BUFFER's sentences SPACES apart, 1 or 2, \\[garamond-mode] on unless it
cannot run there; with PERSIST and a file, write it in the file's Local
Variables: block (the buffer left modified). How the message saying so ends."
  (when (and persist (tuatara.buffer:buffer-file buffer) (tuatara.buffer:buffer-local buffer :read-only))
    (error "~a is read-only: nothing is changed" (tuatara.buffer:buffer-name buffer)))
  (setf (tuatara.buffer:buffer-local buffer :sentence-end-double-space) (= spaces 2))
  (let ((refused (garamond-refusal buffer)))
    (unless refused (setf (tuatara.buffer:buffer-local buffer :garamond) t))
    (concatenate 'string
                 (if refused (format nil "; typing is left alone, ~a" refused) "")
                 (cond ((not (and persist (tuatara.buffer:buffer-file buffer))) "")
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
          do (incf octet (tuatara.text:string-octet-length text :start index :end from))
          collect (cons octet (incf octet (tuatara.text:string-octet-length text :start from :end to)))
          do (setf index to))))

(defun garamond-gaps (buffer start end spaces)
  "The runs of spaces of BUFFER from START to END after a sentence's end in
prose that are not SPACES long, each (FROM . TO) in octets
\(GARAMOND-GAP-RUNS): not after a list item's number, nor in code, tables,
links or maths (GARAMOND-PROSE-P, Org's parse read once)."
  (let* ((text (tuatara.buffer:buffer-string buffer :start start :end end))
         (org (typep (buffer-major-mode buffer) 'org-mode))
         (ranges (when org
                   (multiple-value-bind (tree org-text) (org-tree-now buffer)
                     (garamond-org-ranges tree org-text)))))
    (flet ((prose-p (mark)
             (if org
                 (let ((at (org-byte->char buffer mark)))
                   (loop while (and ranges (<= (cdar ranges) at)) do (pop ranges))
                   (not (or (garamond-in-ranges-p ranges at)
                            (garamond-half-typed-p
                             (tuatara.buffer:buffer-string buffer :start (tuatara.buffer:buffer-line-start buffer mark) :end mark)))))
                 (garamond-prose-p buffer mark))))
      (loop for (from . to) in (runs-octets text (garamond-gap-runs text) start)
            for stop = (garamond-sentence-end-before buffer from)
            when (and stop (/= (- to from) spaces)
                      (not (garamond-enumerator-p buffer stop))
                      (prose-p (tuatara.buffer:buffer-forward-char buffer stop -1)))
              collect (cons from to)))))

(define-command garamond-adjust-spacing ()
  "Make every run of spaces after a sentence's end in the selection, or the
whole buffer without one, 1 or 2 spaces, as one change, then say the buffer
uses that spacing as \\[garamond-set-spacing] does. A sentence's end here is
. ? ! … or ‽ after a letter, with any closing quotes or brackets, before more
text on the line; abbreviations count too, so that the text is the same
throughout. Line breaks, indentation, a list item's number and what is not
prose (code, tables, links, maths) are left as they are. A count gives the
spacing, else it is asked; whether to write it in the file is asked only for
the whole buffer."
  (let* ((b (current-buffer))
         (region (and (tuatara.buffer:buffer-mark-active-p b)
                      (multiple-value-list (tuatara.buffer:buffer-region b))))
         (start (if region (first region) 0))
         (end (if region (second region) (tuatara.buffer:buffer-size b))))
    (when (tuatara.buffer:buffer-local b :read-only)
      (error "~a is read-only: nothing is changed" (tuatara.buffer:buffer-name b)))
    (unless (too-large-here-p start end "Spacing sentences")
      (multiple-value-bind (spaces persist) (garamond-read-spacing (null region))
        (let* ((gaps (garamond-gaps b start end spaces))
               (filler (tuatara.text:string-octets (make-string spaces :initial-element #\Space))))
          (when gaps
            (tuatara.buffer:buffer-edit b (map 'vector (lambda (gap) (tuatara.text:make-edit (car gap) (cdr gap) filler)) gaps)))
          (let ((said (garamond-declare b spaces persist)))
            (message "~d sentence break~:p made ~d space~:p~a" (length gaps) spaces said)))))))

;;; A file that says

(defun garamond-heed-declaration (buffer)
  "As BUFFER's file is opened: \\[garamond-mode] on when the file says how far
apart its sentences are and *GARAMOND-FOLLOW-DECLARATIONS* is on, unless it
cannot run there."
  (when (and tuatara-user:*garamond-follow-declarations* (garamond-declared-p buffer) (not (garamond-refusal buffer)))
    (setf (tuatara.buffer:buffer-local buffer :garamond) t)))

(defun garamond-file-mode (buffer name value)
  "For a file's own settings: mode: garamond (-*- mode: org; mode: garamond -*-)
has \\[garamond-mode] on in BUFFER, unless it cannot run there; true when
taken."
  (when (and (string= name "mode") (stringp value) (string-equal value "garamond"))
    (unless (garamond-refusal buffer)
      (setf (tuatara.buffer:buffer-local buffer :garamond) t))
    t))

(define-command garamond-follow-declarations-mode ()
  "Have a file that says how far apart its sentences are switch
\\[garamond-mode] on as it is opened, or stop (*GARAMOND-FOLLOW-DECLARATIONS*,
on as the package is loaded). With a count, on when it is positive, off when
it is zero or less."
  (setf tuatara-user:*garamond-follow-declarations* (switched-by-count tuatara-user:*garamond-follow-declarations*))
  (message "Files saying their sentence spacing ~:[no longer switch Garamond on~;switch Garamond on~]"
           tuatara-user:*garamond-follow-declarations*))

;;; Why nothing happens

(define-major-mode garamond-doctor-mode special-mode
  "The report of what Garamond does in a buffer and why (\\[garamond-doctor]):
\\<garamond-doctor-mode>\\[quit-window] closes it."
  :lighter "Garamond" :interactive nil)

(defun garamond-diagnosis (buffer)
  "The likeliest reason for what Garamond does in BUFFER, in a sentence or two."
  (let ((declared (garamond-declared-p buffer)) (refused (garamond-refusal buffer)))
    (cond ((and (not (garamond-on-p buffer)) refused)
           (format nil "Garamond will not run here: ~a. There is nothing for it to do." refused))
          ((and (not (garamond-on-p buffer)) (not declared) (null (tuatara.buffer:buffer-file buffer)))
           "This buffer has no file and nothing says its spacing: M-x garamond-set-spacing says it here.")
          ((and (not (garamond-on-p buffer)) (not declared))
           "Nothing says this buffer's spacing, so Garamond leaves it alone. M-x garamond-set-spacing says it and offers to write it in the file.")
          ((not (garamond-on-p buffer))
           (if tuatara-user:*garamond-follow-declarations*
               "The file says its spacing, and Garamond was switched off since, or the file was opened before the package was loaded: M-x garamond-mode switches it on."
               "The file says its spacing, but *garamond-follow-declarations* is off, so it is not followed: M-x garamond-follow-declarations-mode."))
          ((not (sentence-end-double-space-p buffer))
           "Garamond is on, and this buffer's sentences are one space apart: typing is left alone.")
          ((not tuatara-user:*garamond-double-space-on-typing*)
           "Garamond is on, and typing is left alone by *garamond-double-space-on-typing*.")
          ((and (not (typep (buffer-major-mode buffer) 'org-mode)) (tuatara.syntax:buffer-syntax buffer) (null (ready-syntax buffer)))
           "Garamond is on, and this buffer's syntax tree is not ready, so no space is doubled until it is.")
          ((not (garamond-text-mode-p buffer))
           "Garamond is on: in this language a second space goes only in comments.")
          (t "Garamond is on: a space typed after a sentence's end in prose is made two."))))

(defun garamond-report (buffer)
  "The report \\[garamond-doctor] shows of BUFFER."
  (let ((in-force (length (garamond-abbreviations-in-force))))
    (with-output-to-string (out)
      (format out "Garamond in ~a~%~%This buffer~%" (tuatara.buffer:buffer-name buffer))
      (format out "  major mode                  ~(~a~)~%" (type-of (buffer-major-mode buffer)))
      (format out "  says its spacing            ~:[no~;yes, in the file~]~%" (garamond-declared-p buffer))
      (format out "  spaces between sentences    ~d~:[  (*sentence-end-double-space*, not the file's)~;~]~%"
              (if (sentence-end-double-space-p buffer) 2 1) (garamond-declared-p buffer))
      (format out "  garamond-mode               ~:[off~;on~]~@[  (will not run here: ~a)~]~%"
              (garamond-on-p buffer) (garamond-refusal buffer))
      (format out "  a second space goes         ~a~%"
              (if (garamond-text-mode-p buffer) "in prose, not in code, tables, links or maths" "in comments only"))
      (format out "  the mode line says          ~a~%" (or (garamond-mode-line nil buffer) "nothing"))
      (format out "~%Settings~%")
      (format out "  *garamond-follow-declarations*     ~:[nil~;t~]~%" tuatara-user:*garamond-follow-declarations*)
      (format out "  *garamond-double-space-on-typing*  ~:[nil~;t~]~%" tuatara-user:*garamond-double-space-on-typing*)
      (format out "  *garamond-persist*                 ~:[nil~;t~]~%" tuatara-user:*garamond-persist*)
      (format out "  *garamond-mode-line*               ~:[nil~;t~]~%" tuatara-user:*garamond-mode-line*)
      (format out "  abbreviations in force             ~d (~d Tuatara's, ~d removed, ~d yours)~%"
              in-force (length *garamond-abbreviations*)
              (length tuatara-user:*garamond-removed-abbreviations*) (length tuatara-user:*garamond-extra-abbreviations*))
      (format out "~%Diagnosis~%  ~a~%" (garamond-diagnosis buffer)))))

(define-command garamond-doctor ()
  "Show what Garamond does in the current buffer and why, in *Garamond*:
whether its file says its spacing, the spacing in force, whether
\\[garamond-mode] is on and where a second space goes, the settings, and the
likeliest reason for what is seen."
  (let ((text (garamond-report (current-buffer)))
        (b (buffer-in-mode "*Garamond*" 'garamond-doctor-mode)))
    (set-made-text b text)
    (setf (tuatara.buffer:buffer-point b) 0)
    (display-buffer b :at :start)))

;;; Loading

(defun install-garamond ()
  "Have typing make two spaces after a sentence's end where \\[garamond-mode]
says, a file that says its spacing switch it on, and the mode line show it."
  (pushnew 'garamond-typed *post-self-insert-functions*)
  (pushnew 'garamond-heed-declaration *file-local-taken-functions*)
  (pushnew 'garamond-file-mode *file-local-setting-functions*)
  (pushnew 'garamond-mode-line *mode-line-functions*))

(install-garamond)
