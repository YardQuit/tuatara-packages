;;;; palimpsest/pdf.lisp --- A PDF read from its octets alone: its saves, cross-reference sections, objects and streams.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A PDF changed after it was made keeps what it was: each save appends the
;;;; objects it changes, a cross-reference section saying where they are, a
;;;; trailer pointing back to the section before, and an %%EOF line. Here the
;;;; file is read once, whole, as octets: every object written in it found by
;;;; scanning its body, every save (revision) found by its %%EOF and read by
;;;; its cross-reference section (a table, a stream, or both), and the
;;;; objects each revision holds told apart from those of the revision before.
;;;; Streams are decoded here too (Flate, LZW, ASCII hex and 85, run length,
;;;; with their predictors), each held to a size; the data of pictures is
;;;; never decoded. Nothing in the file is run, and the file is never
;;;; written: whatever it holds that does not make sense is noted, never an
;;;; error.

(in-package #:palimpsest)

(define-package-setting *palimpsest-largest-file* (* 512 1024 1024)
  "The most octets a PDF may have to be examined: 512 MB as standard. The file
is read whole, into memory, once.")

(define-package-setting *palimpsest-largest-stream* (* 64 1024 1024)
  "The most octets one stream of a PDF (a page's content, an object stream, a
cross-reference stream) may decode to: 64 MB as standard. A stream that would
decode to more is read no further, and the report says so.")

(define-package-setting *palimpsest-most-objects* 1000000
  "The most objects of a PDF read, and the most entries one cross-reference
section may list: a million as standard. Past them the rest is passed over,
and the report says so.")

(define-package-setting *palimpsest-most-revisions* 500
  "The most saves (revisions) of a PDF taken apart: 500 as standard. A file
saved more often has the rest listed as not read.")

(defparameter *decoded-cache-limit* (* 256 1024 1024)
  "The most octets of decoded streams kept while a PDF is examined, so that a
stream shared by many revisions is decoded once.")

;;; Octets

(deftype octets () "A vector of octets." '(simple-array (unsigned-byte 8) (*)))

(defun bytes (text)
  "TEXT, a string of ASCII, as octets."
  (map 'octets #'char-code text))

(defun white-p (c)
  "True when octet C is white space in a PDF."
  (member c '(0 9 10 12 13 32)))

(defun digit-p (c)
  "True when octet C is a decimal digit."
  (<= 48 c 57))

(defun integer-at (octets i &optional (end (length octets)))
  "The unsigned decimal integer in OCTETS at I (white space before it passed
over), and where it ends; NIL when there is none, or it has more than 15
digits."
  (let ((i (loop while (and (< i end) (white-p (aref octets i))) do (incf i) finally (return i))))
    (when (and (< i end) (digit-p (aref octets i)))
      (let ((value 0) (j i))
        (loop while (and (< j end) (digit-p (aref octets j)) (< (- j i) 16))
              do (setf value (+ (* value 10) (- (aref octets j) 48))) (incf j))
        (and (< (- j i) 16) (values value j))))))

(defun line-end-after (octets i)
  "Where the line end at I in OCTETS (CR LF, LF or CR) ends; I when there is none."
  (let ((n (length octets)))
    (cond ((and (< (1+ i) n) (= (aref octets i) 13) (= (aref octets (1+ i)) 10)) (+ i 2))
          ((and (< i n) (member (aref octets i) '(10 13))) (1+ i))
          (t i))))

(defun hex-offset (n)
  "N, an offset into a file, in hex as the report writes it: 0x0001A2F0."
  (format nil "0x~8,'0x" n))

;;; PDF values, as Tuatara's reader gives them

(defun ref-p (value)
  "True when VALUE is a reference to an object, (:REF NUMBER GENERATION)."
  (and (consp value) (eq (first value) :ref) (integerp (second value))))

(defun dict-p (value)
  "True when VALUE is a dictionary: a list of (NAME . VALUE), each NAME
beginning with a /."
  (and (consp value) (not (eq (first value) :ref))
       (every (lambda (pair) (and (consp pair) (stringp (car pair)) (plusp (length (car pair))) (char= #\/ (char (car pair) 0))))
              value)))

(defun dget (dict key)
  "The value of KEY (a name with its /) in DICT, or NIL."
  (and (dict-p dict) (cdr (assoc key dict :test #'string=))))

(defun name-p (value name)
  "True when VALUE is the name NAME (written with its /)."
  (and (stringp value) (string= value name)))

(defun listed (value)
  "VALUE as a list: itself when it is an array, else a list of it alone; NIL for NIL."
  (cond ((null value) nil)
        ((and (listp value) (not (ref-p value)) (not (dict-p value))) value)
        (t (list value))))

;;; The file as found by scanning it

(defstruct (pobj (:copier nil))
  "An object of a PDF: NUMBER and GENERATION; VALUE as Tuatara's PDF reader
gives it; AT, where it begins (its number), END where it ends (after
endobj); STREAM-START and STREAM-END, its stream's octets in the file, or
NIL; LENGTH, the length its dictionary gives its stream; PACKED-IN, the
number of the object stream holding it (AT then counting in that stream's
decoded octets), else NIL."
  number generation value at end stream-start stream-end length packed-in)

(setf (documentation 'pobj-p 'function) "True when OBJECT is a POBJ.")

(defstruct (entry (:constructor make-entry (kind place generation &optional index)) (:copier nil))
  "What a cross-reference section says of an object: KIND :used (at offset
PLACE), :packed (in the object stream numbered PLACE, at INDEX) or :free;
GENERATION its generation number."
  kind place generation index)

(setf (documentation 'entry-p 'function) "True when OBJECT is an ENTRY.")

(defstruct (revision (:copier nil))
  "A save of a PDF: INDEX from 0 (the original); START and END, its octets in
the file (END just past its %%EOF line); EOF, where its %%EOF is; XREF-AT,
where its cross-reference section is, XREF-KIND :table, :stream, :hybrid or
:none; TRAILER, its trailer dictionary; PREV, where its trailer says the
section before is; ENTRIES, what its section says of each object number (a
hash table); PROBLEMS, what does not make sense in it, in words; ADDED,
CHANGED, SAME and FREED, the object numbers it adds, writes with a new body,
writes again unchanged and frees; PAGES, its pages (PAGE), read on asking;
ROLES, what each object is to the document as it then stood (read on
asking); LINEARISED, true for the first save of a file prepared for the web;
STARTXREF-AT, where its startxref keyword is, and STARTXREF, the offset it
names; TRAILER-AT, where its trailer is (its trailer keyword, or its
cross-reference stream's object)."
  index start end eof xref-at (xref-kind :none) trailer prev startxref-at startxref trailer-at (entries (make-hash-table)) (problems '())
  (added '()) (changed '()) (same '()) (freed '()) (pages :unread) (roles nil) linearised)

(setf (documentation 'revision-p 'function) "True when OBJECT is a REVISION.")

(defstruct (pdf (:copier nil))
  "A PDF read for examining: OCTETS, the whole file; PATH and NAME; HEADER,
where %PDF- begins, VERSION as it says; FOUND, the objects its body holds
as scanning finds them (a vector of POBJ, in order); STREAMS, where each
stream found lies, for telling an %%EOF in one from a real one; REVISIONS,
a vector of REVISION, the original first; OBJECTS, the objects read by
offset; DECODED, streams decoded; ENCRYPTED, true when its trailer names
an encryption; NOTES, what could not be read, in words; CACHE, what is
worked out once while it is examined (fonts, what pages draw), a property
list."
  octets path name (header 0) version (found #()) (revisions #()) (cache '())
  (objects (make-hash-table)) (decoded (make-hash-table :test 'eq)) (decoded-size 0)
  (packed (make-hash-table :test 'eq)) encrypted (notes '()))

(setf (documentation 'pdf-p 'function) "True when OBJECT is a PDF.")

(defvar *depth* 0
  "How deep references are being followed while an object is read: held
below 64, so that a file whose objects refer to one another in a circle
is read no further.")

(defun note (pdf format &rest arguments)
  "Note in PDF what could not be read of it (FORMAT with ARGUMENTS), once."
  (let ((words (apply #'format nil format arguments)))
    (pushnew words (pdf-notes pdf) :test #'string=)))

(defun object-header-before (octets p)
  "Where the object whose obj keyword is at P in OCTETS begins (the first
digit of its number), its number and generation; NIL when no NUMBER
GENERATION stands before P."
  (let ((i (1- p)))
    (flet ((back-white () (let ((from i)) (loop while (and (>= i 0) (white-p (aref octets i))) do (decf i)) (< i from)))
           (back-digits () (let ((to i)) (loop while (and (>= i 0) (digit-p (aref octets i)) (< (- to i) 12)) do (decf i)) (< i to))))
      (when (and (back-white) (back-digits))
        (let ((generation-start (1+ i)))
          (when (and (back-white) (back-digits))
            (let ((start (1+ i)))
              (when (or (< i 0) (white-p (aref octets i)) (pdf-delimiter-p (aref octets i)))
                (values start (integer-at octets start) (integer-at octets generation-start))))))))))

(defun object-at (octets at)
  "The object written in OCTETS from AT (its number, NUMBER GENERATION obj),
as a POBJ with where its stream lies; NIL when none begins there. A stream's
length is the one its dictionary gives when endstream follows it there,
else as far as endstream."
  (let ((n (length octets)))
    (multiple-value-bind (number after) (integer-at octets at)
      (multiple-value-bind (generation after) (and number (integer-at octets after))
        (let ((obj (and generation (pdf-skip-space octets after))))
          (when (and obj (octets-at-p octets "obj" obj)
                     (or (>= (+ obj 3) n) (pdf-delimiter-p (aref octets (+ obj 3)))))
            (multiple-value-bind (value value-end) (pdf-object-at octets (+ obj 3))
              (let* ((next (pdf-skip-space octets value-end))
                     (object (make-pobj :number number :generation generation :value value :at at :end value-end)))
                (when (and (octets-at-p octets "stream" next)
                           (or (>= (+ next 6) n) (member (aref octets (+ next 6)) '(10 13 32))))
                  (let* ((data (let ((e (+ next 6)))
                                 (cond ((and (< (1+ e) n) (= (aref octets e) 13) (= (aref octets (1+ e)) 10)) (+ e 2))
                                       ((and (< e n) (member (aref octets e) '(10 13))) (1+ e))
                                       (t e))))
                         (length (dget value "/Length"))
                         (exact (and (integerp length) (<= 0 length) (<= (+ data length) n)
                                     (octets-at-p octets "endstream" (pdf-skip-space octets (+ data length)))
                                     (+ data length)))
                         (close (or (and exact (pdf-skip-space octets exact))
                                    (search (load-time-value (bytes "endstream")) octets :start2 data)))
                         (data-end (or exact
                                       (and close (let ((e close))
                                                    (when (and (> e data) (= (aref octets (1- e)) 10)) (decf e))
                                                    (when (and (> e data) (= (aref octets (1- e)) 13)) (decf e))
                                                    e))
                                       n)))
                    (setf (pobj-stream-start object) data (pobj-stream-end object) data-end
                          (pobj-length object) length
                          next (if close (pdf-skip-space octets (+ close 9)) n))))
                (setf (pobj-end object)
                      (if (octets-at-p octets "endobj" next) (+ next 6) (max next (pobj-end object))))
                object))))))))

(defun scan-body (pdf &optional (progress (constantly nil)))
  "Find every object written in PDF's body, in the order written, with the
places of their streams (PDF-FOUND, PDF-STREAMS); PROGRESS is called with
the offset reached now and then."
  (let* ((octets (pdf-octets pdf)) (n (length octets)) (found '()) (count 0) (i 0)
         (mark (load-time-value (bytes "obj"))))
    (loop
      (let ((p (search mark octets :start2 i)))
        (unless p (return))
        (when (zerop (mod (incf count) 4096)) (funcall progress p) (job-check-stop))
        (let ((at (object-header-before octets p))
              (next (+ p 3)))
          (when (and at (or (>= (+ p 3) n) (pdf-delimiter-p (aref octets (+ p 3)))))
            (let ((object (ignore-errors (object-at octets at))))
              (when object
                (push object found)
                (setf (gethash at (pdf-objects pdf)) object)
                (setf next (max next (pobj-end object)))
                (when (> (length found) *palimpsest-most-objects*)
                  (note pdf "It holds more than ~:d objects (*palimpsest-most-objects*): the rest were not read" *palimpsest-most-objects*)
                  (return)))))
          (setf i next))))
    (setf (pdf-found pdf) (coerce (nreverse found) 'vector))))

(defun inside-stream-p (pdf at)
  "True when AT, an offset of PDF's file, falls within the octets of a stream found by scanning."
  (let* ((found (pdf-found pdf)) (low 0) (high (length found)))
    (loop while (< low high)
          do (let ((middle (floor (+ low high) 2)))
               (if (<= (pobj-at (aref found middle)) at) (setf low (1+ middle)) (setf high middle))))
    (let ((object (and (plusp low) (aref found (1- low)))))
      (and object (pobj-stream-start object) (<= (pobj-stream-start object) at) (< at (pobj-stream-end object))))))

;;; Streams decoded

(defun filters-of (dict)
  "The filters DICT, a stream's dictionary, names, each (NAME . PARAMETERS),
in the order they are undone."
  (let ((filters (listed (dget dict "/Filter")))
        (parameters (let ((p (dget dict "/DecodeParms"))) (if (dict-p p) (list p) (listed p)))))
    (loop for filter in filters for k from 0
          collect (cons filter (let ((p (nth k parameters))) (and (dict-p p) p))))))

(defun predicted (data parameters)
  "DATA undone of the predictor PARAMETERS (a filter's /DecodeParms) name:
PNG's (10 to 15) row by row, or TIFF's (2); DATA itself for none."
  (let ((predictor (or (dget parameters "/Predictor") 1)))
    (if (not (and (integerp predictor) (>= predictor 2)))
        data
        (let* ((colours (let ((c (dget parameters "/Colors"))) (if (and (integerp c) (<= 1 c 32)) c 1)))
               (bits (let ((b (dget parameters "/BitsPerComponent"))) (if (member b '(1 2 4 8 16)) b 8)))
               (columns (let ((c (dget parameters "/Columns"))) (if (and (integerp c) (<= 1 c 1000000)) c 1)))
               (pixel (max 1 (ceiling (* colours bits) 8)))
               (row (ceiling (* colours bits columns) 8)))
          (if (= predictor 2)
              (let ((out (copy-seq data)))
                (when (= bits 8)
                  (loop for start from 0 below (length out) by row
                        do (loop for k from (+ start pixel) below (min (length out) (+ start row))
                                 do (setf (aref out k) (logand 255 (+ (aref out k) (aref out (- k pixel))))))))
                out)
              (let* ((rows (floor (length data) (1+ row)))
                     (out (make-array (* rows row) :element-type '(unsigned-byte 8) :initial-element 0)))
                (dotimes (r rows out)
                  (let ((kind (aref data (* r (1+ row))))
                        (in (1+ (* r (1+ row))))
                        (at (* r row)))
                    (dotimes (k row)
                      (let* ((x (aref data (+ in k)))
                             (left (if (>= k pixel) (aref out (+ at k (- pixel))) 0))
                             (up (if (plusp r) (aref out (+ at k (- row))) 0))
                             (corner (if (and (plusp r) (>= k pixel)) (aref out (+ at k (- row) (- pixel))) 0)))
                        (setf (aref out (+ at k))
                              (logand 255 (+ x (case kind
                                                 (1 left) (2 up) (3 (floor (+ left up) 2))
                                                 (4 (let* ((p (- (+ left up) corner)) (pa (abs (- p left))) (pb (abs (- p up))) (pc (abs (- p corner))))
                                                      (cond ((and (<= pa pb) (<= pa pc)) left) ((<= pb pc) up) (t corner))))
                                                 (t 0)))))))))))))))

(defun grow (out octet limit)
  "Put OCTET at the end of OUT, an adjustable vector of octets; an error past LIMIT octets."
  (when (>= (fill-pointer out) limit)
    (error "it decodes to more than ~:d octets (*palimpsest-largest-stream*)" limit))
  (vector-push-extend octet out))

(defun new-out (size limit)
  "An adjustable vector of octets, room for about SIZE of them, at most LIMIT."
  (make-array (max 64 (min limit size)) :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun ascii-hex-decoded (data limit)
  "DATA, ASCII hex digits ending at >, as the octets they stand for."
  (let ((out (new-out (floor (length data) 2) limit)) (high nil))
    (loop for c across data
          do (cond ((= c 62) (return))
                   ((digit-char-p (code-char c) 16)
                    (let ((v (digit-char-p (code-char c) 16)))
                      (if high (progn (grow out (+ (* 16 high) v) limit) (setf high nil)) (setf high v))))))
    (when high (grow out (* 16 high) limit))
    (coerce out 'octets)))

(defun ascii-85-decoded (data limit)
  "DATA, ASCII base-85 ending at ~>, as the octets it stands for."
  (let ((out (new-out (length data) limit)) (group '()))
    (flet ((flush (count)
             (let ((value 0))
               (dolist (d (reverse group)) (setf value (+ (* value 85) d)))
               (loop repeat (- 5 count) do (setf value (+ (* value 85) 84)))
               (loop for k from 3 downto (- 4 (1- count)) do (grow out (ldb (byte 8 (* 8 k)) value) limit)))))
      (loop for c across data
            do (cond ((= c 126) (return))
                     ((and (= c 122) (null group)) (dotimes (k 4) (grow out 0 limit)))
                     ((<= 33 c 117)
                      (push (- c 33) group)
                      (when (= (length group) 5) (flush 5) (setf group '())))))
      (when (> (length group) 1) (flush (length group))))
    (coerce out 'octets)))

(defun lzw-decoded (data limit early)
  "DATA, LZW-coded as PDF does it (EARLY 1 for the code length to grow one code
early, as standard), as the octets it stands for."
  (let ((out (new-out (* 3 (length data)) limit))
        (table (make-array 4096 :initial-element nil))
        (next 258) (width 9) (previous nil) (bit-buffer 0) (bit-count 0) (at 0))
    (dotimes (k 256) (setf (aref table k) (make-array 1 :element-type '(unsigned-byte 8) :initial-element k)))
    (loop
      (loop while (and (< bit-count width) (< at (length data)))
            do (setf bit-buffer (logior (ash bit-buffer 8) (aref data at)) bit-count (+ bit-count 8)) (incf at))
      (when (< bit-count width) (return))
      (let ((code (ldb (byte width (- bit-count width)) bit-buffer)))
        (decf bit-count width)
        (setf bit-buffer (ldb (byte bit-count 0) bit-buffer))
        (cond ((= code 256) (setf next 258 width 9 previous nil))
              ((= code 257) (return))
              (t (let ((entry (cond ((aref table code))
                                    ((and previous (= code next)) (concatenate 'octets previous (subseq previous 0 1)))
                                    (t (return)))))
                   (loop for o across entry do (grow out o limit))
                   (when (and previous (< next 4096))
                     (setf (aref table next) (concatenate 'octets previous (subseq entry 0 1)))
                     (incf next))
                   (setf previous entry)
                   (setf width (cond ((>= (+ next early) 2048) 12) ((>= (+ next early) 1024) 11) ((>= (+ next early) 512) 10) (t 9))))))))
    (coerce out 'octets)))

(defun run-length-decoded (data limit)
  "DATA, run-length coded, as the octets it stands for."
  (let ((out (new-out (* 2 (length data)) limit)) (at 0) (n (length data)))
    (loop while (< at n)
          do (let ((length (aref data at)))
               (incf at)
               (cond ((= length 128) (return))
                     ((< length 128) (loop repeat (1+ length) while (< at n) do (grow out (aref data at) limit) (incf at)))
                     (t (when (< at n) (loop repeat (- 257 length) do (grow out (aref data at) limit)) (incf at))))))
    (coerce out 'octets)))

(defparameter *picture-filters*
  '("/DCTDecode" "/DCT" "/JPXDecode" "/JBIG2Decode" "/CCITTFaxDecode" "/CCF")
  "The filters of picture data, never decoded here: a stream ends its decoding at one.")

(defun decode-filters (data filters limit)
  "DATA undone of FILTERS (FILTERS-OF), at most LIMIT octets, as two values:
the octets, and NIL when every filter was undone, the name of a picture's
filter it stopped at, or why it could not be decoded, in words."
  (dolist (filter filters (values data nil))
    (destructuring-bind (name . parameters) filter
      (when (member name *picture-filters* :test #'equal)
        (return (values data name)))
      (handler-case
          (setf data
                (cond ((member name '("/FlateDecode" "/Fl") :test #'equal)
                       (let ((start (if (and (>= (length data) 2) (= 8 (logand 15 (aref data 0)))
                                             (zerop (mod (+ (* 256 (aref data 0)) (aref data 1)) 31)))
                                        2 0)))
                         (predicted (inflate data :start start :limit limit) parameters)))
                      ((member name '("/LZWDecode" "/LZW") :test #'equal)
                       (predicted (lzw-decoded data limit (let ((e (dget parameters "/EarlyChange"))) (if (eql e 0) 0 1))) parameters))
                      ((member name '("/ASCIIHexDecode" "/AHx") :test #'equal) (ascii-hex-decoded data limit))
                      ((member name '("/ASCII85Decode" "/A85") :test #'equal) (ascii-85-decoded data limit))
                      ((member name '("/RunLengthDecode" "/RL") :test #'equal) (run-length-decoded data limit))
                      ((and (equal name "/Crypt") (member (dget parameters "/Name") '(nil "/Identity") :test #'equal)) data)
                      (t (return (values data (format nil "its filter ~a is not one read here" name))))))
        (error (e) (return (values nil (format nil "it could not be decoded (~a): ~a" name e))))))))

(defun stream-data (pdf object &key (limit *palimpsest-largest-stream*))
  "The decoded octets of OBJECT's stream (a POBJ), as two values: the octets
\(the picture data itself for a picture, undecoded), and NIL, the name of
the picture's filter, or why it could not be decoded, in words. NIL for an
object with no stream. Kept, so that a stream is decoded once."
  (when (pobj-stream-start object)
    (let ((kept (gethash object (pdf-decoded pdf))))
      (if kept
          (values (car kept) (cdr kept))
          (let ((raw (subseq (pdf-octets pdf) (pobj-stream-start object) (pobj-stream-end object)))
                (dict (pobj-value object)))
            (multiple-value-bind (data why)
                (cond ((dget dict "/F") (values nil "its data is kept in another file, which is not read"))
                      ((and (pdf-encrypted pdf) (not (name-p (dget dict "/Type") "/XRef")))
                       (values raw "it is encrypted"))
                      (t (decode-filters raw (filters-of dict) limit)))
              (when (and data (< (+ (pdf-decoded-size pdf) (length data)) *decoded-cache-limit*))
                (incf (pdf-decoded-size pdf) (length data))
                (setf (gethash object (pdf-decoded pdf)) (cons data why)))
              (values data why)))))))

(defun raw-stream (pdf object)
  "The octets of OBJECT's stream as the file holds them, not decoded; NIL for one with none."
  (and (pobj-stream-start object) (subseq (pdf-octets pdf) (pobj-stream-start object) (pobj-stream-end object))))

;;; Objects

(defun object-at-offset (pdf at)
  "The object PDF's file holds at AT (a POBJ), read once; NIL when none begins there."
  (when (and (integerp at) (<= 0 at) (< at (length (pdf-octets pdf))))
    (multiple-value-bind (kept there) (gethash at (pdf-objects pdf))
      (if there
          kept
          (setf (gethash at (pdf-objects pdf)) (ignore-errors (object-at (pdf-octets pdf) at)))))))

(defun packed-objects (pdf stream)
  "What the object stream STREAM (a POBJ) holds, as two values: a vector of
\(NUMBER . OFFSET), each object's number and where it is in the decoded
octets, and those octets; NIL when it cannot be read."
  (let ((kept (gethash stream (pdf-packed pdf))))
    (if kept
        (values (car kept) (cdr kept))
        (let* ((dict (pobj-value stream))
               (count (dget dict "/N")) (first (dget dict "/First"))
               (data (and (integerp count) (integerp first) (<= 0 count) (stream-data pdf stream))))
          (when (and data (<= 0 first (length data)))
            (let ((pairs (make-array 0 :adjustable t :fill-pointer 0)) (i 0))
              (loop repeat (min count 100000)
                    do (multiple-value-bind (number after) (integer-at data i first)
                         (multiple-value-bind (offset after) (and number (integer-at data after first))
                           (unless offset (return))
                           (vector-push-extend (cons number (+ first offset)) pairs)
                           (setf i after))))
              (setf (gethash stream (pdf-packed pdf)) (cons pairs data))
              (values pairs data)))))))

(defun packed-object (pdf stream index number)
  "The object NUMBER the object stream STREAM (a POBJ) holds, at INDEX
among them or, when another is there, wherever it is; a POBJ, or NIL."
  (multiple-value-bind (pairs data) (packed-objects pdf stream)
    (when pairs
      (let ((pair (or (and (integerp index) (< -1 index (length pairs)) (= number (car (aref pairs index))) (aref pairs index))
                      (find number pairs :key #'car))))
        (when (and pair (< (cdr pair) (length data)))
          (multiple-value-bind (value end) (pdf-object-at data (cdr pair))
            (make-pobj :number number :generation 0 :value value :at (cdr pair) :end end
                       :packed-in (pobj-number stream))))))))

(defun lookup (pdf revision number)
  "What the cross-reference sections of REVISION and those before say of
the object NUMBER, the latest first (an ENTRY), and the REVISION saying it;
NIL when none does."
  (loop for r from (revision-index revision) downto 0
        for entry = (gethash number (revision-entries (aref (pdf-revisions pdf) r)))
        when entry return (values entry (aref (pdf-revisions pdf) r))))

(defun found-object (pdf number limit)
  "The last object NUMBER written in PDF's body before the offset LIMIT, as scanning found it; NIL for none."
  (let ((index (or (getf (pdf-cache pdf) :by-number)
                   (let ((table (make-hash-table)))
                     (loop for object across (pdf-found pdf) do (push object (gethash (pobj-number object) table)))
                     (setf (getf (pdf-cache pdf) :by-number) table)))))
    ;; each number's objects, the last written first
    (find-if (lambda (object) (< (pobj-at object) limit)) (gethash number index))))

(defun revision-object (pdf revision number)
  "The object NUMBER as REVISION has it (a POBJ): where its cross-reference
says, else, when that holds no such object, the last of that number written
before REVISION ends; NIL when it has none, or it is free."
  (when (and (integerp number) (< *depth* 64))
    (let ((*depth* (1+ *depth*)))
      (multiple-value-bind (entry) (lookup pdf revision number)
        (case (and entry (entry-kind entry))
          (:used (let ((object (object-at-offset pdf (entry-place entry))))
                   (if (and object (eql (pobj-number object) number))
                       object
                       (found-object pdf number (revision-end revision)))))
          (:packed (let ((stream (revision-object pdf revision (entry-place entry))))
                     (and stream (null (pobj-packed-in stream))
                          (packed-object pdf stream (entry-index entry) number))))
          (:free nil)
          (t (and (eq (revision-xref-kind revision) :none) (found-object pdf number (revision-end revision)))))))))

(defun deref (pdf revision value)
  "VALUE with a reference followed, in REVISION, to the value of what it names."
  (if (ref-p value)
      (let ((object (revision-object pdf revision (second value))))
        (and object (pobj-value object)))
      value))

(defun dref (pdf revision dict key)
  "The value of KEY in DICT, a reference followed in REVISION."
  (deref pdf revision (dget dict key)))

(defun ref-number (value)
  "The object number VALUE refers to, or NIL when it is no reference."
  (and (ref-p value) (second value)))

;;; Cross-reference sections

(defun read-xref-table (pdf at)
  "The cross-reference table of PDF at AT (where xref is), as three values:
its entries (a hash table of number to ENTRY), its trailer, and what does not
make sense in it, in words; NIL when there is no table at AT."
  (let* ((octets (pdf-octets pdf)) (n (length octets)) (entries (make-hash-table)) (problems '()) (total 0)
         (i (pdf-skip-space octets at)))
    (when (octets-at-p octets "xref" i)
      (setf i (+ i 4))
      (loop
        (setf i (pdf-skip-space octets i))
        (when (or (>= i n) (octets-at-p octets "trailer" i)) (return))
        (multiple-value-bind (start after) (integer-at octets i)
          (multiple-value-bind (count after) (and start (integer-at octets after))
            (unless count (push (format nil "its cross-reference table at ~a is cut short" (hex-offset at)) problems) (return))
            (setf i after)
            (dotimes (k count)
              (multiple-value-bind (offset after) (integer-at octets i)
                (multiple-value-bind (generation after) (and offset (integer-at octets after))
                  (let ((kind (and generation (pdf-skip-space octets after))))
                    (unless (and kind (< kind n) (member (aref octets kind) '(102 110)))
                      (push (format nil "an entry of its cross-reference table at ~a cannot be read" (hex-offset i)) problems)
                      (return))
                    (when (> (incf total) *palimpsest-most-objects*)
                      (push "its cross-reference table lists more entries than are read (*palimpsest-most-objects*)" problems)
                      (return))
                    (let ((number (+ start k)))
                      (if (gethash number entries)
                          (push (format nil "object ~d is listed twice in its cross-reference table" number) problems)
                          (setf (gethash number entries)
                                (make-entry (if (= (aref octets kind) 110) :used :free) offset generation))))
                    (setf i (1+ kind)))))))))
      (let ((trailer (and (octets-at-p octets "trailer" i) (pdf-object-at octets (+ i 7)))))
        (values entries (and (dict-p trailer) trailer) (nreverse problems))))))

(defun read-xref-stream (pdf object)
  "The entries of OBJECT, a cross-reference stream (a POBJ), as a hash table
of number to ENTRY, and what does not make sense in it, in words, as two
values; NIL when it is no such stream or cannot be decoded."
  (let ((dict (pobj-value object)))
    (when (name-p (dget dict "/Type") "/XRef")
      (multiple-value-bind (data why) (stream-data pdf object)
        (if (or (null data) why)
            (values nil (list (format nil "its cross-reference stream, object ~d, ~a" (pobj-number object) (or why "is empty"))))
            (let* ((widths (dget dict "/W"))
                   (size (dget dict "/Size"))
                   (index (or (dget dict "/Index") (and (integerp size) (list 0 size))))
                   (entries (make-hash-table)) (problems '()) (at 0) (total 0))
              (if (not (and (= (length widths) 3) (every (lambda (w) (and (integerp w) (<= 0 w 8))) widths)
                            (plusp (reduce #'+ widths))))
                  (values nil (list (format nil "its cross-reference stream, object ~d, gives no field widths it can be read by" (pobj-number object))))
                  (let ((row (reduce #'+ widths)))
                    (flet ((field (k standard)
                             (let ((w (nth k widths)))
                               (if (zerop w)
                                   standard
                                   (prog1 (loop with v = 0 for j from at below (+ at w) do (setf v (+ (* v 256) (aref data j))) finally (return v))
                                     (incf at w))))))
                      (loop for (start count) on index by #'cddr
                            while (and (integerp start) (integerp count))
                            do (dotimes (k count)
                                 (when (> (+ at row) (length data))
                                   (push (format nil "its cross-reference stream, object ~d, is shorter than its index says" (pobj-number object)) problems)
                                   (return))
                                 (when (> (incf total) *palimpsest-most-objects*)
                                   (push "its cross-reference stream lists more entries than are read (*palimpsest-most-objects*)" problems)
                                   (return))
                                 (let* ((type (field 0 1)) (second (field 1 0)) (third (field 2 0)) (number (+ start k)))
                                   (unless (gethash number entries)
                                     (setf (gethash number entries)
                                           (case type
                                             (0 (make-entry :free second third))
                                             (1 (make-entry :used second third))
                                             (2 (make-entry :packed second 0 third))
                                             (t (make-entry :free 0 0)))))))))
                    (values entries (nreverse problems))))))))))

(defun read-section (pdf at)
  "The cross-reference section of PDF at AT, as five values: its entries,
its trailer, its kind (:table, :stream, :hybrid), where it really is and
what does not make sense in it; NIL when there is none at AT, nor near it."
  (let ((octets (pdf-octets pdf)))
    (flet ((try (place)
             (multiple-value-bind (entries trailer problems) (read-xref-table pdf place)
               (if entries
                   (let ((hidden (dget trailer "/XRefStm")))
                     (when (integerp hidden)
                       (let ((object (object-at-offset pdf hidden)))
                         (multiple-value-bind (more more-problems) (and object (read-xref-stream pdf object))
                           (setf problems (append problems more-problems))
                           (when more
                             (maphash (lambda (number entry)
                                        (let ((old (gethash number entries)))
                                          (when (or (null old) (eq (entry-kind old) :free))
                                            (setf (gethash number entries) entry))))
                                      more)))))
                     (return-from read-section (values entries trailer (if (integerp hidden) :hybrid :table) place problems)))
                   (let ((object (object-at-offset pdf place)))
                     (when (and object (name-p (dget (pobj-value object) "/Type") "/XRef"))
                       (multiple-value-bind (entries problems) (read-xref-stream pdf object)
                         (return-from read-section
                           (values (or entries (make-hash-table)) (pobj-value object) :stream place problems)))))))))
      (when (and (integerp at) (< -1 at (length octets)))
        (try at)
        ;; a section a little before or after where it is said to be: found, and said
        (loop for delta from 1 to 64
              do (dolist (place (list (- at delta) (+ at delta)))
                   (when (and (< -1 place (length octets))
                              (or (octets-at-p octets "xref" place)
                                  (and (digit-p (aref octets place)) (or (zerop place) (not (digit-p (aref octets (1- place))))))))
                     (try place))))))))

;;; Revisions

(defun eof-ends (pdf)
  "The %%EOF markers of PDF's file that end a save (none within a stream), each
\(EOF . END): where it is, and where its line ends."
  (let ((octets (pdf-octets pdf)) (ends '()))
    (loop for at = (search (load-time-value (bytes "%%EOF")) octets) then (search (load-time-value (bytes "%%EOF")) octets :start2 (+ at 5))
          while at
          unless (inside-stream-p pdf at)
            do (push (cons at (line-end-after octets (+ at 5))) ends))
    (nreverse ends)))

(defun startxref-before (octets from to)
  "The offset the last startxref between FROM and TO in OCTETS names, or NIL."
  (let ((at (search (load-time-value (bytes "startxref")) octets :start2 from :end2 to :from-end t)))
    (and at (integer-at octets (+ at 9) to))))

(defun linearisation (pdf)
  "The linearisation dictionary of PDF (a file prepared for the web), or NIL:
its first object, when that says /Linearized."
  (let ((first (find-if (lambda (o) (> (pobj-at o) (pdf-header pdf))) (pdf-found pdf))))
    (and first (dget (pobj-value first) "/Linearized") (pobj-value first))))

(defun section-revision (pdf index start eof end)
  "The REVISION of PDF ending with the %%EOF at EOF (its line ending at END),
from START, read by the cross-reference section its startxref names."
  (let* ((octets (pdf-octets pdf))
         (revision (make-revision :index index :start start :end end :eof eof))
         (startxref-at (search (load-time-value (bytes "startxref")) octets :start2 start :end2 eof :from-end t))
         (named (startxref-before octets start eof)))
    (multiple-value-bind (entries trailer kind place problems) (and named (read-section pdf named))
      (when (and entries named (/= place named))
        (push (format nil "its startxref names ~a, but its cross-reference section is at ~a" (hex-offset named) (hex-offset place)) problems))
      (unless entries
        ;; none where it is said to be: the last one written in the save, else none at all
        (let ((table (search (load-time-value (bytes "xref")) octets :start2 start :end2 eof :from-end t)))
          (when (and table (not (and (> table 0) (= (aref octets (1- table)) 116)))) ; not startxref
            (multiple-value-setq (entries trailer kind place problems) (read-section pdf table))))
        (unless entries
          (let ((stream (find-if (lambda (o) (and (<= start (pobj-at o) eof) (name-p (dget (pobj-value o) "/Type") "/XRef")))
                                 (pdf-found pdf) :from-end t)))
            (when stream
              (multiple-value-setq (entries trailer kind place problems) (read-section pdf (pobj-at stream))))))
        (push (cond (entries (format nil "~:[it has no startxref~;~:*its startxref names ~a, where no cross-reference section is~]; the one at ~a was read"
                                     (and named (hex-offset named)) (hex-offset place)))
                    (t (format nil "~:[it has no startxref~;~:*its startxref names ~a, where no cross-reference section is~], and no section was found: its objects were found by scanning"
                               (and named (hex-offset named)))))
              problems))
      (unless trailer
        (let ((at (search (load-time-value (bytes "trailer")) octets :start2 start :end2 eof :from-end t)))
          (when at (setf trailer (let ((d (pdf-object-at octets (+ at 7)))) (and (dict-p d) d))))))
      (setf (revision-xref-at revision) place
            (revision-startxref-at revision) startxref-at
            (revision-startxref revision) named
            (revision-trailer-at revision) (case kind
                                             (:stream place)
                                             ((:table :hybrid) (search (load-time-value (bytes "trailer")) octets :start2 place :end2 eof))
                                             (t (search (load-time-value (bytes "trailer")) octets :start2 start :end2 eof :from-end t)))
            (revision-xref-kind revision) (or kind :none)
            (revision-trailer revision) trailer
            (revision-prev revision) (let ((p (dget trailer "/Prev"))) (and (integerp p) p))
            (revision-problems revision) problems)
      (if entries
          (setf (revision-entries revision) entries)
          ;; no section: each object written in the save, the last of each number
          (loop for object across (pdf-found pdf)
                when (and (<= start (pobj-at object)) (< (pobj-at object) end))
                  do (setf (gethash (pobj-number object) (revision-entries revision))
                           (make-entry :used (pobj-at object) (pobj-generation object)))))
      revision)))

(defun merge-revisions (first second)
  "FIRST and SECOND, the two sections of a file prepared for the web, as one
revision: the octets of both, the entries of both (FIRST's first), FIRST's
trailer with what SECOND's adds."
  (maphash (lambda (number entry)
             (unless (gethash number (revision-entries first))
               (setf (gethash number (revision-entries first)) entry)))
           (revision-entries second))
  (setf (revision-end first) (revision-end second)
        (revision-eof first) (revision-eof second)
        (revision-linearised first) t
        (revision-trailer first) (append (revision-trailer first)
                                         (remove-if (lambda (pair) (assoc (car pair) (revision-trailer first) :test #'string=))
                                                    (revision-trailer second)))
        (revision-problems first) (append (revision-problems first) (revision-problems second)))
  first)

(defun object-octets (pdf object)
  "The octets OBJECT (a POBJ) is written as, in the file or its object stream, for telling whether two are alike."
  (if (pobj-packed-in object)
      (princ-to-string (pobj-value object))
      (subseq (pdf-octets pdf) (pobj-at object) (min (length (pdf-octets pdf)) (pobj-end object)))))

(defun note-changes (pdf revision)
  "Work out what REVISION adds, changes, writes again unchanged and frees,
from the revision before it; the original adds all it holds."
  (let ((before (and (plusp (revision-index revision)) (aref (pdf-revisions pdf) (1- (revision-index revision)))))
        (added '()) (changed '()) (same '()) (freed '()))
    (maphash (lambda (number entry)
               (unless (zerop number)
                 (let ((old (and before (lookup pdf before number))))
                   (if (eq (entry-kind entry) :free)
                       (when (and old (not (eq (entry-kind old) :free))) (push number freed))
                       (if (or (null old) (eq (entry-kind old) :free))
                           (push number added)
                           (let ((new-object (revision-object pdf revision number))
                                 (old-object (revision-object pdf before number)))
                             (if (and new-object old-object (equalp (object-octets pdf new-object) (object-octets pdf old-object)))
                                 (push number same)
                                 (push number changed))))))))
             (revision-entries revision))
    (setf (revision-added revision) (sort added #'<) (revision-changed revision) (sort changed #'<)
          (revision-same revision) (sort same #'<) (revision-freed revision) (sort freed #'<))))

(defun read-pdf (octets &key path name (progress (constantly nil)))
  "PDF's structure read from OCTETS, the whole file (from PATH, called NAME):
its objects found by scanning, its saves (revisions) from the first, each with
its cross-reference section and what it changes. PROGRESS is called with a
number from 0 to 1 as it goes."
  (let ((pdf (make-pdf :octets octets :path path :name name)))
    (let ((header (search (load-time-value (bytes "%PDF-")) octets :end2 (min (length octets) 1024))))
      (setf (pdf-header pdf) (or header 0)
            (pdf-version pdf) (and header (let ((end (position-if (lambda (c) (or (white-p c) (= c 37))) octets :start (+ header 5))))
                                            (octets-latin-1 octets :start (+ header 5) :end (min (or end (+ header 9)) (+ header 9))))))
      (unless header (note pdf "It does not begin with %PDF- in its first 1,024 octets: it may not be a PDF at all")))
    (let ((*pdf-most-items* *palimpsest-most-objects*))
      (scan-body pdf (lambda (at) (funcall progress (* 0.6 (/ at (max 1 (length octets)))))))
      (let* ((ends (eof-ends pdf))
             (revisions '()) (start 0))
        (when (> (length ends) *palimpsest-most-revisions*)
          (note pdf "It was saved ~:d times; only the first ~:d saves were taken apart (*palimpsest-most-revisions*)"
                (length ends) *palimpsest-most-revisions*)
          (setf ends (subseq ends 0 *palimpsest-most-revisions*)))
        (loop for (eof . end) in ends for k from 0
              do (job-check-stop)
                 (push (section-revision pdf k start eof end) revisions)
                 (setf start end)
                 (funcall progress (+ 0.6 (* 0.2 (/ (1+ k) (length ends))))))
        (setf revisions (nreverse revisions))
        (when (null revisions)
          (note pdf "It has no %%EOF line: it was cut short, or is no PDF; its objects were read as one save")
          (setf revisions (list (section-revision pdf 0 0 (length octets) (length octets)))))
        ;; a file prepared for the web: its first two sections are one save
        (when (and (linearisation pdf) (rest revisions)
                   (eql (revision-prev (first revisions)) (revision-xref-at (second revisions))))
          (setf revisions (cons (merge-revisions (first revisions) (second revisions)) (cddr revisions))))
        (setf (pdf-revisions pdf) (coerce revisions 'vector))
        (loop for revision across (pdf-revisions pdf) for k from 0
              do (setf (revision-index revision) k)))
      (setf (pdf-encrypted pdf) (some (lambda (r) (dget (revision-trailer r) "/Encrypt")) (pdf-revisions pdf)))
      (loop for revision across (pdf-revisions pdf)
            do (job-check-stop)
               (note-changes pdf revision)))
    (funcall progress 0.85)
    pdf))

(defun last-revision (pdf)
  "The latest save of PDF, as it is shown."
  (let ((revisions (pdf-revisions pdf))) (aref revisions (1- (length revisions)))))

(defun revision-of-offset (pdf at)
  "The revision of PDF whose octets hold the offset AT, or NIL."
  (find-if (lambda (r) (and (<= (revision-start r) at) (< at (revision-end r)))) (pdf-revisions pdf)))

(defun written-in (pdf revision number)
  "The revision in which the object NUMBER, as REVISION has it, was written:
the one whose octets hold it, or its object stream; NIL when there is none."
  (let ((object (revision-object pdf revision number)))
    (when object
      (if (pobj-packed-in object)
          (let ((stream (revision-object pdf revision (pobj-packed-in object))))
            (and stream (revision-of-offset pdf (pobj-at stream))))
          (revision-of-offset pdf (pobj-at object))))))

(defun live-numbers (pdf revision)
  "The numbers of the objects REVISION holds, not free, in order."
  (let ((seen (make-hash-table)) (live '()))
    (loop for r from (revision-index revision) downto 0
          do (maphash (lambda (number entry)
                        (unless (or (zerop number) (gethash number seen))
                          (setf (gethash number seen) t)
                          (unless (eq (entry-kind entry) :free) (push number live))))
                      (revision-entries (aref (pdf-revisions pdf) r))))
    (sort live #'<)))

(defun references-in (value)
  "The object numbers VALUE refers to, at any depth."
  (let ((found '()))
    (labels ((walk (v depth)
               (cond ((> depth 40))
                     ((ref-p v) (push (second v) found))
                     ((consp v)
                      ;; along the list, a dictionary's (KEY . VALUE) whose value is a reference among its tails
                      (loop for rest = v then (cdr rest)
                            do (cond ((ref-p rest) (push (second rest) found) (return))
                                     ((consp rest) (walk (car rest) (1+ depth)))
                                     (t (return))))))))
      (walk value 0))
    found))

(defun reachable (pdf revision &key (from nil from-p) skip)
  "The numbers of the objects REVISION's trailer leads to (or the values
FROM leads to), following every reference but those under the keys SKIP
names, as a hash table."
  (let ((seen (make-hash-table)) (queue (references-in (if from-p from (remove "/Prev" (revision-trailer revision) :key #'car :test #'string=)))))
    (loop while queue
          do (let ((number (pop queue)))
               (unless (gethash number seen)
                 (setf (gethash number seen) t)
                 (let ((object (revision-object pdf revision number)))
                   (when object
                     (let ((value (pobj-value object)))
                       (dolist (n (references-in (if (and skip (dict-p value))
                                                     (remove-if (lambda (pair) (member (car pair) skip :test #'string=)) value)
                                                     value)))
                         (unless (gethash n seen) (push n queue)))))))))
    seen))
