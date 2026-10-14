;;;; csv-records/csv-records.lisp --- Records of a CSV file kept or deleted by a pattern, and its columns rearranged as awk prints them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package, not part of the editor: loaded by (load-package "csv-records")
;;;; in the init file.
;;;;
;;;; In a CSV or TSV buffer, C-c r k keeps the records whose column, chosen by
;;;; its header's name or its number, matches a regular expression, C-c r d
;;;; deletes them, and C-c r a rearranges the columns of every record as
;;;; awk's print does, columns counted from 0 as csvdt counts them:
;;;; $0,$1,$3,$4"cent","hello" leaves out the third column, puts cent after
;;;; the fifth's value and adds a column holding hello. A
;;;; record is a line, as CSV mode reads one; the first line, the header, is
;;;; never kept out or deleted. Each is one step to undo, done in the
;;;; background on every core when the file is large, as keep-lines is.

(defpackage #:csv-records
  (:use #:cl #:tuatara)
  (:documentation "Records of a CSV file kept or deleted by a pattern, and its columns rearranged as awk prints them."))

(in-package #:csv-records)

;; made for a Tuatara whose API has these: an older one is refused at once,
;; rather than each command failing on a name it lacks
(let ((missing (remove-if (lambda (name) (eq (nth-value 1 (find-symbol name '#:tuatara)) :external))
                          '("CSV-DELIMITER" "CSV-FIELD-SPANS" "CSV-FIELD-BOUNDS" "CSV-VALUE-BOUNDS" "CSV-FIELD-VALUE" "CSV-HEADER-NAMES"
                            "COMPILE-REGEX" "SELECTED-LINES" "REWRITE-LINES"))))
  (when missing
    (error "csv-records needs a newer Tuatara, whose API has ~{~(~a~)~^, ~}: build it again from its sources" missing)))

;;; The columns asked for

(defun column-index (answer names)
  "The column ANSWER names, counted from 0: a header among NAMES, a number or
$ and a number (0 or $0 the first), or NIL for the whole record (an empty
answer, \"whole record\" or $*); :UNKNOWN for anything else."
  (let ((answer (string-trim " " answer)))
    (cond ((or (string= answer "") (string-equal answer "whole record") (string= answer "$*")) nil)
          ((position answer names :test #'string=))
          ((position answer names :test #'string-equal))
          (t (let ((number (ignore-errors (parse-integer (string-left-trim "$" answer)))))
               (if (and number (>= number 0)) number :unknown))))))

(defun read-column (buffer prompt)
  "Ask with PROMPT for a column of BUFFER, offering its header's names and the
whole record: its index from 0, or NIL for the whole record."
  (let* ((names (csv-header-names buffer))
         (candidates (cons (make-candidate "whole record" :annotation "the line as it is")
                           (loop for name in names
                                 for n from 0
                                 collect (make-candidate name :annotation (format nil "$~d" n)))))
         (answer (completing-read prompt candidates :keep-order t))
         (index (column-index answer names)))
    (when (eq index :unknown)
      (error "No such column: ~a (a header's name, or a number from 0)" answer))
    index))

;;; Keeping and deleting records

(defun field-matches-p (scanner text length delimiter index)
  "True when SCANNER matches the value of field INDEX of the record TEXT holds
in its first LENGTH characters, or with INDEX NIL the record itself; a record
without that field does not match. The value is matched where it stands,
nothing made, unless it holds a doubled quote, as every record of a large
file is."
  (if (null index)
      (and (cl-ppcre:scan scanner text :start 0 :end length) t)
      (multiple-value-bind (field-start field-end) (csv-field-bounds text 0 length delimiter index)
        (when field-start
          (multiple-value-bind (start end quoted) (csv-value-bounds text field-start field-end)
            (and (if (and quoted (search "\"\"" text :start2 start :end2 end))
                     (cl-ppcre:scan scanner (csv-field-value text field-start field-end))
                     (cl-ppcre:scan scanner text :start start :end end))
                 t))))))

(defun records-range (buffer)
  "The records a filter works on, START and END of whole lines: the lines the
selection touches, or every line; never the first, the header."
  (multiple-value-bind (start end) (selected-lines buffer)
    (let ((after-header (min (buffer-size buffer) (1+ (buffer-line-end buffer 0)))))
      (if start
          (values (max start after-header) end)
          (values after-header (buffer-size buffer))))))

(defun filter-records (keep)
  "Ask for a column and a regular expression, and delete the records whose
column the pattern does not match, with KEEP, or those it matches."
  (let* ((buffer (current-buffer))
         (index (read-column buffer (if keep "Keep records whose column: " "Delete records whose column: ")))
         (pattern (read-string (format nil "~:[Delete~;Keep~] records whose ~a matches regexp: "
                                       keep (if index (format nil "$~d" index) "line"))))
         (scanner (and (plusp (length pattern)) (ignore-errors (compile-regex pattern))))
         (delimiter (csv-delimiter buffer)))
    (multiple-value-bind (start end) (records-range buffer)
      (cond ((zerop (length pattern)) (message "No pattern: no record kept out"))
            ((null scanner) (message "Not a valid pattern: ~a" pattern))
            ((>= start end) (message "No records but the header"))
            (t (rewrite-lines buffer start end (if keep "Keeping records" "Deleting records")
                              (lambda (text length)
                                (let ((matches (field-matches-p scanner text length delimiter index)))
                                  (if keep matches (not matches))))
                              (lambda (gone changed)
                                (declare (ignore changed))
                                (format nil "Deleted ~:d ~:[matching~;non-matching~] record~p" gone keep gone))))))))

(define-command csv-keep-records ()
  "Keep the records whose column matches a regular expression and delete the
rest: the column is asked for by its header's name or its number, counted
from 0 (or the whole record), then the pattern, tried on the field's value
alone, quotes taken off (^ and $ are its start and end). Smart case, as in search: a
pattern with a capital letter heeds case. The lines the selection touches,
or every record; the header line always stays. One step to undo; a large
file is filtered in the background."
  (filter-records t))

(define-command csv-delete-records ()
  "Delete the records whose column matches a regular expression, keeping the
rest: asked for as \\[csv-keep-records] asks. The header line always stays."
  (filter-records nil))

;;; Rearranging the columns

(defun parse-layout (spec)
  "The columns SPEC asks for: columns separated by commas, each made of $N
\(field N, counted from 0; $NF the last, $* the whole record) and \"text\"
\(\\\" a quote, \\\\ a backslash inside), side by side to join them. A list of
columns, each a list of its parts: a field's index, :LAST, :RECORD, or a
string. An error says where SPEC is not understood."
  (let ((columns '()) (parts '()) (i 0) (n (length spec)))
    (flet ((fail (what) (error "~a at ~d of ~a" what (1+ i) spec)))
      (loop
        (loop while (and (< i n) (member (char spec i) '(#\Space #\Tab))) do (incf i))
        (when (>= i n)
          (push (nreverse parts) columns)
          (return (nreverse columns)))
        (let ((c (char spec i)))
          (cond ((char= c #\,)
                 (push (nreverse parts) columns)
                 (setf parts '())
                 (incf i))
                ((char= c #\$)
                 (incf i)
                 (cond ((and (<= (+ i 2) n) (string-equal (subseq spec i (+ i 2)) "NF"))
                        (push :last parts)
                        (incf i 2))
                       ((and (< i n) (char= (char spec i) #\*))
                        (push :record parts)
                        (incf i))
                       ((and (< i n) (digit-char-p (char spec i)))
                        (let ((end (or (position-if-not #'digit-char-p spec :start i) n)))
                          (push (parse-integer spec :start i :end end) parts)
                          (setf i end)))
                       (t (fail "A $ needs a number, NF or *"))))
                ((char= c #\")
                 (let ((out (make-string-output-stream)))
                   (incf i)
                   (loop
                     (when (>= i n) (fail "A quote is left open"))
                     (let ((d (char spec i)))
                       (cond ((char= d #\") (incf i) (return))
                             ((and (char= d #\\) (< (1+ i) n))
                              (write-char (char spec (1+ i)) out)
                              (incf i 2))
                             (t (write-char d out) (incf i)))))
                   (push (get-output-stream-string out) parts)))
                (t (fail "Not understood"))))))))

(defun field-span (text from length delimiter spans count part)
  "Where the field PART names (an index, :LAST or :RECORD) stands in the
record TEXT holds from FROM below LENGTH, as written: START and END, both
LENGTH for a field the record lacks. SPANS holds where the record's first
fields are and COUNT how many it has (CSV-FIELD-SPANS)."
  (declare (type (simple-array fixnum (*)) spans) (type fixnum from length count))
  (let ((index (case part (:record -1) (:last (1- count)) (t part))))
    (declare (type fixnum index))
    (cond ((minusp index) (values from length))
          ((>= index count) (values length length))
          ((< (1+ (* 2 index)) (length spans)) (values (aref spans (* 2 index)) (aref spans (1+ (* 2 index)))))
          ;; past what SPANS has room for ($NF of a wide record)
          (t (csv-field-bounds text from length delimiter index)))))

(defun put-value (out at text start end unquote double)
  "Put TEXT from START below END in the string OUT from AT, or only count it
when OUT is NIL: with UNQUOTE a doubled quote as one, with DOUBLE each quote
doubled. Where the next character goes."
  (declare (type (or null (simple-array character (*))) out) (type simple-string text)
           (type fixnum at start end) (optimize speed))
  (if (not (or unquote double))
      (progn (when out (replace out text :start1 at :start2 start :end2 end))
             (+ at (- end start)))
      (let ((i start))
        (declare (type fixnum i))
        (loop while (< i end)
              do (let ((c (schar text i)))
                   (when (and unquote (char= c #\") (< (1+ i) end) (char= (schar text (1+ i)) #\"))
                     (incf i))
                   (when (and double (char= c #\"))
                     (when out (setf (schar out at) #\"))
                     (incf at))
                   (when out (setf (schar out at) c))
                   (incf at)
                   (incf i)))
        at)))

(defun column-needs-quotes-p (text from length delimiter spans count parts)
  "True when the column made of PARTS joined must be quoted: its text holds
the DELIMITER (a character), a quote or a line end, or begins or ends with a
space."
  (let ((first-char nil) (last-char nil))
    (flet ((look (string start end)
             (declare (type simple-string string) (type fixnum start end))
             (when (< start end)
               (unless first-char (setf first-char (schar string start)))
               (setf last-char (schar string (1- end)))
               (loop for i of-type fixnum from start below end
                     for c = (schar string i)
                     thereis (or (char= c delimiter) (char= c #\") (char= c #\Newline) (char= c #\Return))))))
      (or (loop for part in parts
                thereis (if (stringp part)
                            (look part 0 (length part))
                            (multiple-value-bind (start end)
                                (multiple-value-call #'csv-value-bounds text (field-span text from length delimiter spans count part))
                              (look text start end))))
          (eql first-char #\Space)
          (eql last-char #\Space)))))

(defun put-record (out text from length delimiter spans count layout &key (at 0))
  "Put the record LAYOUT makes of TEXT from FROM below LENGTH in the string
OUT from AT, or only count it when OUT is NIL (REARRANGED-RECORD); where it
ends."
  (let ((at at))
    (declare (type fixnum at))
    (loop for (column . more) on layout
          do (if (and column (null (rest column)) (not (stringp (first column))))
                 ;; one field alone: as it is written
                 (multiple-value-bind (start end) (field-span text from length delimiter spans count (first column))
                   (setf at (put-value out at text start end nil nil)))
                 (let ((quote (column-needs-quotes-p text from length delimiter spans count column)))
                   (when quote (when out (setf (schar out at) #\")) (incf at))
                   (dolist (part column)
                     (setf at (if (stringp part)
                                  (put-value out at part 0 (length part) nil quote)
                                  (multiple-value-bind (start end quoted)
                                      (multiple-value-call #'csv-value-bounds text (field-span text from length delimiter spans count part))
                                    (put-value out at text start end quoted quote)))))
                   (when quote (when out (setf (schar out at) #\")) (incf at))))
             (when more
               (when out (setf (schar out at) delimiter))
               (incf at)))
    at))

(defparameter *layout-most-placed* 256
  "How many of a record's first fields are placed at once for a layout; fields
further on are found one at a time.")

(defun layout-width (layout)
  "How many of a record's first fields LAYOUT names by number, to place at
once: one more than the largest index it names, at least one, at most
*LAYOUT-MOST-PLACED*."
  (min *layout-most-placed*
       (1+ (reduce #'max (loop for column in layout append (remove-if-not #'integerp column)) :initial-value 0))))

(defun rearranged-record (text length delimiter layout &optional (width (layout-width layout)))
  "The record TEXT holds in its first LENGTH characters, its columns as
LAYOUT (PARSE-LAYOUT) asks, DELIMITER the code of the separator, WIDTH its
LAYOUT-WIDTH: a column of one field alone is that field as written; one
joining several parts, or a text, is written anew, quoted when it needs to
be (a quote inside doubled). A field the record lacks is empty. A
byte-order mark the record begins with, as a file's first line may, is no
part of its first field and stays at its start. The record is read once and
the new one is the only thing made, measured first and then filled, as every
line of a large file is."
  (let ((delimiter (code-char delimiter))
        (spans (make-array (* 2 width) :element-type 'fixnum :initial-element 0))
        (mark (and (plusp length) (char= (char text 0) (code-char #xfeff)))))
    (declare (dynamic-extent spans))
    (let* ((from (if mark 1 0))
           (count (csv-field-spans text from length delimiter spans))
           (out (make-string (put-record nil text from length delimiter spans count layout :at from))))
      (when mark (setf (char out 0) (code-char #xfeff)))
      (put-record out text from length delimiter spans count layout :at from)
      out)))

(define-command csv-rearrange-columns ()
  "Rearrange the columns of every record, the columns asked for counted from
0: $0,$1,$3,$4\"cent\",\"hello\" leaves out the third column, joins cent to
the fifth's value and adds a column holding hello. $N is field N from 0, $NF
the last, $* the whole record; \"text\" is text as it is, joined to what
stands beside it. A field alone keeps its quotes as written; joined text is
quoted when it holds the separator or a quote. A field a record lacks is
empty. The header line is rearranged too, like every other. The lines the
selection touches, or every line; one step to undo; a large file is done in
the background."
  (let* ((buffer (current-buffer))
         (spec (read-string "Columns from $0 ($0,$2,$1\"text\"): "))
         (layout (parse-layout spec))
         (width (layout-width layout))
         (delimiter (csv-delimiter buffer)))
    (multiple-value-bind (start end) (selected-lines buffer)
      (unless start (setf start 0 end (buffer-size buffer)))
      (cond ((string= (string-trim " " spec) "") (message "No columns given: nothing rearranged"))
            ((>= start end) (message "No records to rearrange"))
            (t (rewrite-lines buffer start end "Rearranging columns"
                              (lambda (text length) (rearranged-record text length delimiter layout width))
                              (lambda (gone changed)
                                (declare (ignore gone))
                                (format nil "Rearranged ~:d line~:p" changed))))))))

;;; Keys

(defun install-csv-records ()
  "Bind the package's keys in CSV and TSV buffers, under C-c r."
  (define-keys (mode-keymap 'csv-mode) '(("C-c r k" csv-keep-records) ("C-c r d" csv-delete-records)
                                         ("C-c r a" csv-rearrange-columns)))
  (label-keys (mode-keymap 'csv-mode) '(("C-c r" "records")))
  (commands-for-mode 'csv-mode "a CSV or TSV buffer, with the csv-records package"
                     'csv-keep-records 'csv-delete-records 'csv-rearrange-columns))

(install-csv-records)
