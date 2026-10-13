;;;; csv-records/test.lisp --- The csv-records package: records kept or deleted by a pattern, columns rearranged.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test csv-records
  :description "The csv-records package: records kept or deleted by a pattern on one column (by its header's name or its number from 0) or the whole record, the header always kept; columns rearranged as awk's print, counted from 0; the same in memory and on disk in pieces.")

(defun csv-records-keys (text)
  "TEXT as the keys that type it, a space as SPC."
  (format nil "~{~a~^ ~}" (map 'list (lambda (c) (if (char= c #\Space) "SPC" (string c))) text)))

(defun csv-records-after (keys text &key on-disk (mode "csv-mode") crlf)
  "The text after typing KEYS on TEXT in MODE, with CRLF its lines ending
with a carriage return and a newline. With ON-DISK, the work goes to disk in
tiny pieces, a few lines each, so that a small text exercises it."
  (flet ((run ()
           (session-text (nth-value 1 (run-keys-in-turns (list (format nil "M-x ~a RET ~a" (csv-records-keys mode) keys) "")
                                                         :text text :width 100 :crlf crlf)))))
    (if on-disk
        (let ((tuatara.defaults::*sort-in-memory-limit* 0)
              (tuatara.defaults::*filter-piece* 16))
          (run))
        (run))))

(defparameter *csv-records-sample*
  (format nil "NAME,CITY,NOTE~%Ann,Auckland,tea~%Bob,\"Napier, Hawke's Bay\",coffee~%Cat,Nelson,\"said \"\"hi\"\"\"~%Dan,Auckland~%")
  "Records: a quoted field holding the separator, one holding doubled quotes, one short a field.")

(define-test (csv-records layout-read)
  (is equal '((0) (1) (3) (4 "cent") ("hello")) (csv-records::parse-layout "$0,$1,$3,$4\"cent\",\"hello\""))
  (is equal '((:last) (:record) (2 " " 0)) (csv-records::parse-layout "$NF, $* , $2 \" \" $0"))
  (is equal '(("a\"b\\")) (csv-records::parse-layout "\"a\\\"b\\\\\""))
  (is equal '((0) () (1)) (csv-records::parse-layout "$0,,$1"))
  (fail (csv-records::parse-layout "$x"))
  (fail (csv-records::parse-layout "\"open"))
  (fail (csv-records::parse-layout "$0 + $1")))

(define-test (csv-records record-rearranged)
  (flet ((arranged (spec record)
           (csv-records::rearranged-record record (length record) 44 (csv-records::parse-layout spec))))
    ;; the user's example: five columns, the third left out, cent joined to the fifth, hello added
    (is string= "a,b,d,ecent,hello" (arranged "$0,$1,$3,$4\"cent\",\"hello\"" "a,b,c,d,e"))
    ;; a field alone keeps its quotes as written; one joined is quoted anew when it needs it
    (is string= "\"x,y\",\"x,yZ\",\"q\"\"Z\"" (arranged "$1,$1\"Z\",$2\"Z\"" "a,\"x,y\",\"q\"\"\""))
    ;; a field the record lacks is empty; $NF the last; $* the record
    (is string= ",c,a,b,c" (arranged "$9,$NF,$*" "a,b,c"))
    ;; a quoted field joined to text is quoted again only when its value needs it
    (is string= "abcZ,\" xZ\"" (arranged "$0\"Z\",$1\"Z\"" "\"abc\",\" x\""))
    ;; $NF follows each record's own length
    (is string= "e" (arranged "$NF" "a,b,c,d,e"))
    (is string= "a" (arranged "$NF" "a"))))

(define-test (csv-records keep-and-delete)
  (dolist (on-disk '(nil t))
    ;; by the header's name: the quoted field's value, the comma inside it no separator
    (is string= (format nil "NAME,CITY,NOTE~%Bob,\"Napier, Hawke's Bay\",coffee~%")
        (csv-records-after (format nil "C-c r k C I T Y RET ~a RET" (csv-records-keys "^Napier, H")) *csv-records-sample* :on-disk on-disk))
    ;; by its number, from 0: the city, Auckland's records deleted, the header kept though it does not match
    (is string= (format nil "NAME,CITY,NOTE~%Bob,\"Napier, Hawke's Bay\",coffee~%Cat,Nelson,\"said \"\"hi\"\"\"~%")
        (csv-records-after "C-c r d 1 RET a u c k RET" *csv-records-sample* :on-disk on-disk))
    ;; a doubled quote read as one; a record short of the column does not match
    (is string= (format nil "NAME,CITY,NOTE~%Cat,Nelson,\"said \"\"hi\"\"\"~%")
        (csv-records-after (format nil "C-c r k $ 2 RET ~a RET" (csv-records-keys "\"hi\"$")) *csv-records-sample* :on-disk on-disk))
    ;; the whole record
    (is string= (format nil "NAME,CITY,NOTE~%Ann,Auckland,tea~%Dan,Auckland~%")
        (csv-records-after "C-c r k RET l a n d RET" *csv-records-sample* :on-disk on-disk))
    ;; a tab-separated file by its own separator
    (is string= (format nil "A	B~%2	y~%")
        (csv-records-after "C-c r k B RET y RET" (format nil "A	B~%1	x~%2	y~%") :on-disk on-disk :mode "tsv-mode")))
  ;; no such column: said, nothing changed
  (multiple-value-bind (f editor) (run-keys "M-x c s v - m o d e RET C-c r k P L A C E RET" :text *csv-records-sample* :width 100)
    (is string= *csv-records-sample* (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "No such column: PLACE")))
  ;; one step to undo
  (is string= *csv-records-sample* (csv-records-after "C-c r d 1 RET a u c k RET u" *csv-records-sample*)))

(define-test (csv-records rearranged)
  (dolist (on-disk '(nil t))
    ;; every line, the header too, as awk prints them
    (is string= (format nil "NOTE,NAME,NOTE!~%tea,Ann,tea!~%coffee,Bob,coffee!~%\"said \"\"hi\"\"\",Cat,\"said \"\"hi\"\"!\"~%,Dan,!~%")
        (csv-records-after (format nil "C-c r a ~a RET" (csv-records-keys "$2,$0,$2\"!\"")) *csv-records-sample* :on-disk on-disk)))
  ;; a CRLF file keeps its line ends
  (is string= (format nil "b,a~a~%d,c~a~%" #\Return #\Return)
      (csv-records-after "C-c r a $ 1 , $ 0 RET" (format nil "a,b~a~%c,d~a~%" #\Return #\Return) :crlf t)))
