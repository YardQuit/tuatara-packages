;;;; palimpsest/test.lisp --- The palimpsest package: a PDF's saves taken apart, and the signs of its editing found.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test palimpsest
  :description "The palimpsest package: PDFs written here by hand (an original and its incremental updates,
cross-reference and object streams with predictors, white boxes over text, invisible text, signatures that stop
short, a shadowed object, mixed font subsets, metadata that disagrees, data after %%EOF) read from their octets
alone; every revision found, compared and written out with a log naming the offsets and octets of each sign;
hostile files (loops, huge counts, offsets out of range, a stream that unpacks without end) read as far as they
make sense, never hanging or failing; the original never changed; the report and the object view.")

;;; PDFs written by hand

(defun pal-octets (&rest parts)
  "PARTS joined as octets: strings as Latin-1, integers as one octet each, vectors as they are."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (part parts (coerce out '(simple-array (unsigned-byte 8) (*))))
      (etypecase part
        (string (loop for c across part do (vector-push-extend (char-code c) out)))
        (integer (vector-push-extend part out))
        (vector (loop for o across part do (vector-push-extend o out)))))))

(defun pal-object (number body)
  "The octets of the object NUMBER 0: BODY a string, its value, or (:stream ENTRIES OCTETS), a stream with
ENTRIES inside its dictionary and its /Length worked out."
  (if (and (consp body) (eq (first body) :stream))
      (let ((data (if (stringp (third body)) (pal-octets (third body)) (third body))))
        (pal-octets (format nil "~d 0 obj~%<< ~a /Length ~d >>~%stream~%" number (second body) (length data))
                    data (format nil "~%endstream~%endobj~%")))
      (pal-octets (format nil "~d 0 obj~%~a~%endobj~%" number body))))

(defun pal-last-startxref (octets)
  "The offset the last startxref of OCTETS names."
  (let ((at (search (pal-octets "startxref") octets :from-end t)))
    (parse-integer (map 'string #'code-char octets) :start (+ at 10) :junk-allowed t)))

(defun pal-save (base objects &key (trailer "/Root 1 0 R /Info 6 0 R") free)
  "BASE (octets, empty for a new file) with OBJECTS (each (NUMBER . BODY)) saved after it: the objects, a
cross-reference table listing them (and the numbers FREE as free), a trailer with TRAILER and /Prev, startxref
and %%EOF."
  (let* ((out (if (plusp (length base)) (pal-octets base) (pal-octets "%PDF-1.7" 10 "%" 226 227 207 211 10)))
         (offsets '()))
    (dolist (object objects)
      (push (cons (car object) (length out)) offsets)
      (setf out (pal-octets out (pal-object (car object) (cdr object)))))
    (let* ((xref (length out))
           (entries (sort (append (mapcar (lambda (o) (list (car o) (cdr o) "n")) offsets)
                                  (mapcar (lambda (n) (list n 0 "f")) free)
                                  (and (zerop (length base)) (list (list 0 0 "f"))))
                          #'< :key #'first))
           (size (1+ (reduce #'max entries :key #'first)))
           (text (with-output-to-string (s)
                   (format s "xref~%")
                   (loop while entries
                         do (let ((run (loop for e on entries
                                             for k from (first (first entries))
                                             while (= (first (first e)) k) collect (first e))))
                              (format s "~d ~d~%" (first (first run)) (length run))
                              (dolist (e run)
                                (format s "~10,'0d ~5,'0d ~a~c~c" (if (= (first e) 0) 0 (second e)) (if (= (first e) 0) 65535 0) (third e) #\Return #\Newline))
                              (setf entries (nthcdr (length run) entries))))
                   (format s "trailer~%<< /Size ~d ~a~@[ /Prev ~d~] >>~%startxref~%~d~%%%EOF~%"
                           (max size (if (plusp (length base)) 20 0)) trailer (and (plusp (length base)) (pal-last-startxref base)) xref))))
      (pal-octets out text))))

(defparameter *pal-original-objects*
  '((1 . "<< /Type /Catalog /Pages 2 0 R >>")
    (2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
    (3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>")
    (4 :stream "" "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders) Tj ET BT /F1 12 Tf 72 680 Td (Total: 100) Tj ET")
    (5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>")
    (6 . "<< /Producer (Kea Writer 1.0) /Creator (Kea Writer) /CreationDate (D:20260101090000+13'00') /ModDate (D:20260101090000+13'00') >>"))
  "The objects of the original invoice the tests change.")

(defun pal-original ()
  "The original invoice: one page, its total 100."
  (pal-save #() *pal-original-objects*))

(defun pal-edited ()
  "The invoice changed by a later save: its total 900, saved by an online editor."
  (pal-save (pal-original)
            '((4 :stream "" "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders) Tj ET BT /F1 12 Tf 72 680 Td (Total: 900) Tj ET")
              (6 . "<< /Producer (iLovePDF) /Creator (Kea Writer) /CreationDate (D:20260101090000+13'00') /ModDate (D:20260105120000+13'00') >>"))))

(defun pal-read (octets)
  "OCTETS read as a PDF, with its revisions compared and every sign found, as three values: the PDF, the accounts and the findings."
  (let* ((pdf (palimpsest::read-pdf octets :name "test.pdf"))
         (accounts (loop for r across (palimpsest::pdf-revisions pdf) collect (palimpsest::compare-revision pdf r))))
    (values pdf accounts (palimpsest::all-findings pdf accounts))))

(defun pal-finding (findings strength text)
  "The first of FINDINGS of STRENGTH (or any, for NIL) whose words hold TEXT."
  (find-if (lambda (f) (and (or (null strength) (eq strength (palimpsest::finding-strength f)))
                            (search text (palimpsest::finding-words f))))
           findings))

(defun pal-has (text part)
  "Check that TEXT holds PART."
  (true (search part text) "~s not in~%~a" part text))

(defun pal-zlib-stored (octets)
  "OCTETS as zlib data of stored deflate blocks, as /FlateDecode takes them."
  (let ((parts (list (pal-octets #x78 #x01))))
    (loop for start from 0 below (max 1 (length octets)) by 65535
          do (let* ((end (min (length octets) (+ start 65535))) (n (- end start)))
               (push (pal-octets (if (>= end (length octets)) 1 0) (logand n 255) (ash n -8)
                                 (logand (lognot n) 255) (logand (ash (lognot n) -8) 255) (subseq octets start end))
                     parts)))
    (push (pal-octets 0 0 0 0) parts)
    (apply #'pal-octets (nreverse parts))))

(defun pal-zlib-bomb (repeats)
  "Deflate data, fixed codes, of a zero then REPEATS copies of 258 octets back one: tiny, unpacking to REPEATS times 258 octets."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)) (buffer 0) (count 0))
    (labels ((bits (value n) (dotimes (k n) (setf buffer (logior buffer (ash (ldb (byte 1 k) value) count))) (incf count)
                               (when (= count 8) (vector-push-extend buffer out) (setf buffer 0 count 0))))
             (code (value n) (loop for k from (1- n) downto 0 do (bits (ldb (byte 1 k) value) 1))))
      (bits 1 1) (bits 1 2)
      (code #b00110000 8)
      (dotimes (k repeats) (code #b11000101 8) (code 0 5))
      (code 0 7)
      (when (plusp count) (vector-push-extend buffer out)))
    (pal-octets #x78 #x01 out)))

;;; Saves, cross-references and objects

(define-test (palimpsest palimpsest-saves-found)
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-edited))
    (let ((revisions (palimpsest::pdf-revisions pdf)))
      (is = 2 (length revisions))
      (is eq :table (palimpsest::revision-xref-kind (aref revisions 0)))
      (is equal '(1 2 3 4 5 6) (palimpsest::revision-added (aref revisions 0)))
      (is equal '(4 6) (palimpsest::revision-changed (aref revisions 1)))
      (is eql (palimpsest::revision-xref-at (aref revisions 0)) (palimpsest::revision-prev (aref revisions 1))))
    ;; the page's text compared line by line
    (let ((change (first (palimpsest::account-changes (second accounts)))))
      (true change)
      (is equal '((:removed "Total: 100") (:added "Total: 900")) (palimpsest::page-change-text change)))
    (true (pal-finding findings :look "The file was changed after it was first saved"))
    (true (pal-finding findings :look "Page 1 was changed in revision 2"))
    (true (pal-finding findings :look "names iLovePDF"))
    (true (pal-finding findings :look "names another program in /Producer"))))

(define-test (palimpsest palimpsest-cross-reference-streams)
  ;; objects packed in a Flate object stream, a cross-reference stream with a PNG predictor
  (let* ((packed-objects '((1 . "<< /Type /Catalog /Pages 2 0 R >>")
                           (2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                           (3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>")
                           (5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")))
         (bodies (mapcar (lambda (o) (format nil "~a " (cdr o))) packed-objects))
         (header (format nil "~{~a ~}" (loop for o in packed-objects for k from 0
                                             collect (format nil "~d ~d" (car o) (reduce #'+ (subseq bodies 0 k) :key #'length)))))
         (data (pal-octets header (format nil "~{~a~}" bodies)))
         (out (pal-octets "%PDF-1.5" 10))
         (places '()))
    (push (cons 4 (length out)) places)
    (setf out (pal-octets out (pal-object 4 '(:stream "" "BT /F1 10 Tf 50 50 Td (Packed hello) Tj ET"))))
    (push (cons 6 (length out)) places)
    (setf out (pal-octets out (pal-object 6 (list :stream (format nil "/Type /ObjStm /N 4 /First ~d /Filter /FlateDecode" (length header))
                                                  (pal-zlib-stored data)))))
    (let* ((xref-at (length out))
           (rows (loop for n from 0 to 7
                       collect (cond ((= n 0) (list 0 0 255))
                                     ((assoc n places) (list 1 (cdr (assoc n places)) 0))
                                     ((= n 7) (list 1 xref-at 0))
                                     (t (list 2 6 (position n packed-objects :key #'car))))))
           ;; each row: type (1), offset (2), third (1), each row after the Up predictor's tag 2
           (raw (loop for (a b c) in rows append (list a (ash b -8) (logand b 255) c)))
           (predicted (let ((previous (list 0 0 0 0)) (out '()))
                        (loop for k from 0 below (length raw) by 4
                              do (let ((row (subseq raw k (+ k 4))))
                                   (push 2 out)
                                   (loop for x in row for p in previous do (push (logand 255 (- x p)) out))
                                   (setf previous row)))
                        (coerce (nreverse out) '(vector (unsigned-byte 8))))))
      (setf out (pal-octets out (pal-object 7 (list :stream "/Type /XRef /Size 8 /W [1 2 1] /Root 1 0 R /Filter /FlateDecode /DecodeParms << /Predictor 12 /Columns 4 >>"
                                                    (pal-zlib-stored predicted)))
                            (format nil "startxref~%~d~%%%EOF~%" xref-at))))
    (multiple-value-bind (pdf) (pal-read out)
      (let ((revision (aref (palimpsest::pdf-revisions pdf) 0)))
        (is eq :stream (palimpsest::revision-xref-kind revision))
        (is eq :packed (palimpsest::entry-kind (palimpsest::lookup pdf revision 3)))
        (let ((pages (palimpsest::pages-of pdf revision)))
          (is = 1 (length pages))
          (is equal '("Packed hello") (palimpsest::text-lines (palimpsest::marks-of pdf revision (first pages)))))))))

(define-test (palimpsest palimpsest-decoders)
  (is string= "Hello" (map 'string #'code-char (palimpsest::ascii-hex-decoded (pal-octets "48 65 6C6c6F>") 100)))
  (is string= "Man " (map 'string #'code-char (palimpsest::ascii-85-decoded (pal-octets "9jqo^~>") 100)))
  (is string= "abcxxx" (map 'string #'code-char (palimpsest::run-length-decoded (pal-octets 2 "abc" 254 "x" 128) 100)))
  (is equalp (pal-octets 45 45 45 45 45 65 45 45 45 66)
      (palimpsest::lzw-decoded (pal-octets #x80 #x0B #x60 #x50 #x22 #x0C #x0C #x85 #x01) 100 1))
  ;; a stream that would unpack without end is stopped at the limit, said so
  (let ((tuatara-user:*palimpsest-largest-stream* (* 256 1024)))
    (multiple-value-bind (data why) (palimpsest::decode-filters (pal-zlib-bomb 4000) '(("/FlateDecode")) (* 256 1024))
      (false data)
      (pal-has why "more than"))))

;;; What pages draw

(defun pal-one-page (content &key (fonts "/F1 5 0 R") (more '()))
  "A one-page PDF drawing CONTENT with FONTS, MORE objects beside."
  (pal-save #() (append (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                              '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                              (cons 3 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << ~a >> >> /Contents 4 0 R >>" fonts))
                              (list 4 :stream "" content)
                              '(5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
                              '(6 . "<< /Producer (Kea Writer) >>"))
                        more)))

(define-test (palimpsest palimpsest-content-signs)
  ;; a white box drawn over text: the text named, the operators in the log's evidence
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-one-page "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET 1 1 1 rg 70 695 100 20 re f"))
    (declare (ignore accounts))
    (let ((covered (pal-finding findings :strong "covered by a white shape")))
      (true covered)
      (pal-has (palimpsest::finding-words covered) "Secret 42")
      (pal-has (format nil "~{~a~%~}" (palimpsest::finding-evidence covered)) "1 1 1 rg")
      (pal-has (format nil "~{~a~%~}" (palimpsest::finding-evidence covered)) "70 695 100 20 re f")
      (true pdf)))
  ;; invisible, tiny and white text, text off the page
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page "BT /F1 12 Tf 3 Tr 72 600 Td (Hidden words) Tj 0 Tr ET BT /F1 0.5 Tf 72 500 Td (Tiny print) Tj ET BT 1 1 1 rg /F1 12 Tf 72 400 Td (White words) Tj 0 g ET BT /F1 12 Tf 900 900 Td (Far away) Tj ET"))
    (declare (ignore pdf accounts))
    (true (pal-finding findings nil "holds invisible text: \"Hidden words\""))
    (true (pal-finding findings :look "too small to read"))
    (true (pal-finding findings nil "white text on a white page"))
    (true (pal-finding findings :look "lies outside the page")))
  ;; a line drawn character by character, figures in another font
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (concatenate 'string
                                           "BT /F1 12 Tf 72 700 Td (The quick brown fox) Tj ET BT /F1 12 Tf 72 680 Td (jumps over the lazy dog) Tj ET "
                                           "BT /F1 12 Tf 72 660 Td (Amount due today) Tj ET BT /F1 12 Tf 72 640 Td (Thank you for your business in 2026) Tj ET "
                                           "BT /F1 12 Tf 72 620 Td (T) Tj (o) Tj (t) Tj (a) Tj (l) Tj (d) Tj (u) Tj (e) Tj (n) Tj (o) Tj (w) Tj ET "
                                           "BT /F1 12 Tf 72 600 Td (Paid on account:) Tj /F2 12 Tf 180 0 Td (950) Tj ET")
                              :fonts "/F1 5 0 R /F2 7 0 R"
                              :more '((7 . "<< /Type /Font /Subtype /Type1 /BaseFont /Courier >>"))))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :look "1 line on page 1 is drawn a character at a time"))
    (let ((retyped (pal-finding findings :look "1 figure is in another font than the line around it")))
      (true retyped)
      (is equal '("\"950\" in Courier, its line in Helvetica") (palimpsest::finding-details retyped))))
  ;; one font in two subsets, a ToUnicode map read
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page "BT /F1 12 Tf 72 700 Td <00010002> Tj ET BT /F2 12 Tf 72 680 Td (Second) Tj ET"
                              :fonts "/F1 7 0 R /F2 8 0 R"
                              :more (list '(7 . "<< /Type /Font /Subtype /Type0 /BaseFont /ABCDEF+Arial /Encoding /Identity-H /DescendantFonts [9 0 R] /ToUnicode 10 0 R >>")
                                          '(8 . "<< /Type /Font /Subtype /TrueType /BaseFont /GHIJKL+Arial >>")
                                          '(9 . "<< /Type /Font /Subtype /CIDFontType2 /BaseFont /ABCDEF+Arial /W [1 [500 600]] >>")
                                          (list 10 :stream "" "/CIDInit /ProcSet findresource begin 1 begincodespacerange <0000> <FFFF> endcodespacerange 2 beginbfchar <0001> <004B> <0002> <0065> endbfchar end"))))
    (declare (ignore accounts))
    (true (pal-finding findings :look "The font Arial is in the file as 2 subsets (ABCDEF+Arial, GHIJKL+Arial)"))
    (let* ((revision (aref (palimpsest::pdf-revisions pdf) 0)))
      (is equal '("Ke" "Second") (palimpsest::text-lines (palimpsest::marks-of pdf revision (first (palimpsest::pages-of pdf revision))))))))

(define-test (palimpsest palimpsest-content-appended)
  ;; a later save lays a second content stream over the page
  (let* ((original (pal-original))
         (edited (pal-save original
                           '((3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >> /Contents [4 0 R 7 0 R] >>")
                             (7 :stream "" "1 1 1 rg 70 675 120 20 re f BT /F1 12 Tf 72 680 Td (Total: 50) Tj ET")))))
    (multiple-value-bind (pdf accounts findings) (pal-read edited)
      (declare (ignore pdf))
      (true (pal-finding findings :strong "one of them appended in a later save"))
      (true (pal-finding findings :strong "covered by a white shape drawn over it, added in a later save"))
      (let ((change (first (palimpsest::account-changes (second accounts)))))
        (true (find "Total: 50" (palimpsest::page-change-added change) :key #'palimpsest::mark-text :test #'equal))))))

;;; Metadata, ends

(define-test (palimpsest palimpsest-metadata-and-ends)
  (let* ((xmp (format nil "<?xpacket begin='' id='W5M0MpCehiHzreSzNTczkc9d'?><x:xmpmeta xmlns:x='adobe:ns:meta/'><rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'>~
<rdf:Description xmlns:xmp='http://ns.adobe.com/xap/1.0/' xmlns:xmpMM='http://ns.adobe.com/xap/1.0/mm/' xmlns:stEvt='http://ns.adobe.com/xap/1.0/sType/ResourceEvent#' ~
xmp:CreateDate='2020-03-01T09:00:00+13:00' xmp:CreatorTool='Kea Writer' xmpMM:DocumentID='uuid:kea-1' xmpMM:OriginalDocumentID='uuid:other-9'>~
<xmpMM:History><rdf:Seq><rdf:li stEvt:action='created' stEvt:when='2020-03-01T09:00:00+13:00' stEvt:softwareAgent='Kea Writer'/>~
<rdf:li stEvt:action='saved' stEvt:when='2026-01-05T12:00:00+13:00' stEvt:softwareAgent='Sejda PDF'/></rdf:Seq></xmpMM:History>~
</rdf:Description></rdf:RDF></x:xmpmeta><?xpacket end='w'?>"))
         (octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R /Metadata 7 0 R >>")
                                     '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                     '(6 . "<< /CreationDate (D:20260301090000+13'00') /ModDate (D:20250101090000+13'00') /Creator (Kea Writer) >>")
                                     (list 7 :stream "/Type /Metadata /Subtype /XML" xmp))))
         (with-tail (pal-octets octets "1 0 obj << /Hidden (after the end) >> endobj" 10)))
    (multiple-value-bind (pdf accounts findings) (pal-read with-tail)
      (declare (ignore accounts))
      (true (pal-finding findings :strong "give different creation dates"))
      (true (pal-finding findings :strong "was last changed"))
      (true (pal-finding findings :look "edit history of 2 steps, by Sejda"))
      (true (pal-finding findings :look "made from another one"))
      (let ((tail (pal-finding findings :strong "follow the last %%EOF")))
        (true tail)
        (pal-has (format nil "~{~a~%~}" (palimpsest::finding-evidence tail)) "/Hidden (after the end)"))
      (is = 1 (length (palimpsest::pdf-revisions pdf))))))

;;; Signatures

(defun pal-signed (&key (short 0))
  "The invoice signed in its first save, its /ByteRange covering it all but SHORT octets less."
  (let* ((placeholder "0000000000")
         (base (pal-save #() (append (butlast *pal-original-objects*)
                                     (list '(6 . "<< /Producer (Kea Writer 1.0) >>")
                                           (cons 8 (format nil "<< /Type /Sig /Filter /Adobe.PPKLite /SubFilter /adbe.pkcs7.detached /Name (Kea Signer) /M (D:20260102100000+13'00') /ByteRange [0 ~a ~a ~a] /Contents <~a> >>"
                                                           placeholder placeholder placeholder (make-string 64 :initial-element #\0)))
                                           (list 9 :stream "" "BT /F1 12 Tf 72 300 Td (Shadow total: 1) Tj ET")))))
         (text (map 'string #'code-char base))
         (open (position #\< text :start (search "/Contents" text)))
         (close (1+ (position #\> text :start open)))
         (range (format nil "[0 ~10,'0d ~10,'0d ~10,'0d]" open close (- (length base) close short)))
         (at (search "[0 0000000000" text)))
    (pal-octets (subseq base 0 at) range (subseq base (+ at (length range))))))

(define-test (palimpsest palimpsest-signatures)
  ;; signed, then a later save changes the page and brings an object hidden in the signed part into use
  (let ((after (pal-save (pal-signed)
                         '((3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >> /Contents [4 0 R 9 0 R] >>")))))
    (multiple-value-bind (pdf accounts findings) (pal-read after)
      (declare (ignore pdf accounts))
      (true (pal-finding findings :record "An approval signature (object 8), dated 2026-01-02"))
      (true (pal-finding findings :strong "after the signature in object 8, changes what the document shows"))
      (true (pal-finding findings :strong "a shadow attack"))
      (false (pal-finding findings :strong "stops short"))))
  ;; a /ByteRange that stops short of where its save ends
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-signed :short 10))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "stops short"))))

;;; Hostile files

(define-test (palimpsest palimpsest-hostile-files)
  (let ((start (get-internal-real-time)))
    (dolist (octets (list
                     ;; a page tree in a circle, a /Prev naming its own section
                     (pal-save #() '((1 . "<< /Type /Catalog /Pages 2 0 R >>") (2 . "<< /Type /Pages /Kids [2 0 R 3 0 R] /Count 1 >>")
                                     (3 . "<< /Type /Page /Parent 2 0 R /Contents 4 0 R >>") (4 :stream "" "q q q Q BT ET")))
                     (pal-octets "%PDF-1.4" 10 "1 0 obj << /Type /Catalog /Pages 1 0 R >> endobj" 10 "xref" 10 "0 999999999" 10
                                 "0000000009 00000 n" 13 10 "trailer << /Root 1 0 R /Prev 20 >>" 10 "startxref" 10 "20" 10 "%%EOF" 10)
                     ;; offsets out of range, a startxref pointing past the end
                     (pal-octets "%PDF-1.4" 10 "xref" 10 "0 2" 10 "0000000000 65535 f" 13 10 "9999999999 00000 n" 13 10
                                 "trailer << /Root 1 0 R /Size 2 >>" 10 "startxref" 10 "123456789" 10 "%%EOF" 10)
                     ;; an object stream claiming a billion objects, arrays nested deep, no %%EOF
                     (pal-octets "%PDF-1.5" 10 "1 0 obj << /Type /ObjStm /N 1000000000 /First 3 /Length 5 >> stream" 10 "1 0 2" 10 "endstream endobj" 10
                                 "2 0 obj " (make-string 5000 :initial-element #\[) " endobj" 10)
                     ;; a content stream that unpacks without end
                     (pal-one-page "" :more (list (list 7 :stream "/Filter /FlateDecode" (pal-zlib-bomb 20000))))
                     ;; nothing of a PDF at all
                     (pal-octets "not a PDF" 0 0 0 255)
                     (pal-octets)))
      (let ((tuatara-user:*palimpsest-largest-stream* (* 1024 1024)))
        (multiple-value-bind (pdf accounts findings) (pal-read octets)
          (declare (ignore accounts))
          (true (plusp (length (palimpsest::pdf-revisions pdf))))
          (true (listp findings)))))
    (true (< (- (get-internal-real-time) start) (* 30 internal-time-units-per-second)))))

;;; The revisions written, their logs, the report

(define-test (palimpsest palimpsest-revisions-and-logs)
  (with-test-directory (dir)
    (let* ((octets (pal-edited))
           (path (namestring (merge-pathnames "invoice.pdf" dir)))
           (folder (tuatara.defaults::new-private-folder (tuatara.defaults::private-cache-directory "palimpsest/"))))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)) (write-sequence octets out))
      (let ((stamp (tuatara.io:path-stamp path))
            (job (palimpsest::make-palimpsest-job nil path folder)))
        (palimpsest::examine job)
        (let* ((analysis (palimpsest::palimpsest-job-analysis job))
               (files (palimpsest::analysis-files analysis))
               (pdf (palimpsest::analysis-pdf analysis))
               (second-revision (aref (palimpsest::pdf-revisions pdf) 1)))
          (is = 2 (length files))
          ;; each revision its octets up to its %%EOF line, read only, in a folder for you alone
          (destructuring-bind (file log sum) (first files)
            (is equalp (subseq octets 0 (palimpsest::revision-end (aref (palimpsest::pdf-revisions pdf) 0))) (tuatara.io:file-octets file))
            (is = #o400 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp file))))
            (is = #o700 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp folder))))
            (true (search "/palimpsest/" file))
            (true sum)
            (pal-has (uiop:read-file-string log) "revision-1.pdf SHA-256: "))
          (let ((log (uiop:read-file-string (second (second files)))))
            ;; how the revision was found, with its offsets in hex and decimal
            (pal-has log (format nil "Its %%EOF is at ~a" (palimpsest::offset-words (palimpsest::revision-eof second-revision))))
            (pal-has log (format nil "Its trailer's /Prev is ~d" (palimpsest::revision-xref-at (aref (palimpsest::pdf-revisions pdf) 0))))
            (pal-has log "the cross-reference section of revision 1 (the original)")
            (pal-has log (format nil "This revision is the source file's octets from 0x00000000 to ~a" (palimpsest::hex-offset (1- (length octets)))))
            ;; each sign with its rule, the octets it rests on and the steps to see it
            (pal-has log "Worth a look: /Producer names iLovePDF")
            (pal-has log "2F 50 72 6F 64 75 63 65 72")
            (pal-has log "text: /Producer (iLovePDF)")
            (pal-has log "Steps to see it by hand:")
            ;; what changed, both places of each object, and the text found in the decoded stream
            (pal-has log (format nil "object 4 now at ~a, before at ~a"
                                 (palimpsest::offset-words (palimpsest::pobj-at (palimpsest::revision-object pdf second-revision 4)))
                                 (palimpsest::offset-words (palimpsest::pobj-at (palimpsest::revision-object pdf (aref (palimpsest::pdf-revisions pdf) 0) 4)))))
            (pal-has log "text - Total: 100")
            (pal-has log "text + Total: 900")
            (pal-has log "(Total: 900) Tj")
            ;; the same file, the same log but its date line
            (let ((again (palimpsest::revision-log analysis second-revision (third (second files)))))
              (flet ((dateless (text) (cl-ppcre:regex-replace "Written: .*" text "")))
                (is string= (dateless log) (dateless again)))))
          ;; the report
          (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "*Palimpsest test*"))))
            (tuatara.modes:set-major-mode buffer 'palimpsest::palimpsest-mode)
            (palimpsest::report-render buffer analysis)
            (let ((text (tuatara.buffer:buffer-string buffer)))
              (pal-has text "Palimpsest: invoice.pdf")
              (pal-has text "Signs worth a look, none of them strong.")
              (pal-has text "** Revision 2: 0x")
              (pal-has text "revision-2.pdf  its log revision-2.log")
              (pal-has text "+ Total: 900"))
            (true (tuatara.buffer:buffer-local buffer :buttons))
            (tuatara.buffer:kill-buffer buffer))
          ;; an object shown: its place, its dictionary, its stream as text, or in hex
          (let ((text (palimpsest::object-text analysis second-revision 4)))
            (pal-has text "Object 4 0 begins at")
            (pal-has text "(Total: 900) Tj"))
          (pal-has (palimpsest::object-text analysis second-revision 4 :hex t) "42 54 20 2F 46 31")
          ;; saved where asked, each copy's sum its original's; never beside the evidence, in the cache, or over a file
          (pal-has (palimpsest::save-refusal analysis (namestring dir)) "the folder of the PDF examined")
          (pal-has (palimpsest::save-refusal analysis folder) "within palimpsest's own folder in the cache")
          (let* ((kept (namestring (merge-pathnames "kept/" dir)))
                 (saved (palimpsest::save-copies analysis kept)))
            (is equal '("revision-1.pdf" "revision-1.log" "revision-2.pdf" "revision-2.log" "report.txt") (mapcar #'first saved))
            (loop for (name sum) in saved
                  for source in (list (first (first files)) (second (first files)) (first (second files)) (second (second files)))
                  do (is string= (tuatara:file-sha256 source) sum)
                     (is string= sum (tuatara:file-sha256 (tuatara.io:path-join kept name))))
            (is equalp (tuatara.io:file-octets (first (second files))) (tuatara.io:file-octets (tuatara.io:path-join kept "revision-2.pdf")))
            (let ((report (uiop:read-file-string (tuatara.io:path-join kept "report.txt"))))
              (pal-has report "Palimpsest: invoice.pdf")
              (pal-has report (format nil "Saved to ~a" kept))
              (pal-has report (format nil "~a  ~arevision-2.log" (second (fourth saved)) kept)))
            (is string= kept (palimpsest::analysis-saved analysis))
            (pal-has (palimpsest::save-refusal analysis kept) "revision-1.pdf, revision-1.log, revision-2.pdf, revision-2.log, report.txt are there already"))
          ;; the PDF itself never touched
          (is equalp octets (tuatara.io:file-octets path))
          (is = (tuatara.io:stamp-mtime stamp) (tuatara.io:stamp-mtime (tuatara.io:path-stamp path))))
        (tuatara.io:delete-tree folder)))))

(define-test (palimpsest palimpsest-command)
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "invoice.pdf" dir)))
          (f (tuatara.frontend.fake:make-fake-frontend :width 140 :height 40))
          (start (get-internal-real-time)))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)) (write-sequence (pal-edited) out))
      (let ((scratch (tuatara.buffer:find-buffer "*scratch*"))) (when scratch (tuatara.buffer:kill-buffer scratch)))
      (tuatara.buffer:make-buffer "*scratch*" :text "")
      (tuatara.frontend.fake:fake-queue-keys f (format nil "M-x ~a RET C-a C-k ~a RET" (rimu-typed "palimpsest") (rimu-typed path)))
      (labels ((check ()
                 (when (and (some #'palimpsest::palimpsest-job-p tuatara.runtime:*jobs*)
                            (< (- (get-internal-real-time) start) (* 30 internal-time-units-per-second)))
                   (sleep 0.02)
                   (tuatara.frontend.fake:fake-queue-action f #'check))))
        (tuatara.frontend.fake:fake-queue-action f #'check))
      (let* ((editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                   :buffer (tuatara.buffer:find-buffer "*scratch*")))
             (buffer (session-buffer editor)))
        (is string= "*Palimpsest: invoice.pdf*" (tuatara.buffer:buffer-name buffer))
        (true (tuatara.buffer:buffer-local buffer :read-only))
        (pal-has (session-text editor) "Page 1 was changed in revision 2")
        (let ((folder (tuatara.buffer:buffer-local buffer 'palimpsest::palimpsest-folder)))
          (true (probe-file (tuatara.io:path-join folder "revision-2.log")))
          ;; closing it with nothing saved asks first: n keeps it, y closes it and deletes what it wrote
          (flet ((close-answering (answer)
                   (let ((f (tuatara.frontend.fake:make-fake-frontend :width 140 :height 40)))
                     (tuatara.frontend.fake:fake-queue-keys f (format nil "M-x ~a RET ~a RET" (rimu-typed "kill-buffer") answer))
                     (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer buffer)
                     f)))
            (close-answering "n")
            (true (tuatara.buffer:live-buffer-p buffer))
            (true (probe-file folder))
            (close-answering "y")
            (false (tuatara.buffer:live-buffer-p buffer)))
          (loop repeat 200 while (probe-file folder) do (sleep 0.02))
          (false (probe-file folder))))))
  (true (fboundp 'palimpsest::palimpsest))
  (is eq 'palimpsest::palimpsest-show-object
      (tuatara.command:lookup-key (tuatara.modes:mode-keymap 'palimpsest::palimpsest-mode) (tuatara.frontend:parse-key-sequence "o")))
  (is eq 'palimpsest::palimpsest-save
      (tuatara.command:lookup-key (tuatara.modes:mode-keymap 'palimpsest::palimpsest-mode) (tuatara.frontend:parse-key-sequence "s"))))
