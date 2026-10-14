;;;; outlook/mime.lisp --- Internet mail read: headers, MIME parts and their encodings, HTML as text.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A saved mail (.eml), and each message readpst takes out of a mailbox, is
;;;; internet mail: headers, then a body that may be made of parts within
;;;; parts (MIME), each in an encoding of its own (base64, quoted-printable)
;;;; and a charset. It is read here from its octets, by positions in them,
;;;; never copied whole; a part's contents are decoded only when wanted. Parts
;;;; within parts are read to a depth, and to a number of them, and a
;;;; boundary that never comes leaves the rest of the message one part.
;;;; HTML is read as text: its markup left out, links and pictures named,
;;;; nothing it points to fetched.

(in-package #:outlook)

(defparameter *mime-part-limit* 5000
  "The most parts of a message read; those after are left out, and the message says so.")

(defstruct (mime-part (:copier nil))
  "A part of internet mail, or the whole message: HEADERS, each (NAME . VALUE)
in order, NAME in lower case, VALUE unfolded; TYPE, its content type in lower
case (text/plain); PARAMETERS of the type, and DISPOSITION and its
DISPOSITION-PARAMETERS, each (NAME . VALUE); OCTETS the whole message's,
the part's headers from START below HEADER-END in them, its body from
BODY-START below BODY-END; PARTS, the parts it is made of; MESSAGE, a MIME-PART, when
it is a message itself."
  headers type parameters disposition disposition-parameters octets start header-end body-start body-end
  (parts '()) message)

(setf (documentation 'mime-part-p 'function) "True when OBJECT is a MIME-PART.")

(defun mime-header (part name)
  "The value of the header NAME (lower case) of PART, the first there is, or NIL."
  (cdr (assoc name (mime-part-headers part) :test #'string=)))

;;; Lines and headers

(defun mime-line-end (octets start end)
  "Where the line of OCTETS from START ends, below END: the position of its
line feed, or END."
  (or (position 10 octets :start start :end end) end))

(defun mime-headers (octets start end)
  "The headers of the message or part in OCTETS from START below END, as two
values: a list of (NAME . VALUE), NAME in lower case and VALUE unfolded (its
lines joined) as text; and where its body starts (after the empty line, or
END)."
  (let ((headers '()) (i start))
    (loop while (< i end)
          do (let* ((line-end (mime-line-end octets i end))
                    (text-end (if (and (> line-end i) (= 13 (aref octets (1- line-end)))) (1- line-end) line-end)))
               (cond ((= text-end i) (return-from mime-headers (values (nreverse headers) (min end (1+ line-end)))))
                     ((and headers (member (aref octets i) '(32 9)))       ; a line folded on
                      (setf (cdr (first headers))
                            (concatenate 'string (cdr (first headers)) " "
                                         (string-trim " 	" (octets-string octets :start i :end text-end)))))
                     (t (let ((colon (position 58 octets :start i :end text-end)))
                          (if colon
                              (push (cons (string-downcase (string-trim " 	" (octets-string octets :start i :end colon)))
                                          (string-trim " 	" (octets-string octets :start (1+ colon) :end text-end)))
                                    headers)
                              ;; no header: the body began without its empty line
                              (return-from mime-headers (values (nreverse headers) i))))))
               (setf i (1+ line-end))))
    (values (nreverse headers) end)))

(defun mime-parameters (text)
  "TEXT, a header's value such as text/plain; charset=\"utf-8\", as two
values: what comes before its first ; in lower case, and its parameters,
each (NAME . VALUE), NAME in lower case, a value given in parts or in a
charset of its own (name*0*=utf-8''%C3%A9...) put together and decoded."
  (let* ((parts (mime-split-parameters text))
         (head (string-downcase (string-trim " 	" (or (first parts) ""))))
         (plain '()) (pieces '()))
    (dolist (part (rest parts))
      (let ((equals (position #\= part)))
        (when equals
          (let ((name (string-downcase (string-trim " 	" (subseq part 0 equals))))
                (value (string-trim " 	" (subseq part (1+ equals)))))
            (when (and (>= (length value) 2) (char= #\" (char value 0)) (char= #\" (char value (1- (length value)))))
              (setf value (cl-ppcre:regex-replace-all "\\\\(.)" (subseq value 1 (1- (length value))) "\\1")))
            (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^([^*]+)\\*(?:(\\d+))?(\\*)?$" name)
              (if match
                  (push (list (aref groups 0) (if (aref groups 1) (parse-integer (aref groups 1)) 0) (and (or (aref groups 2) (null (aref groups 1))) t) value)
                        pieces)
                  (push (cons name value) plain)))))))
    ;; a value in parts or with its charset: name*=utf-8''..., name*0*=...; name*1=...
    (dolist (name (remove-duplicates (mapcar #'first pieces) :test #'string=))
      (let* ((mine (sort (remove-if-not (lambda (p) (string= name (first p))) pieces) #'< :key #'second))
             (charset nil)
             (octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
        (loop for (nil index encoded value) in mine
              do (let ((value value))
                   (when (and encoded (= index 0))
                     (let ((first (position #\' value)) )
                       (when first
                         (let ((second (position #\' value :start (1+ first))))
                           (when second
                             (setf charset (subseq value 0 first) value (subseq value (1+ second))))))))
                   (loop for b across (if encoded (percent-decoded-octets value) (string-octets value))
                         do (vector-push-extend b octets))))
        (push (cons name (mail-decode (coerce octets '(simple-array (unsigned-byte 8) (*))) charset)) plain)))
    (values head (nreverse plain))))

(defun mime-split-parameters (text)
  "TEXT split at each ; not within double quotes."
  (let ((parts '()) (start 0) (quoted nil))
    (loop for i from 0 below (length text)
          for c = (char text i)
          do (cond ((and quoted (char= c #\\)) (incf i))
                   ((char= c #\") (setf quoted (not quoted)))
                   ((and (not quoted) (char= c #\;)) (push (subseq text start i) parts) (setf start (1+ i)))))
    (nreverse (cons (subseq text (min start (length text))) parts))))

;;; Encoded words in headers

(defun mime-decode-words (text)
  "TEXT, a header's value, its encoded words (=?charset?B?...?= and
=?charset?Q?...?=) decoded in their charset, the space between two of them
left out."
  (if (not (search "=?" text))
      text
      (let ((out (make-string-output-stream)) (at 0) (last-end nil))
        (cl-ppcre:do-scans (start end group-starts group-ends "=\\?([^?\\s]+)\\?([bBqQ])\\?([^?]*)\\?=" text)
          (let ((between (subseq text at start)))
            ;; only spaces between two encoded words: they go
            (unless (and last-end (= at last-end) (every (lambda (c) (member c '(#\Space #\Tab))) between))
              (write-string between out)))
          (let* ((charset (subseq text (aref group-starts 0) (aref group-ends 0)))
                 (charset (subseq charset 0 (or (position #\* charset) (length charset))))
                 (encoding (char-upcase (char text (aref group-starts 1))))
                 (data (subseq text (aref group-starts 2) (aref group-ends 2)))
                 (octets (if (char= encoding #\B)
                             (base64-octets (string-octets data))
                             (mime-quoted-printable-octets (string-octets (substitute #\Space #\_ data))))))
            (write-string (mail-decode octets charset) out))
          (setf at end last-end end))
        (write-string text out :start at)
        (get-output-stream-string out))))

;;; Transfer encodings

(defun mime-quoted-printable-octets (octets &key (start 0) (end (length octets)))
  "The octets quoted-printable in OCTETS from START below END stands for: =XX
the octet XX, = at a line's end joining it to the next, the rest as it is."
  (let ((out (make-array (- end start) :element-type '(unsigned-byte 8) :fill-pointer 0)) (i start))
    (flet ((hex (o) (cond ((<= 48 o 57) (- o 48)) ((<= 65 o 70) (- o 55)) ((<= 97 o 102) (- o 87)))))
      (loop while (< i end)
            do (let ((o (aref octets i)))
                 (if (/= o 61)
                     (progn (vector-push o out) (incf i))
                     (let ((a (and (< (+ i 1) end) (hex (aref octets (+ i 1)))))
                           (b (and (< (+ i 2) end) (hex (aref octets (+ i 2))))))
                       (cond ((and a b) (vector-push (+ (* 16 a) b) out) (incf i 3))
                             ;; a soft line break: = then spaces then the line's end
                             ((let ((j (1+ i)))
                                (loop while (and (< j end) (member (aref octets j) '(32 9))) do (incf j))
                                (cond ((and (< j end) (= (aref octets j) 10)) (setf i (1+ j)) t)
                                      ((and (< (1+ j) end) (= (aref octets j) 13) (= (aref octets (1+ j)) 10)) (setf i (+ j 2)) t)
                                      ((= j end) (setf i end) t))))
                             (t (vector-push o out) (incf i))))))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

;;; Parts

(defun mime-read (octets &key (start 0) (end (length octets)) (depth 0))
  "The internet mail in OCTETS from START below END, as a MIME-PART: its
headers, its type, its parts read in turn to a depth of twice
*OUTLOOK-NESTING-LIMIT*, and a part that is a message read as one."
  (multiple-value-bind (headers body-start) (mime-headers octets start end)
    (let ((part (make-mime-part :headers headers :octets octets :start start :header-end body-start
                                :body-start body-start :body-end end)))
      (multiple-value-bind (type parameters) (mime-parameters (or (cdr (assoc "content-type" headers :test #'string=)) "text/plain"))
        (setf (mime-part-type part) (if (find #\/ type) type "text/plain")
              (mime-part-parameters part) parameters))
      (let ((disposition (cdr (assoc "content-disposition" headers :test #'string=))))
        (when disposition
          (multiple-value-bind (kind parameters) (mime-parameters disposition)
            (setf (mime-part-disposition part) kind (mime-part-disposition-parameters part) parameters))))
      (let ((type (mime-part-type part)))
        (cond ((>= depth (* 2 *outlook-nesting-limit*)))
              ((uiop:string-prefix-p "multipart/" type)
               (let ((boundary (cdr (assoc "boundary" (mime-part-parameters part) :test #'string=))))
                 (when (and boundary (plusp (length boundary)))
                   (setf (mime-part-parts part) (mime-split octets body-start end boundary depth)))))
              ((and (string= type "message/rfc822") (mime-plain-encoding-p part))
               (setf (mime-part-message part) (mime-read octets :start body-start :end end :depth (1+ depth))))))
      part)))

(defun mime-plain-encoding-p (part)
  "True when PART's body is written as it is (7bit, 8bit, binary, or none said)."
  (member (string-downcase (string-trim " " (or (mime-header part "content-transfer-encoding") "7bit")))
          '("7bit" "8bit" "binary") :test #'string=))

(defun mime-split (octets start end boundary depth)
  "The parts of a multipart body in OCTETS from START below END, between the
lines --BOUNDARY, each read with MIME-READ at DEPTH + 1; the last running to
END when its closing line --BOUNDARY-- never comes. NIL when the boundary
is never found."
  (let* ((delimiter (string-octets (concatenate 'string "--" boundary)))
         (marks '()) (closed nil) (i start))
    ;; the delimiter lines: their starts, and where the parts after them start
    (loop while (and (< i end) (not closed))
          do (let ((line-end (mime-line-end octets i end)))
               (when (and (<= (+ i (length delimiter)) line-end)
                          (not (mismatch delimiter octets :start2 i :end2 (+ i (length delimiter)))))
                 (let ((after (+ i (length delimiter))))
                   (if (and (< (1+ after) line-end) (= 45 (aref octets after)) (= 45 (aref octets (1+ after))))
                       (progn (push (cons i nil) marks) (setf closed t))
                       (push (cons i (min end (1+ line-end))) marks))))
               (setf i (1+ line-end))))
    (setf marks (nreverse marks))
    (let ((parts '()))
      (loop for (mark . rest) on marks
            for part-start = (cdr mark)
            while (and part-start (< (length parts) *mime-part-limit*))
            do (let* ((next (first rest))
                      ;; the line end before the next delimiter belongs to it
                      (part-end (if next
                                    (let ((e (car next)))
                                      (when (and (> e part-start) (= 10 (aref octets (1- e)))) (decf e))
                                      (when (and (> e part-start) (= 13 (aref octets (1- e)))) (decf e))
                                      e)
                                    end)))
                 (push (mime-read octets :start part-start :end (max part-start part-end) :depth (1+ depth)) parts)))
      (nreverse parts))))

(defun mime-part-content (part &key (most *outlook-largest-file*))
  "The octets of PART's body, its transfer encoding undone (base64,
quoted-printable); at most MOST of them read, *OUTLOOK-LARGEST-FILE* unless
given."
  (let* ((octets (mime-part-octets part))
         (start (mime-part-body-start part))
         (end (min (mime-part-body-end part) (+ start most)))
         (encoding (string-downcase (string-trim " " (or (mime-header part "content-transfer-encoding") "")))))
    (cond ((string= encoding "base64") (base64-octets octets :start start :end end))
          ((string= encoding "quoted-printable") (mime-quoted-printable-octets octets :start start :end end))
          (t (subseq octets start end)))))

(defun mime-part-text (part &key (most *outlook-largest-file*))
  "PART's body as text, in its charset, from at most MOST octets of it as written."
  (mail-decode (mime-part-content part :most most) (cdr (assoc "charset" (mime-part-parameters part) :test #'string=))))

(defun mime-leaves (part)
  "The parts of PART that are made of no others, in order; a part that is a
message is one, its own parts not among them."
  (if (mime-part-parts part)
      (mapcan #'mime-leaves (mime-part-parts part))
      (list part)))

(defun mime-part-name (part)
  "The name PART gives its contents, as an attachment's file: its
disposition's filename or its type's name, encoded words decoded; NIL for none."
  (let ((name (or (cdr (assoc "filename" (mime-part-disposition-parameters part) :test #'string=))
                  (cdr (assoc "name" (mime-part-parameters part) :test #'string=)))))
    (and name (plusp (length name)) (mime-decode-words name))))

(defun mime-body-part (leaves)
  "The part among LEAVES, a message's parts made of no others, that is its
text to show: its first plain text, else its first HTML, neither named nor
an attachment; NIL for none."
  (flet ((shown (type) (find-if (lambda (leaf) (and (string= (mime-part-type leaf) type) (not (mime-part-name leaf))
                                                     (not (equal (mime-part-disposition leaf) "attachment"))))
                                leaves)))
    (or (shown "text/plain") (shown "text/html"))))

(defun mime-attachment-p (part body)
  "True when PART, a part made of no others, is an attachment of the message
whose BODY is shown (a part, or NIL): one marked so, one with a name, or one
that is not text, nor the text shown, nor a text another way of saying it."
  (let ((type (mime-part-type part)))
    (or (equal (mime-part-disposition part) "attachment")
        (and (mime-part-name part) t)
        (mime-part-message part)
        (not (member type '("text/plain" "text/html") :test #'string=))
        (and body (not (eq part body)) (equal type (mime-part-type body))))))

;;; Dates

(defparameter *mail-zones*
  '(("UT" . 0) ("UTC" . 0) ("GMT" . 0) ("Z" . 0) ("EST" . -500) ("EDT" . -400) ("CST" . -600) ("CDT" . -500)
    ("MST" . -700) ("MDT" . -600) ("PST" . -800) ("PDT" . -700))
  "Time zones mail may name by letters, each with its offset as +HHMM is written.")

(defun mail-date-time (text)
  "The universal time a mail's date TEXT (Tue, 7 Oct 2026 09:15:00 +1300)
says, or NIL when it cannot be read as one."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "(?i)(\\d{1,2})\\s+(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*\\.?\\s+(\\d{2,4})\\s+(\\d{1,2}):(\\d{2})(?::(\\d{2}))?(?:\\s*([+-]\\d{4}|[a-z]{1,5}))?"
                                (or text ""))
    (when match
      (ignore-errors
       (let* ((day (parse-integer (aref groups 0)))
              (month (1+ (position (string-downcase (aref groups 1)) '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec") :test #'string=)))
              (year (let ((y (parse-integer (aref groups 2)))) (cond ((< y 50) (+ y 2000)) ((< y 100) (+ y 1900)) (t y))))
              (zone (let ((z (aref groups 6)))
                      (cond ((null z) 0)
                            ((member (char z 0) '(#\+ #\-)) (parse-integer z))
                            (t (or (cdr (assoc z *mail-zones* :test #'string-equal)) 0)))))
              (offset (* (signum zone) (+ (* 3600 (floor (abs zone) 100)) (* 60 (mod (abs zone) 100))))))
         (and (valid-date-p year month day) (<= 1900 year 9999)
              (- (encode-universal-time (if (aref groups 5) (parse-integer (aref groups 5)) 0) (parse-integer (aref groups 4))
                                        (parse-integer (aref groups 3)) day month year 0)
                 offset)))))))

;;; A message read

(defparameter *mail-shown-headers*
  '("reply-to" "sender" "return-path" "in-reply-to" "x-mailer" "user-agent" "x-originating-ip" "importance" "x-priority")
  "The headers of internet mail shown among what a message says of itself,
besides those every message shows; and those readpst adds (X-libpst-...).")

(defun mime-mail (part &key (depth 0))
  "The MAIL the internet mail PART (MIME-READ) is: its headers, its body (its
plain text, else its HTML read as text), its attachments, a message among
them read as one to a depth of *OUTLOOK-NESTING-LIMIT*, and its headers
whole."
  (flet ((header (name) (let ((value (mime-header part name))) (and value (mime-decode-words value)))))
    (let* ((leaves (mime-leaves part))
           (body (mime-body-part leaves))
           (received (let ((first (mime-header part "received")))
                       (and first (find #\; first) (mail-date-time (subseq first (1+ (position #\; first :from-end t)))))))
           (date (header "date"))
           (mail (make-mail :from (header "from") :to (header "to") :cc (header "cc") :bcc (header "bcc")
                            :subject (header "subject") :sent (mail-date-time date) :received received
                            :message-id (header "message-id")
                            :headers (string-right-trim '(#\Return #\Newline)
                                                        (octets-string (mime-part-octets part)
                                                                       :start (mime-part-start part)
                                                                       :end (mime-part-header-end part))))))
      (unless (mail-sent mail) (setf (mail-date-text mail) date))
      (dolist (name *mail-shown-headers*)
        (mail-detail mail (string-capitalize name) (header name)))
      (loop for (name . value) in (mime-part-headers part)
            when (uiop:string-prefix-p "x-libpst-" name)
              do (mail-detail mail name (mime-decode-words value)))
      (when body
        (if (string= (mime-part-type body) "text/html")
            (setf (mail-body mail) (html-text (mime-part-text body)) (mail-body-kind mail) :html)
            (setf (mail-body mail) (mime-part-text body) (mail-body-kind mail) :plain)))
      (when (and (null (mime-part-parts part)) (uiop:string-prefix-p "multipart/" (mime-part-type part)))
        (mail-note mail "Its parts could not be told apart: the boundary they are said to be between was never found"))
      (when (>= (length leaves) *mime-part-limit*)
        (mail-note mail "Only its first ~:d parts were read" *mime-part-limit*))
      (let ((count 0))
        (dolist (leaf leaves)
          (when (mime-attachment-p leaf body)
            (incf count)
            (let ((nested (mime-part-message leaf)))
              (push (make-mail-attachment
                     :name (or (mime-part-name leaf)
                               (and nested (let ((subject (mime-header nested "subject"))) (and subject (mime-decode-words subject))))
                               (format nil "part ~d (~a)" count (mime-part-type leaf)))
                     :mime (mime-part-type leaf) :content-id (mime-header leaf "content-id")
                     :message (and nested (< depth *outlook-nesting-limit*) (mime-mail nested :depth (1+ depth)))
                     :octets (and (not nested) (mime-part-content leaf))
                     :why (and nested (>= depth *outlook-nesting-limit*) "a message nested too deep to read (*outlook-nesting-limit*)"))
                    (mail-attachments mail))))))
      (setf (mail-attachments mail) (nreverse (mail-attachments mail)))
      mail)))

(defun eml-read (octets)
  "The MAIL a saved internet mail's OCTETS hold."
  (mime-mail (mime-read octets)))

;;; HTML read as text

(defparameter *html-entities*
  '(("nbsp" . #.(string (code-char #xA0))) ("copy" . "©") ("reg" . "®") ("trade" . "™") ("hellip" . "…")
    ("mdash" . "—") ("ndash" . "–") ("lsquo" . "‘") ("rsquo" . "’") ("ldquo" . "“") ("rdquo" . "”")
    ("bull" . "•") ("middot" . "·") ("euro" . "€") ("pound" . "£") ("laquo" . "«") ("raquo" . "»")
    ("times" . "×") ("deg" . "°") ("shy" . ""))
  "Named characters of HTML that XML has not, each (NAME . TEXT).")

(defun html-entity-text (name)
  "The text the HTML entity NAME (without & and ;) stands for, or NIL."
  (or (xml-entity name) (cdr (assoc name *html-entities* :test #'string=))))

(defun html-attribute (tag name)
  "The value of the attribute NAME in TAG, the text of an HTML tag, entities
decoded; NIL when it has none."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings (format nil "(?is)\\s~a\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))" name) tag)
    (and match (xml-text-value (or (aref groups 0) (aref groups 1) (aref groups 2)) #'html-entity-text))))

(defparameter *html-block-tags*
  '("p" "div" "br" "tr" "li" "h1" "h2" "h3" "h4" "h5" "h6" "table" "ul" "ol" "blockquote" "hr" "pre" "dt" "dd" "title")
  "HTML elements that begin a line of their own.")

(defun html-text (html)
  "The text of HTML, as a message shows it: its markup left out, scripts,
styles and comments with it; elements that make blocks on lines of their
own; a link followed by its address in <>, when that is not its text; a
picture as [picture: what it shows], its address said and not fetched;
entities decoded and spaces run together as HTML runs them."
  (let ((out (make-array 1024 :element-type 'character :adjustable t :fill-pointer 0))
        (i 0) (n (length html)) (pre 0) (link nil) (link-start 0))
    (labels ((put (text) (loop for c across text do (vector-push-extend c out)))
             (text-run (start end)
               (let ((text (xml-text-value (subseq html start end) #'html-entity-text)))
                 (put (if (plusp pre) text (cl-ppcre:regex-replace-all "[ \\t\\r\\n]+" text " "))))))
      (loop while (< i n)
            do (let ((open (position #\< html :start i)))
                 (when (null open) (text-run i n) (return))
                 (text-run i open)
                 (if (and (< (+ open 3) n) (string= "<!--" html :start2 open :end2 (+ open 4)))
                     (setf i (let ((close (search "-->" html :start2 (+ open 4)))) (if close (+ close 3) n)))
                     (let* ((close (or (xml-tag-end html open) (1- n)))
                            (tag (subseq html open (1+ close)))
                            (ending (and (> (length tag) 1) (char= #\/ (char tag 1))))
                            (name (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^</?\\s*([A-Za-z0-9]+)" tag)
                                    (if match (string-downcase (aref groups 0)) ""))))
                       (setf i (1+ close))
                       (cond
                         ((and (not ending) (member name '("script" "style" "head" "xml" "object") :test #'string=))
                          ;; what they hold is no text: on to their end
                          (let ((end (search (format nil "</~a" name) html :start2 i :test #'char-equal)))
                            (setf i (if end (let ((e (position #\> html :start end))) (if e (1+ e) n)) n))))
                         ((string= name "pre") (if ending (setf pre (max 0 (1- pre))) (incf pre)) (put (string #\Newline)))
                         ((member name *html-block-tags* :test #'string=)
                          (put (string #\Newline))
                          (when (and (string= name "li") (not ending)) (put "• ")))
                         ((member name '("td" "th") :test #'string=) (unless ending (put (string #\Tab))))
                         ((and (string= name "a") (not ending))
                          (setf link (html-attribute tag "href") link-start (fill-pointer out)))
                         ((string= name "a")
                          (let ((shown (string-trim " " (subseq out (min link-start (fill-pointer out))))))
                            (when (and link (plusp (length link)) (string-not-equal shown link)
                                       (string-not-equal (format nil "mailto:~a" shown) link))
                              (put (format nil " <~a>" link))))
                          (setf link nil))
                         ((string= name "img")
                          (let ((alt (html-attribute tag "alt")) (source (html-attribute tag "src")))
                            (put (format nil "[picture~@[: ~a~]~@[; ~a~]]"
                                         (and alt (plusp (length alt)) alt)
                                         (and source (if (cl-ppcre:scan "(?i)^(https?|ftp):" source)
                                                         (format nil "from ~a, not fetched" source)
                                                         source)))))))))))
      (string-trim '(#\Newline #\Space)
                   (cl-ppcre:regex-replace-all "\\n{3,}" (cl-ppcre:regex-replace-all "(?m)[ \\t]+$" (coerce out 'simple-string) "")
                                               (format nil "~%~%"))))))
