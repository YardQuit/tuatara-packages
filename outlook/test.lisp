;;;; outlook/test.lisp --- The outlook package: Outlook items, saved mail and mailboxes read safely.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test outlook
  :description "The outlook package: compound files read with every chain checked (loops, streams larger than
their chains, items nested deep or in a circle refused or cut short, never hanging); Outlook items read as
messages (sender, recipients, dates, plain, HTML and compressed RTF bodies, internet headers, attachments, an
attached item as a message, what the item says of itself); messages drawn as they look, shut in, nothing
fetched; internet mail (encoded words, quoted-printable,
base64, parts within parts, a boundary that never comes); attachments opened from private copies, the
original untouched; mailboxes listed from what readpst writes, and refused with how to get readpst without
it; previews, health and load-package.")

;;; A compound file written, for the tests

(defstruct (cfbw-entry (:copier nil))
  "An entry of a compound file being written for the tests."
  name type octets child right (start #xFFFFFFFE) (size 0))

(defun cfb-test-octets (tree)
  "The octets of a compound file (sectors of 512 octets) holding TREE: a
storage (:storage NAME CHILD...) or a stream (:stream NAME OCTETS), the
first the root. Streams below 4,096 octets go in the mini stream."
  (let ((entries (make-array 0 :adjustable t :fill-pointer 0))
        (mini (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (mini-fat '()) (big '()))
    (labels ((add (node)
               (let* ((entry (make-cfbw-entry :name (second node) :type (if (eq (first node) :stream) 2 1)))
                      (index (vector-push-extend entry entries)))
                 (if (eq (first node) :stream)
                     (let ((octets (coerce (third node) '(vector (unsigned-byte 8)))))
                       (setf (cfbw-entry-octets entry) octets (cfbw-entry-size entry) (length octets)))
                     (let ((children (mapcar #'add (cddr node))))
                       (setf (cfbw-entry-child entry) (first children))
                       (loop for (a b) on children while b do (setf (cfbw-entry-right (aref entries a)) b))))
                 index)))
      (add tree)
      (setf (cfbw-entry-type (aref entries 0)) 5))
    ;; the small streams into the mini stream, the others in sectors of their own
    (loop for entry across entries
          for size = (cfbw-entry-size entry)
          when (and (= 2 (cfbw-entry-type entry)) (plusp size) (< size 4096))
            do (let ((first (floor (length mini) 64)) (count (ceiling size 64)))
                 (setf (cfbw-entry-start entry) first)
                 (loop for k below count do (push (if (< k (1- count)) (+ first k 1) #xFFFFFFFE) mini-fat))
                 (loop for o across (cfbw-entry-octets entry) do (vector-push-extend o mini))
                 (loop repeat (- (* 64 count) size) do (vector-push-extend 0 mini)))
          when (and (= 2 (cfbw-entry-type entry)) (>= size 4096))
            do (push entry big))
    (setf mini-fat (coerce (nreverse mini-fat) 'vector) big (nreverse big))
    (let* ((dir-count (ceiling (length entries) 4))
           (mini-fat-count (ceiling (length mini-fat) 128))
           (mini-count (ceiling (length mini) 512))
           (big-count (reduce #'+ big :key (lambda (e) (ceiling (cfbw-entry-size e) 512))))
           (others (+ dir-count mini-fat-count mini-count big-count))
           (fat-count (loop for f from 1 when (>= (* f 128) (+ others f)) return f))
           (total (+ fat-count others))
           (out (make-array (* 512 (1+ total)) :element-type '(unsigned-byte 8) :initial-element 0))
           (fat (make-array (* fat-count 128) :initial-element #xFFFFFFFF))
           (next fat-count))
      (labels ((put (at value count) (tuatara.io:octets-put-integer out value count :at at))
               (run (count) (let ((start next))
                              (dotimes (k count) (setf (aref fat (+ start k)) (if (< k (1- count)) (+ start k 1) #xFFFFFFFE)))
                              (incf next count)
                              (if (plusp count) start #xFFFFFFFE)))
               (sector (n) (* 512 (1+ n))))
        (dotimes (k fat-count) (setf (aref fat k) #xFFFFFFFD))
        (let* ((dir-start (run dir-count))
               (mini-fat-start (run mini-fat-count))
               (mini-start (run mini-count)))
          (dolist (entry big) (setf (cfbw-entry-start entry) (run (ceiling (cfbw-entry-size entry) 512))))
          (setf (cfbw-entry-start (aref entries 0)) (if (plusp (length mini)) mini-start #xFFFFFFFE)
                (cfbw-entry-size (aref entries 0)) (length mini))
          ;; the header
          (replace out #(#xD0 #xCF #x11 #xE0 #xA1 #xB1 #x1A #xE1))
          (put 24 #x3E 2) (put 26 3 2) (put 28 #xFFFE 2) (put 30 9 2) (put 32 6 2)
          (put 44 fat-count 4) (put 48 dir-start 4) (put 56 4096 4)
          (put 60 (if (plusp mini-fat-count) mini-fat-start #xFFFFFFFE) 4) (put 64 mini-fat-count 4)
          (put 68 #xFFFFFFFE 4) (put 72 0 4)
          (dotimes (k 109) (put (+ 76 (* 4 k)) (if (< k fat-count) k #xFFFFFFFF) 4))
          ;; the FAT, the directory, the mini FAT, the mini stream, the large streams
          (loop for value across fat for k from 0 do (put (+ (sector 0) (* 4 k)) value 4))
          (loop for entry across entries
                for at from (sector dir-start) by 128
                do (let ((name (sb-ext:string-to-octets (cfbw-entry-name entry) :external-format :utf-16le)))
                     (replace out name :start1 at)
                     (put (+ at 64) (+ 2 (length name)) 2)
                     (put (+ at 66) (cfbw-entry-type entry) 1)
                     (put (+ at 67) 1 1)
                     (put (+ at 68) #xFFFFFFFF 4)
                     (put (+ at 72) (or (cfbw-entry-right entry) #xFFFFFFFF) 4)
                     (put (+ at 76) (or (cfbw-entry-child entry) #xFFFFFFFF) 4)
                     (put (+ at 116) (cfbw-entry-start entry) 4)
                     (put (+ at 120) (cfbw-entry-size entry) 8)))
          ;; unused entries of the last directory sector: no links
          (loop for k from (length entries) below (* 4 dir-count)
                do (let ((at (+ (sector dir-start) (* 128 k)))) (put (+ at 68) #xFFFFFFFF 4) (put (+ at 72) #xFFFFFFFF 4) (put (+ at 76) #xFFFFFFFF 4)))
          (loop for value across mini-fat for k from 0 do (put (+ (sector mini-fat-start) (* 4 k)) value 4))
          (replace out mini :start1 (sector mini-start))
          (dolist (entry big) (replace out (cfbw-entry-octets entry) :start1 (sector (cfbw-entry-start entry))))))
      out)))

;;; An Outlook item written, for the tests

(defun utf-16-octets (text)
  "TEXT as UTF-16, least significant octet first."
  (sb-ext:string-to-octets text :external-format :utf-16le))

(defun msg-test-storage (name properties &key (header 32) children)
  "A storage called NAME (:storage ...) for CFB-TEST-OCTETS laid out as an
Outlook item's: PROPERTIES, each (ID TYPE VALUE), TYPE #x001F a string,
#x0102 octets, #x0003 a number, #x000B a boolean, #x0040 a universal time,
#x000D a list of the nodes of a storage within; HEADER octets before the
fixed-size properties; CHILDREN, more nodes in it."
  (let ((fixed (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (streams '()))
    (dotimes (k header) (vector-push-extend 0 fixed))
    (flet ((entry (id type value)
             (let ((octets (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
               (tuatara.io:octets-put-integer octets (logior type (ash id 16)) 4 :at 0)
               (setf (aref octets 4) 6)
               (tuatara.io:octets-put-integer octets value 8 :at 8)
               (loop for o across octets do (vector-push-extend o fixed)))))
      (loop for (id type value) in properties
            for stream-name = (format nil "__substg1.0_~4,'0X~4,'0X" id type)
            do (ecase type
                 (#x001F (let ((octets (concatenate '(vector (unsigned-byte 8)) (utf-16-octets value) #(0 0))))
                           (push (list :stream stream-name octets) streams)
                           (entry id type (length octets))))
                 (#x0102 (push (list :stream stream-name value) streams) (entry id type (length value)))
                 (#x0003 (entry id type (ldb (byte 32 0) value)))
                 (#x000B (entry id type (if value 1 0)))
                 (#x0040 (entry id type (* (+ value 9435484800) 10000000)))
                 (#x000D (push (list* :storage stream-name value) streams) (entry id type #xFFFFFFFF)))))
    (list* :storage name (list :stream "__properties_version1.0" (coerce fixed '(vector (unsigned-byte 8))))
           (append (nreverse streams) children))))

(defparameter *png-test-octets*
  (coerce '(#x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A #x00 #x00 #x00 #x0D #x49 #x48 #x44 #x52 #x00 #x00 #x00 #x01
            #x00 #x00 #x00 #x01 #x08 #x06 #x00 #x00 #x00 #x1F #x15 #xC4 #x89 #x00 #x00 #x00 #x0A #x49 #x44 #x41
            #x54 #x78 #x9C #x63 #x00 #x01 #x00 #x00 #x05 #x00 #x01 #x0D #x0A #x2D #xB4 #x00 #x00 #x00 #x00 #x49
            #x45 #x4E #x44 #xAE #x42 #x60 #x82)
          '(vector (unsigned-byte 8)))
  "A picture of one pixel, as a PNG.")

(defparameter *msg-test-sent* (encode-universal-time 0 15 20 6 10 2026 0)
  "When the test item was sent: 2026-10-06 20:15 UTC.")

(defun lzfu-test-octets (tokens)
  "Compressed RTF (LZFu) made of TOKENS, each an octet written as it is or
(OFFSET LENGTH), a reference into the dictionary; its end marked."
  (let ((body (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (write (length outlook::*lzfu-dictionary*))
        (raw 0)
        (tokens (append tokens (list :end))))
    (loop while tokens
          do (let ((control 0) (group '()))
               (dotimes (bit 8)
                 (let ((token (pop tokens)))
                   (when (null token) (return))
                   (cond ((eq token :end) (setf control (logior control (ash 1 bit))) (push (list (ash write 4)) group))
                         ((consp token)
                          (setf control (logior control (ash 1 bit)))
                          (push (list (logior (ash (first token) 4) (- (second token) 2))) group)
                          (incf write (second token)) (incf raw (second token)))
                         (t (push token group) (incf write) (incf raw)))
                   (setf write (logand write 4095))))
               (vector-push-extend control body)
               (dolist (item (nreverse group))
                 (if (consp item)
                     (tuatara.io:octets-put-integer body (first item) 2 :big-endian t)
                     (vector-push-extend item body)))))
    (let ((out (make-array (+ 16 (length body)) :element-type '(unsigned-byte 8) :initial-element 0)))
      (flet ((put (at value) (tuatara.io:octets-put-integer out value 4 :at at)))
        (put 0 (+ 12 (length body))) (put 4 raw) (put 8 #x75465A4C))
      (replace out body :start1 16)
      out)))

(defun rtf-test-tokens (text)
  "TEXT's octets as LZFu tokens written as they are."
  (coerce (sb-ext:string-to-octets text :external-format :latin-1) 'list))

(defun msg-test-octets (&key (body :plain) (inner-depth 1))
  "An Outlook item for the tests: sent by Kea Counter, to two, copied to one,
blind copied to one; its BODY plain text, HTML alone (:html) or compressed
RTF alone (:rtf); its internet headers; two attachments, a PNG and an
attached item nested INNER-DEPTH deep."
  (labels ((inner (depth)
             (msg-test-storage "__substg1.0_3701000D"
                               `((#x0037 #x001F ,(format nil "Inner subject ~d" depth))
                                 (#x001A #x001F "IPM.Note")
                                 (#x1000 #x001F ,(format nil "Inner body ~d" depth)))
                               :header 24
                               :children (when (< depth inner-depth)
                                           (list (msg-test-storage "__attach_version1.0_#00000000"
                                                                   '((#x3705 #x0003 5))
                                                                   :header 8 :children (list (inner (1+ depth)))))))))
    (cfb-test-octets
     (msg-test-storage
      "Root Entry"
      (append
       `((#x0037 #x001F "Kea survey results")
         (#x001A #x001F "IPM.Note")
         (#x0E07 #x0003 9)
         (#x0039 #x0040 ,*msg-test-sent*)
         (#x0E06 #x0040 ,(+ *msg-test-sent* 60))
         (#x0C1A #x001F "Kea Counter")
         (#x0C1F #x001F "/O=EXAMPLE/OU=FIRST/CN=RECIPIENTS/CN=KEA")
         (#x0C1E #x001F "EX")
         (#x5D01 #x001F "kea@example.org")
         (#x1035 #x001F "<survey-1@example.org>")
         (#x007D #x001F ,(format nil "Received: from mail.example.org by mx.example.net; Tue, 6 Oct 2026 20:15:30 +0000~c~cX-Origin: tested~c~c"
                                 #\Return #\Newline #\Return #\Newline)))
       (ecase body
         (:plain '((#x1000 #x001F "Twelve kea counted at dawn.")))
         (:html `((#x1013 #x0102 ,(sb-ext:string-to-octets "<html><head><meta charset='utf-8'><style>p{}</style><script>alert(1)</script></head><body><p>Kea &amp; kaka seen</p><p><a href='https://example.org/count'>the count</a></p><img src='https://example.org/track.png' alt='tracker'></body></html>" :external-format :utf-8))))
         (:rtf `((#x1009 #x0102 ,(lzfu-test-octets (append (list '(0 11)) (rtf-test-tokens "\\ansicpg1252 {\\fonttbl{\\f0 Arial;}}Kea \\'e9t\\'e9\\par \\u8364?done}"))))))))
      :children
      (list (msg-test-storage "__recip_version1.0_#00000000" '((#x3001 #x001F "Ruru Watcher") (#x39FE #x001F "ruru@example.org") (#x0C15 #x0003 1)) :header 8)
            (msg-test-storage "__recip_version1.0_#00000001" '((#x3001 #x001F "Tui Singer") (#x39FE #x001F "tui@example.org") (#x0C15 #x0003 1)) :header 8)
            (msg-test-storage "__recip_version1.0_#00000002" '((#x3001 #x001F "Weka Walker") (#x39FE #x001F "weka@example.org") (#x0C15 #x0003 2)) :header 8)
            (msg-test-storage "__recip_version1.0_#00000003" '((#x3001 #x001F "Hidden Hoiho") (#x39FE #x001F "hoiho@example.org") (#x0C15 #x0003 3)) :header 8)
            (msg-test-storage "__attach_version1.0_#00000000" `((#x3707 #x001F "kea.png") (#x3705 #x0003 1) (#x3701 #x0102 ,*png-test-octets*)) :header 8)
            (msg-test-storage "__attach_version1.0_#00000001" '((#x3705 #x0003 5)) :header 8 :children (list (inner 1))))))))

(defun write-test-octets (path octets)
  "Write OCTETS as the file at PATH; return PATH as a native string."
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence octets out))
  (namestring path))

(defun mail-test-text (mail)
  "MAIL as its buffer shows it."
  (values (tuatara.defaults::pieces-text (outlook::mail-pieces mail))))

(defun has (text part)
  "Check that TEXT holds PART."
  (true (search part text) "~s in~%~a" part text))

;;; Compound files

(define-test (outlook item-read)
    (let* ((mail (outlook::msg-read (msg-test-octets)))
         (text (mail-test-text mail)))
      (is string= "Kea survey results" (outlook::mail-subject mail))
      (is string= "Kea Counter <kea@example.org>" (outlook::mail-from mail))
      (is string= "Ruru Watcher <ruru@example.org>, Tui Singer <tui@example.org>" (outlook::mail-to mail))
      (is string= "Weka Walker <weka@example.org>" (outlook::mail-cc mail))
      (is string= "Hidden Hoiho <hoiho@example.org>" (outlook::mail-bcc mail))
      (is = *msg-test-sent* (outlook::mail-sent mail))
      (is = (+ 60 *msg-test-sent*) (outlook::mail-received mail))
      (is eq :plain (outlook::mail-body-kind mail))
    (has text "Twelve kea counted at dawn.")
    (has text "Message-ID  <survey-1@example.org>")
    (has text "2026-10-06 20:15:00 UTC")
    ;; what an examiner looks for: the item's class, its flags, the sender as the item has it
    (has text "Item: IPM.Note")
    (has text "Flags: read, unsent (a draft)")
    (has text "Sender as the item says: Kea Counter </O=EXAMPLE/OU=FIRST/CN=RECIPIENTS/CN=KEA>")
    (has text "Sender address type: EX")
    (has text "Bodies it holds: plain text")
    (has text "Internet headers")
    (has text "Received: from mail.example.org")
    ;; its attachments: a picture by its contents, and an item attached, read as a message
    (has text "Attachments: 2")
    (has text "kea.png  67 B, PNG image, 1 × 1, 8-bit RGBA")
    (has text "Inner subject 1  a message")
      (let ((inner (outlook::mail-attachment-message (second (outlook::mail-attachments mail)))))
      (true inner)
        (is string= "Inner body 1" (outlook::mail-body inner)))
    ;; the brief text a preview shows
      (let ((brief (outlook::mail-brief-text mail)))
      (has brief "Kea survey results")
      (false (search "Internet headers" brief)))))

(define-test (outlook item-bodies)
  ;; HTML alone: read as text, nothing fetched, the script left out, the link's address said
    (let* ((mail (outlook::msg-read (msg-test-octets :body :html)))
           (body (outlook::mail-body mail)))
      (is eq :html (outlook::mail-body-kind mail))
    (has body "Kea & kaka seen")
    (has body "the count <https://example.org/count>")
    (has body "[picture: tracker; from https://example.org/track.png, not fetched]")
    (false (search "alert" body))
    (has (mail-test-text mail) "Text (HTML read as text)")
    (has (funcall (outlook::mail-html mail)) "<script>alert(1)</script>"))
  ;; RTF that keeps HTML gives it back; RTF of its own has none; plain text alone has none
  (multiple-value-bind (text html)
      (outlook::rtf-text (tuatara.text:string-octets "{\\rtf1\\ansi\\fromhtml1 {\\*\\htmltag64 <p style=\"color:red\">}Kea{\\*\\htmltag72 </p>}\\htmlrtf {\\par}\\htmlrtf0 }"))
    (is string= "Kea" (string-trim '(#\Newline) text))
    (is string= "<p style=\"color:red\">Kea</p>" (string-trim '(#\Space) html)))
  (false (funcall (outlook::mail-html (outlook::msg-read (msg-test-octets :body :rtf)))))
  (false (outlook::mail-html (outlook::msg-read (msg-test-octets))))
  ;; compressed RTF alone: taken out, a reference into the dictionary followed, its text read
  (is = 207 (length outlook::*lzfu-dictionary*))
  (let* ((mail (outlook::msg-read (msg-test-octets :body :rtf)))
         (body (outlook::mail-body mail)))
    (is eq :rtf (outlook::mail-body-kind mail))
    (is string= (format nil "Kea été~%€done") body))
  (is string= "{\\rtf1\\ansi" (tuatara.text:octets-latin-1 (outlook::lzfu-decompress (lzfu-test-octets (list '(0 11)))))))

(define-test (outlook crafted-compound-files)
  (let* ((good (msg-test-octets))
         (dir (tuatara.io:octets-le good 48 4))
         (fat (* 512 (1+ (tuatara.io:octets-le good 76 4)))))
    (flet ((refused (octets words)
             (let ((start (get-internal-real-time)))
                 (fail (outlook::msg-read octets))
                 (let ((said (handler-case (progn (outlook::msg-read octets) nil) (error (e) (princ-to-string e)))))
                 (true (and said (search words said)) "~s said ~s" words said))
               (true (< (- (get-internal-real-time) start) (* 2 internal-time-units-per-second))))))
      ;; a FAT whose chain comes back on itself
      (let ((loop (copy-seq good)))
        (tuatara.io:octets-put-integer loop dir 4 :at (+ fat (* 4 dir)))
        (refused loop "loops"))
      ;; a stream saying it holds more than its chain does: the subject's
        (let* ((cfb (outlook::cfb-read good))
               (subject (find "__substg1.0_0037001F" (outlook::cfb-entries cfb)
                              :key (lambda (e) (and e (outlook::cfb-entry-name e))) :test #'equal))
             (big (copy-seq good))
               (at (+ (* 512 (1+ dir)) (* 128 (outlook::cfb-entry-index subject)) 120)))
        (tuatara.io:octets-put-integer big 4000 4 :at at)
        (refused big "more than its"))
      ;; not a compound file, and one cut short
      (refused (make-array 600 :element-type '(unsigned-byte 8) :initial-element 0) "Not a compound file")
      (refused (subseq good 0 1024) "compound file")))
  ;; items attached within items, deeper than *outlook-nesting-limit*: read to it, the rest listed as not read
  (let* ((mail (outlook::msg-read (msg-test-octets :inner-depth 20)))
         (deepest mail))
    (loop for attachment = (car (last (outlook::mail-attachments deepest)))
          while (and attachment (outlook::mail-attachment-message attachment))
          do (setf deepest (outlook::mail-attachment-message attachment)))
    (has (outlook::mail-attachment-why (car (last (outlook::mail-attachments deepest)))) "nested too deep"))
  ;; an attached item made to hold the item it is in: read once, never round and round
  (let* ((good (msg-test-octets))
         (cfb (outlook::cfb-read good))
         (entries (outlook::cfb-entries cfb))
         (inner (find "__substg1.0_3701000D" entries :key (lambda (e) (and e (outlook::cfb-entry-name e))) :test #'equal))
         (circle (copy-seq good))
         (at (+ (* 512 (1+ (tuatara.io:octets-le good 48 4))) (* 128 (outlook::cfb-entry-index inner)) 76))
         (start (get-internal-real-time)))
    (tuatara.io:octets-put-integer circle (outlook::cfb-entry-child (aref entries 0)) 4 :at at)
    (let ((mail (outlook::msg-read circle)))
      (is string= "Kea survey results" (outlook::mail-subject mail)))
    (true (< (- (get-internal-real-time) start) (* 5 internal-time-units-per-second)))))

;;; Internet mail

(defparameter *eml-test-text*
  (format nil "From: =?UTF-8?B?S8SBcmVhIEJpcmQ=?= <karearea@example.org>~@
To: Ruru <ruru@example.org>~@
Subject: =?iso-8859-1?Q?Caf=E9?= =?UTF-8?Q?_survey?=~@
Date: Tue, 6 Oct 2026 20:15:00 +1300~@
Message-ID: <eml-1@example.org>~@
Received: from a.example.org by b.example.org; Tue, 6 Oct 2026 07:16:00 -0000~@
X-Mailer: Feather 1.0~@
MIME-Version: 1.0~@
Content-Type: multipart/mixed; boundary=\"outer\"~@
~@
Preamble, not shown.~@
--outer~@
Content-Type: multipart/alternative; boundary=\"inner\"~@
~@
--inner~@
Content-Type: text/plain; charset=utf-8~@
Content-Transfer-Encoding: quoted-printable~@
~@
Caf=C3=A9 count: twelve =~@
kea.~@
--inner~@
Content-Type: text/html; charset=utf-8~@
Content-Transfer-Encoding: base64~@
~@
PHA+Q2Fmw6k8L3A+~@
--inner--~@
--outer~@
Content-Type: image/png~@
Content-Disposition: attachment; filename*=utf-8''r%C3%A9sum%C3%A9.png~@
Content-Transfer-Encoding: base64~@
~@
~a~@
--outer~@
Content-Type: message/rfc822~@
~@
From: Inner <inner@example.org>~@
Subject: Forwarded count~@
~@
Inner text.~@
--outer--~@
Epilogue.~%"
          (let ((o (coerce *png-test-octets* '(simple-array (unsigned-byte 8) (*)))))
            ;; in lines of 60, as mail writes it
            (let ((all (tuatara.frontend.tui::base64-string o 0 (length o))))
              (format nil "~{~a~^~%~}" (loop for at from 0 below (length all) by 60 collect (subseq all at (min (length all) (+ at 60))))))))
  "A saved mail for the tests: encoded words, quoted-printable, base64, parts within parts, an attached picture and message.")

(define-test (outlook internet-mail)
    (let* ((mail (outlook::eml-read (sb-ext:string-to-octets *eml-test-text* :external-format :utf-8)))
         (text (mail-test-text mail)))
      (is string= "Kārea Bird <karearea@example.org>" (outlook::mail-from mail))
      (is string= "Café survey" (outlook::mail-subject mail))
      (is = (encode-universal-time 0 15 7 6 10 2026 0) (outlook::mail-sent mail))
      (is = (encode-universal-time 0 16 7 6 10 2026 0) (outlook::mail-received mail))
      (is eq :plain (outlook::mail-body-kind mail))
      (is string= (format nil "Café count: twelve kea.") (string-right-trim '(#\Newline) (outlook::mail-body mail)))
      ;; its HTML kept beside the plain text shown, for drawing it as it looks
      (is string= "<p>Café</p>" (funcall (outlook::mail-html mail)))
    (has text "X-Mailer: Feather 1.0")
    (has text "Content-Type: multipart/mixed")
      (false (search "Preamble" (outlook::mail-body mail)))
      (let ((attachments (outlook::mail-attachments mail)))
      (is = 2 (length attachments))
        (is string= "résumé.png" (outlook::mail-attachment-name (first attachments)))
        (is equalp *png-test-octets* (outlook::mail-attachment-octets (first attachments)))
        (is string= "Forwarded count" (outlook::mail-attachment-name (second attachments)))
        (is string= "Inner text." (string-right-trim '(#\Newline) (outlook::mail-body (outlook::mail-attachment-message (second attachments)))))))
  ;; quoted-printable and base64 on their own
  (is string= "a=b é" (outlook::mail-decode (outlook::mime-quoted-printable-octets (sb-ext:string-to-octets (format nil "a=3Db =~%=C3=A9") :external-format :latin-1)) "utf-8"))
  (is string= "Man" (tuatara.text:octets-latin-1 (tuatara.io:base64-octets (sb-ext:string-to-octets (format nil "TW~%Fu") :external-format :latin-1))))
  (is string= "Ma" (tuatara.text:octets-latin-1 (tuatara.io:base64-octets (sb-ext:string-to-octets "TWE=" :external-format :latin-1))))
  ;; a boundary that never comes: the body is the rest, and the message says so
  (let ((mail (outlook::eml-read (sb-ext:string-to-octets (format nil "Subject: broken~%Content-Type: multipart/mixed; boundary=\"gone\"~%~%Only text.~%") :external-format :utf-8))))
    (is string= "broken" (outlook::mail-subject mail))
    (true (some (lambda (note) (search "boundary" note)) (outlook::mail-notes mail))))
  ;; a closing line that never comes: the last part runs to the end
  (let ((mail (outlook::eml-read (sb-ext:string-to-octets (format nil "Content-Type: multipart/mixed; boundary=b~%~%--b~%Content-Type: text/plain~%~%cut short~%") :external-format :utf-8))))
    (has (outlook::mail-body mail) "cut short"))
  ;; a date of every kind, or none
  (is = (encode-universal-time 0 0 12 1 1 2026 0) (outlook::mail-date-time "1 Jan 26 12:00 GMT"))
  (false (outlook::mail-date-time "yesterday"))
  ;; headers in charsets of their own, and an unknown one read as the editor reads files
  (is string= "€" (outlook::mime-decode-words "=?windows-1252?Q?=80?="))
  (is string= "plain" (outlook::mime-decode-words "plain")))

;;; Seen and opened in the editor

(defun outlook-busy-p ()
  "True while a message or a mailbox is being read, or a message drawn as it looks."
  (or (some (lambda (job) (or (outlook::mail-read-job-p job) (outlook::mailbox-job-p job) (outlook::mail-pages-job-p job)))
            tuatara.runtime:*jobs*)
      (some (lambda (b) (let ((view (tuatara.defaults::pdf-view-of b)))
                          (and view (or (tuatara.defaults::pdf-view-job view) (tuatara.defaults::pdf-view-text-job view)))))
            tuatara.defaults::*pdf-views*)))

(defun outlook-session (path steps &key (width 120) (height 40))
  "Visit PATH with C-x C-f in an editor session, then do each of STEPS (keys,
or a function called there) once what is read in the background is done.
Returns the frontend and the editor."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height))
        (start (get-internal-real-time)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "")
    (tuatara.frontend.fake:fake-queue-keys f (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed path)))
    (labels ((check ()
               (if (and (outlook-busy-p) (< (- (get-internal-real-time) start) (* 30 internal-time-units-per-second)))
                   (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f #'check))
                   (when steps
                     (let ((step (pop steps)))
                       (if (functionp step)
                           (tuatara.frontend.fake:fake-queue-action f step)
                           (tuatara.frontend.fake:fake-queue-keys f step)))
                     (tuatara.frontend.fake:fake-queue-action f #'check)))))
      (tuatara.frontend.fake:fake-queue-action f #'check))
    (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                            :buffer (tuatara.buffer:find-buffer "*scratch*")))))

(defun to-attachment (n)
  "A step for OUTLOOK-SESSION: point to the Nth attachment's button of the message shown."
  (lambda ()
    (let ((place (nth n (tuatara.buffer:buffer-local (tuatara.command:current-buffer) 'outlook::mail-attachment-places))))
      (when place (setf (tuatara.buffer:buffer-point (tuatara.command:current-buffer)) (first place))))))

(define-test (outlook item-opened-attachments-safely)
  (with-test-directory (dir)
    (let* ((path (write-test-octets (merge-pathnames "survey.msg" dir) (msg-test-octets)))
           (before (tuatara.io:file-octets path))
           (stamp (tuatara.io:path-stamp path)))
      ;; visited: a message, read only
      (multiple-value-bind (f editor) (outlook-session path '())
        (declare (ignore f))
        (let ((buffer (session-buffer editor)))
          (is string= "survey.msg" (tuatara.buffer:buffer-name buffer))
            (true (tuatara.command::buffer-mode-named-p buffer 'outlook::mail-view-mode))
          (true (tuatara.buffer:buffer-local buffer :read-only))
          (has (session-text editor) "Kea survey results")
          (tuatara.buffer:kill-buffer buffer)))
      ;; RET on the picture: its private copy shown as rimu shows it, a picture drawn in the sandbox
      (when (and (eq :ready (tuatara.io:sandbox-status)) (tuatara.defaults::find-image-decoder))
        (multiple-value-bind (f editor) (outlook-session path (list (to-attachment 0) "RET"))
          (declare (ignore f))
          (let* ((view (tuatara.defaults::image-view-of (session-buffer editor)))
                 (copy (and view (tuatara.defaults::image-view-path view))))
            (true view)
            (is string= "kea.png" (and copy (tuatara.io:native-file-name copy)))
            (tuatara.buffer:kill-buffer (session-buffer editor))
            (when (and copy (search "/opened/" copy)) (tuatara.io:delete-tree (tuatara.io:parent-path copy))))))
      ;; with no view for it, its private copy opened safely; on the item attached, a message
      (multiple-value-bind (f editor) (let ((outlook::*mail-attachment-views* '()))
                                        (outlook-session path (list (to-attachment 0) "RET")))
        (declare (ignore f))
        (let* ((shown (session-buffer editor))
               (copy (tuatara.io:native (tuatara.buffer:buffer-file shown))))
          (true (tuatara.io:safe-visit-p shown))
          (is string= "kea.png" (tuatara.io:native-file-name copy))
          (true (search "/opened/" copy))
          (is = #o400 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp copy))))
          (is equalp *png-test-octets* (tuatara.io:file-octets copy))
          (tuatara.buffer:kill-buffer shown)
          (when (search "/opened/" copy) (tuatara.io:delete-tree (tuatara.io:parent-path copy)))))
      (multiple-value-bind (f editor) (outlook-session path (list (to-attachment 1) "RET"))
        (declare (ignore f))
        (has (session-text editor) "Inner body 1")
          (true (tuatara.command::buffer-mode-named-p (session-buffer editor) 'outlook::mail-view-mode)))
      ;; T: the file itself, as text
      (multiple-value-bind (f editor) (outlook-session path (list "T"))
        (declare (ignore f))
        (is equal (tuatara.io:native path) (tuatara.io:native (tuatara.buffer:buffer-file (session-buffer editor)))))
      (dolist (b (copy-list tuatara.buffer:*buffers*))
          (when (tuatara.command::buffer-mode-named-p b 'outlook::mail-view-mode) (tuatara.buffer:kill-buffer b)))
      ;; the item itself never touched
      (is equalp before (tuatara.io:file-octets path))
      (is = (tuatara.io:stamp-mtime stamp) (tuatara.io:stamp-mtime (tuatara.io:path-stamp path)))))
  ;; names an attachment gives made a name of a file alone
  (is string= "passwd" (tuatara.defaults::attachment-file-name "../../etc/passwd"))
  (is string= "b.exe" (tuatara.defaults::attachment-file-name "a\\b.exe"))
  (is string= "attachment" (tuatara.defaults::attachment-file-name ".."))
  (is string= "attachment" (tuatara.defaults::attachment-file-name nil)))

(define-test (outlook saved-mail-opened)
  (with-test-directory (dir)
    (let ((path (write-test-octets (merge-pathnames "count.eml" dir) (sb-ext:string-to-octets *eml-test-text* :external-format :utf-8))))
      (multiple-value-bind (f editor) (outlook-session path '())
        (declare (ignore f))
        (has (session-text editor) "Café survey")
        (has (session-text editor) "résumé.png")
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      ;; T shows its text, as any file is opened
      (multiple-value-bind (f editor) (outlook-session path (list "T"))
        (declare (ignore f))
        (has (session-text editor) "Content-Transfer-Encoding: quoted-printable")
        (dolist (b (copy-list tuatara.buffer:*buffers*))
            (when (or (tuatara.command::buffer-mode-named-p b 'outlook::mail-view-mode) (eq b (session-buffer editor)))
            (tuatara.buffer:kill-buffer b)))))))

;;; As it looks

(define-test (outlook html-made-ready)
  ;; a picture shown by its Content-ID written beside the HTML and pointed to; one of no attachment left as it is
  (with-test-directory (dir)
    (let* ((folder (namestring dir))
           (picture (outlook::make-mail-attachment :name "kea.png" :octets *png-test-octets* :content-id "<kea%401@example.org>"))
           (other (outlook::make-mail-attachment :name "notes.txt" :octets (tuatara.text:string-octets "notes")))
           (html "<img src=\"cid:kea@1@example.org\"><img src='CID:kea%401@example.org'><img src=\"cid:gone\"><img src=\"https://example.org/track.png\"><div style=\"background:url( 'http://example.org/b.gif')\"></div><link rel=stylesheet href=\"//example.org/s.css\"><a href=\"https://example.org/\">a link</a>"))
      (multiple-value-bind (ready fetching) (outlook::mail-page-html html (list other picture) folder)
        (has ready "<img src=\"part-1.png\"><img src='part-1.png'><img src=\"cid:gone\">")
        ;; the picture, the background and the style would be fetched; a link is never followed
        (is = 3 fetching)
        (is equal '("part-1.png") (mapcar #'first (tuatara.io:directory-entries folder)))
        (is equalp *png-test-octets* (tuatara.io:file-octets (merge-pathnames "part-1.png" dir))))
      ;; a name an attachment gives is never used but for a plain ending
      (let ((odd (outlook::make-mail-attachment :name "x.p/n g" :octets *png-test-octets* :content-id "odd")))
        (has (outlook::mail-page-html "<img src=cid:odd>" (list odd) folder) "src=part-1>")))))

(define-test (outlook page-head)
  ;; the headers drawn at the start of the body, written as text, never as markup
  (let ((mail (outlook::make-mail :from "Kea <kea@example.org>" :subject "Tui & </td><script>x</script>"
                                  :attachments (list (outlook::make-mail-attachment :name "kea.png")
                                                     (outlook::make-mail-attachment :name "logo.png" :hidden t)))))
    (flet ((head-at (html) (search "<table" (outlook::mail-page-with-headers html mail))))
      (is = 20 (head-at "<html><body class=a><p>x</p></body></html>"))
      (is = 19 (head-at "<html><head></head><p>x</p></html>"))
      (is = 21 (head-at "<!DOCTYPE html><html><p>x</p></html>"))
      (is = 0 (head-at "<p>x</p>")))
    (let ((head (outlook::mail-page-head mail)))
      (has head "Kea &lt;kea@example.org&gt;")
      (has head "Tui &amp; &lt;/td&gt;&lt;script&gt;")
      (false (search "<script" head))
      (has head "kea.png")
      (false (search "logo.png" head)))
    ;; the internet headers on a page of their own, before the last end of the body
    (false (outlook::mail-page-foot mail))
    (setf (outlook::mail-headers mail) (format nil "Received: from <mx>~c~cX-Note: a & b~c~c" #\Return #\Newline #\Return #\Newline))
    (let ((foot (outlook::mail-page-foot mail)))
      (has foot "Received: from &lt;mx&gt;
X-Note: a &amp; b</pre>")
      (flet ((foot-before (html tail) (let ((all (outlook::mail-page-with-headers html mail)))
                                        (is = (- (length all) (length tail)) (+ (search foot all) (length foot))))))
        (foot-before "<html><body><p>x</p></body><!-- </body> --></BODY></html>" "</BODY></html>")
        (foot-before "<html><p>x</p></html>" "</html>")
        (foot-before "<p>x</p>" "")
        ;; an end of the body before its start: both after the start
        (foot-before "</body><body><p>x</p>" "<p>x</p>")))))

(defun mail-pages-drawn-p ()
  "True when messages can be drawn as they look here: the sandbox ready,
WeasyPrint installed in it and a program drawing PDFs."
  (and (eq :ready (tuatara.io:sandbox-status))
       (tuatara.io:sandbox-find-program outlook::*outlook-page-program*)
       (tuatara.defaults::find-pdf-decoder)))

(define-test (outlook item-drawn-as-it-looks)
  (with-test-directory (dir)
    (let* ((html (write-test-octets (merge-pathnames "survey.msg" dir) (msg-test-octets :body :html)))
           (plain (write-test-octets (merge-pathnames "plain.msg" dir) (msg-test-octets)))
           (before (tuatara.io:file-octets html)))
      ;; plain text alone looks as it is shown
      (multiple-value-bind (f editor) (outlook-session plain (list "P"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Its text is plain"))
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      ;; where WeasyPrint is not, it says what to install
      (let ((outlook::*outlook-page-program* "no-such-weasyprint"))
        (multiple-value-bind (f editor) (outlook-session html (list "P"))
          (true (tuatara.frontend.fake:fake-frame-showing f "no-such-weasyprint, which is not installed"))
          (tuatara.buffer:kill-buffer (session-buffer editor))))
      (if (not (mail-pages-drawn-p))
          (skip "WeasyPrint, the sandbox or a program for PDFs is not here" (true t))
          ;; P: its HTML drawn as pages, shut in, the script not run and nothing fetched; T back to the message
          (let ((pages nil) (back nil))
            (multiple-value-bind (f editor)
                (outlook-session html (list "P" (lambda () (setf pages (tuatara:current-buffer))) "T"
                                            (lambda () (setf back (tuatara:current-buffer)))))
              (declare (ignore f))
              (is string= "survey.msg (as it looks)" (tuatara.buffer:buffer-name pages))
              (true (tuatara.command::buffer-mode-named-p pages 'tuatara.defaults::pdf-view-mode))
              (let ((text (tuatara.buffer:buffer-string pages)))
                (has text "Kea & kaka seen")
                ;; its headers above it, as a mail program prints it
                (has text "Kea survey results")
                (has text "Ruru Watcher")
                (has text "X-Origin: tested")
                (false (search "alert" text)))
              (let ((view (tuatara.defaults::pdf-view-of pages)))
                ;; the message, then its internet headers on a page of their own
                (is = 2 (tuatara.defaults::pdf-view-pages view))
                ;; fitted to the window's width, to be read
                (is eq :width (tuatara.defaults::pdf-view-zoom view))
                (has (tuatara.defaults::pdf-view-words view) "1 thing from the internet not fetched"))
              (true (tuatara.command::buffer-mode-named-p back 'outlook::mail-view-mode))
              ;; its HTML written for you alone, beside nothing of yours
              (let ((written (tuatara.defaults::pdf-view-path (tuatara.defaults::pdf-view-of pages))))
                (true (search "/mail-pages/" written))
                (is = #o400 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp written))))
                (tuatara.io:delete-tree (tuatara.io:parent-path written)))
              (dolist (b (list pages (session-buffer editor) back))
                (when (tuatara.buffer:live-buffer-p b) (tuatara.buffer:kill-buffer b))))))
      (is equalp before (tuatara.io:file-octets html)))))

;;; Mailboxes

(defun put-test-file (path text)
  "Write TEXT at PATH, its directories made."
  (ensure-directories-exist path)
  (write-test-octets path (sb-ext:string-to-octets text :external-format :utf-8)))

(define-test (outlook mailbox-listed)
  ;; what readpst writes, made by hand: readpst itself is run only where it is installed
  (with-test-directory (dir)
    (let* ((out (merge-pathnames "out/" dir))
           (pst (write-test-octets (merge-pathnames "archive.pst" dir) (sb-ext:string-to-octets "!BDN    SM" :external-format :latin-1)))
           (buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "archive.pst (mailbox)")))
             (job (outlook::make-mailbox-job buffer pst (namestring out))))
      (put-test-file (merge-pathnames "Outlook Data File/Inbox/1.eml" out)
                     (format nil "From: Kea <kea@example.org>~%Subject: First~%Date: Tue, 6 Oct 2026 09:00:00 +0000~%~%one~%"))
      (put-test-file (merge-pathnames "Outlook Data File/Inbox/2.eml" out) *eml-test-text*)
      (put-test-file (merge-pathnames "Outlook Data File/Inbox/10.eml" out) (format nil "Subject: Tenth~%~%ten~%"))
      (put-test-file (merge-pathnames "Outlook Data File/Contacts/1.vcf" out) (format nil "BEGIN:VCARD~%FN:Kea~%END:VCARD~%"))
      (sb-posix:symlink "/etc/passwd" (namestring (merge-pathnames "Outlook Data File/Inbox/3.eml" out)))
      (is eq :pst (nth-value 1 (tuatara.io:octets-kind (tuatara.io:file-head pst))))
        (outlook::mailbox-list job)
        (let ((folders (outlook::mailbox-job-folders job)))
        (is equal '("Outlook Data File/Contacts" "Outlook Data File/Inbox") (mapcar #'car folders))
        ;; in the order of their numbers, the link passed over
          (is equal '("First" "Café survey" "Tenth") (mapcar #'outlook::mailbox-item-subject (cdr (second folders))))
          (is equal '(0 2 0) (mapcar #'outlook::mailbox-item-attachments (cdr (second folders))))
          (is string= "1.vcf" (outlook::mailbox-item-subject (first (cdr (first folders))))))
        (tuatara.modes:set-major-mode buffer 'outlook::mailbox-mode)
        (outlook::mailbox-render buffer job)
      (let ((text (tuatara.buffer:buffer-string buffer)))
        (has text "Outlook mailbox archive.pst")
        (has text "4 items in 2 folders")
        (has text "▾ Outlook Data File/Inbox  3")
        (has text "2026-10-06")
        (has text "Café survey"))
      ;; each line its item
      (let ((item (tuatara.defaults::entry-member (tuatara.defaults::list-entry-at buffer (tuatara.buffer:buffer-point buffer)))))
          (true (outlook::mailbox-item-p item)))
      (tuatara.buffer:kill-buffer buffer))))

;;; A mailbox's list narrowed by its messages' headers and contents

(defun mailbox-search-test-messages (out)
  "Write into OUT, as readpst would, the messages the search of a mailbox's
list is tried on: a header not shown in the list, bodies quoted-printable and
base64, an attachment's name, and one larger than the search looks through."
  (flet ((put (name text) (put-test-file (merge-pathnames (concatenate 'string "Outlook Data File/Inbox/" name) out) text)))
    (put "1.eml" (format nil "From: Kea <kea@example.org>~%Subject: First~%X-Case-Number: walrus-7~%~%nothing here~%"))
    (put "2.eml" (format nil "From: Ruru <ruru@example.org>~%Subject: Second~%Content-Type: text/plain; charset=utf-8~%~
                              Content-Transfer-Encoding: quoted-printable~%~%Caf=C3=A9 and a gan=~%net seen~%"))
    (put "3.eml" (format nil "From: Tui <tui@example.org>~%Subject: Third~%Content-Type: text/plain~%~
                              Content-Transfer-Encoding: base64~%~%dGhlIGFsYmF0cm9zcyBmbGllcw==~%"))
    (put "4.eml" (format nil "From: Weka <weka@example.org>~%Subject: Fourth~%MIME-Version: 1.0~%~
                              Content-Type: multipart/mixed; boundary=\"b\"~%~%--b~%Content-Type: text/plain~%~%see attached~%~
                              --b~%Content-Type: application/pdf; name=\"invoice-2026.pdf\"~%Content-Transfer-Encoding: base64~%~%~
                              JVBERi0xLjQK~%--b--~%"))
    (put "5.eml" (format nil "From: Hoiho <hoiho@example.org>~%Subject: Fifth~%~%~a petrel~%" (make-string 400 :initial-element #\x)))))

(defun mailbox-search-test-list (dir)
  "The list of the mailbox MAILBOX-SEARCH-TEST-MESSAGES writes in DIR, read
for it and its messages' text written as the listing job writes it, with
*OUTLOOK-SEARCH-TEXT-LIMIT* small enough that the fifth is too large."
  (let* ((out (merge-pathnames "out/" dir))
         (texts (merge-pathnames "text/" dir))
         (pst (write-test-octets (merge-pathnames "archive.pst" dir) (sb-ext:string-to-octets "!BDN    SM" :external-format :latin-1)))
         (buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "archive.pst (mailbox)")))
         (job (outlook::make-mailbox-job buffer pst (namestring out) (namestring texts))))
    (mailbox-search-test-messages out)
    (ensure-directories-exist texts)
    (let ((tuatara-user:*outlook-search-text-limit* 300))
      (outlook::mailbox-list job))
    (tuatara.modes:set-major-mode buffer 'outlook::mailbox-mode)
    (outlook::mailbox-render buffer job)
    buffer))

(defun mailbox-subjects (buffer)
  "The subjects of the messages BUFFER, a mailbox's list, shows."
  (loop for line in (uiop:split-string (tuatara.buffer:buffer-string buffer) :separator '(#\Newline))
        for found = (find-if (lambda (subject) (search subject line)) '("First" "Second" "Third" "Fourth" "Fifth"))
        when (and found (not (search "iltered by" line))) collect found))

(defun wait-for-mailbox-search ()
  "Wait, a few seconds at most, till no list is being filtered in the background."
  (let ((until (+ (get-internal-real-time) (* 10 internal-time-units-per-second))))
    (loop while (and (some (lambda (job) (and (tuatara.defaults::search-filter-job-p job) (tuatara.runtime:job-running-p job)))
                           tuatara.runtime:*jobs*)
                     (< (get-internal-real-time) until))
          do (sleep 0.01))))

(defun mailbox-search-session (list keys)
  "Type KEYS, after showing LIST, a mailbox's list, in an editor session:
:SEARCHED among them waits for the search in the background and lets the
editor take it in, :LOOK keeps the subjects shown there. Returns the editor
and the subjects kept."
  (let ((seen nil))
    (multiple-value-bind (f editor)
        (tuatara.tests::run-keys
         (list* (lambda () (tuatara:show-buffer list))
                (loop for key in keys
                      append (case key
                               (:searched (list #'wait-for-mailbox-search :tick))
                               (:look (list (lambda () (setf seen (mailbox-subjects list)))))
                               (t (list key)))))
         :width 140 :height 30)
      (declare (ignore f))
      (values editor seen))))

(define-test (outlook mailbox-searched)
  (with-test-directory (dir)
    (let ((list (mailbox-search-test-list dir)))
      (is equal '("First" "Second" "Third" "Fourth" "Fifth") (mailbox-subjects list))
      ;; the text written for each message: every header, the text parts decoded, attachments by name
      (let ((texts (loop for entry in (tuatara.buffer:buffer-local list 'tuatara.defaults::list-entries)
                         for item = (tuatara.defaults::entry-member entry)
                         when (outlook::mailbox-item-p item) collect (outlook::mailbox-item-search-text item nil))))
        (has (first texts) "x-case-number: walrus-7")
        (has (second texts) "Café and a gannet seen")
        (has (third texts) "the albatross flies")
        (has (fourth texts) "invoice-2026.pdf")
        (false (search "PDF-1" (fourth texts))))
      (has (tuatara.buffer:buffer-string list) "1 message too large to be searched whole")
      (flet ((narrowed (pattern)
               (nth-value 1 (mailbox-search-session list (list (format nil "C-s ~a" (tuatara.tests::rimu-typed pattern)) :searched :look)))))
        ;; a header the list does not show
        (is equal '("First") (narrowed "walrus-7"))
        ;; quoted-printable, its soft line break joined and its UTF-8 read; base64
        (is equal '("Second") (narrowed "gannet"))
        (is equal '("Third") (narrowed "albatross"))
        ;; an attachment's name, never its contents
        (is equal '("Fourth") (narrowed "invoice-20"))
        (is equal '() (narrowed "PDF-1"))
        ;; past what the search looks through in a message too large
        (is equal '() (narrowed "petrel"))
        ;; headers matched by a regular expression, case ignored
        (is equal '("Second" "Third") (narrowed "^from: (ruru|tui)")))
      ;; C-g brings the whole list back
      (multiple-value-bind (editor seen) (mailbox-search-session list '("C-s w a l r u s" :searched :look "C-g"))
        (declare (ignore editor))
        (is equal '("First") seen)
        (is equal '("First" "Second" "Third" "Fourth" "Fifth") (mailbox-subjects list)))
      ;; kept with RET, a second search narrows what the first left; then all taken away
      (mailbox-search-session list '("C-s e e n RET" :searched "C-s a" :searched "RET"))
      (is equal '("Second") (mailbox-subjects list))
      (is equal '("een" "a") (mapcar #'tuatara:search-filter-string (tuatara:search-filters list)))
      (mailbox-search-session list '("C-x / r"))
      (is equal '("First" "Second" "Third" "Fourth" "Fifth") (mailbox-subjects list))
      ;; RET opens the message on the line of the narrowed list, the filter's matches lit in it
      (multiple-value-bind (editor) (mailbox-search-session
                                     list (list "C-s a l b a" :searched "RET"
                                                (lambda ()
                                                  (setf (tuatara.buffer:buffer-point list)
                                                        (tuatara.buffer:buffer-goto-line
                                                         list (position-if (lambda (line) (search "Third" line))
                                                                           (uiop:split-string (tuatara.buffer:buffer-string list)
                                                                                              :separator '(#\Newline))))))
                                                "RET"))
        (let ((shown (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (is string= "Third" (tuatara.buffer:buffer-name shown))
          (is eq shown (cdr tuatara.defaults::*search-lit*))
          (setf tuatara.defaults::*search-lit* nil tuatara.defaults::*search-lit-shown* nil)
          (tuatara.buffer:kill-buffer shown)))
      (tuatara.buffer:kill-buffer list))))

;;; Marked, folded, opened and copied out

(define-test (outlook mailbox-marked-while-narrowed)
  ;; a mark given or a folder folded in a narrowed list keeps it narrowed, nothing searched again
  (with-test-directory (dir)
    (let ((list (mailbox-search-test-list dir)) (after-mark nil) (after-fold nil) (jobs nil))
      (tuatara.tests::run-keys
       (list (lambda () (tuatara:show-buffer list))
             "C-s g a n n e t | a l b a t r o s s RET" #'wait-for-mailbox-search :tick
             "M-< C-n C-n C-n C-n C-n C-n C-n C-n m"
             (lambda () (setf after-mark (tuatara.buffer:buffer-string list)
                              jobs (count-if #'tuatara.defaults::search-filter-job-p tuatara.runtime:*jobs*)))
             "M-< C-n C-n C-n C-n C-n C-n C-n RET" (lambda () (setf after-fold (tuatara.buffer:buffer-string list))))
       :width 140 :height 30)
      (has after-mark "Filtered by gannet|albatross: 2 of 5 items")
      (is = 0 jobs)
      (is equal '("Second" "Third") (let ((seen '()))
                                      (dolist (s '("First" "Second" "Third" "Fourth" "Fifth") (nreverse seen))
                                        (when (search s after-mark) (push s seen)))))
      (true (search "* " after-mark))
      (has after-fold "Filtered by gannet|albatross")
      (has after-fold "▸ Outlook Data File/Inbox")
      (false (search "Second" after-fold))
      (tuatara.buffer:kill-buffer list))))

(defun wait-for-mailbox-export ()
  "Wait, a few seconds at most, till no messages are being copied out of a mailbox."
  (let ((until (+ (get-internal-real-time) (* 10 internal-time-units-per-second))))
    (loop while (and (some (lambda (job) (and (outlook::mailbox-export-job-p job) (tuatara.runtime:job-running-p job)))
                           tuatara.runtime:*jobs*)
                     (< (get-internal-real-time) until))
          do (sleep 0.01))))

(define-test (outlook mailbox-marked-and-copied-out)
  (with-test-directory (dir)
    (let* ((out (merge-pathnames "out/" dir))
           (pst (write-test-octets (merge-pathnames "archive.pst" dir) (sb-ext:string-to-octets "!BDN    SM" :external-format :latin-1)))
           (list (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "archive.pst (mailbox)")))
           (job (outlook::make-mailbox-job list pst (namestring out)))
           (sent (encode-universal-time 0 0 9 6 10 2026 0)))
      (flet ((put (name text) (put-test-file (merge-pathnames (concatenate 'string "Outlook Data File/" name) out) text))
             (copied (folder) (sort (mapcar (lambda (p) (tuatara.io:native-file-name (namestring p)))
                                            (directory (merge-pathnames "*.*" folder)))
                                    #'string<))
             (text-of (folder name) (uiop:read-file-string (merge-pathnames name folder))))
        ;; readpst -m writes each message as .eml and .msg, and leaves temp_file_attachment behind
        (put "Inbox/1.eml" (format nil "From: Kea <kea@example.org>~%Subject: Re: a/b?~%Date: Tue, 6 Oct 2026 09:00:00 +0000~%~%one~%"))
        (put "Inbox/1.msg" "the first as an Outlook item")
        (put "Inbox/2.eml" (format nil "Subject: Second~%~%two~%"))
        (put "Inbox/temp_file_attachment" "left over")
        (put "Sent Items/1.eml" (format nil "Subject: Third~%~%three~%"))
        (put "Sent Items/1.msg" "the third as an Outlook item")
        (outlook::mailbox-list job)
        (let* ((inbox (cdr (assoc "Outlook Data File/Inbox" (outlook::mailbox-job-folders job) :test #'string=)))
               (first (first inbox)) (second (second inbox)))
          (is equal '("Re: a/b?" "Second") (mapcar #'outlook::mailbox-item-subject inbox))
          (true (search "Inbox/1.msg" (outlook::mailbox-item-msg first)))
          (false (outlook::mailbox-item-msg second))
          (is = 3 (reduce #'+ (outlook::mailbox-job-folders job) :key (lambda (f) (length (cdr f)))))
          (tuatara.modes:set-major-mode list 'outlook::mailbox-mode)
          (setf (tuatara.buffer:buffer-local list 'outlook::mailbox-path) pst)
          (outlook::mailbox-render list job)
          (let ((shown '()) (opened nil) (moved nil) (back nil) (marked nil) (stayed nil)
                (copies (namestring (merge-pathnames "copies/" dir))))
            (tuatara.tests::run-keys
             (list (lambda () (tuatara:show-buffer list))
                   ;; h asks before it goes back to the folder: no, and the list stays
                   "h n RET" (lambda () (setf stayed (tuatara:current-buffer)))
                   ;; m marks the message on the line and moves down; RET and l on a folder's line fold it
                   "m" (lambda () (setf marked (tuatara.defaults::list-marked list :marked)))
                   "k k RET" (lambda () (push (tuatara.buffer:buffer-string list) shown))
                   "l" (lambda () (push (tuatara.buffer:buffer-string list) shown))
                   ;; l opens a message, q comes back to the list, h only moving the cursor
                   "j l" (lambda () (setf opened (tuatara.buffer:buffer-name (tuatara:current-buffer))))
                   "h" (lambda () (setf moved (tuatara:current-buffer)))
                   "q" (lambda () (setf back (tuatara:current-buffer)))
                   ;; C copies the one marked out as asked, into a folder made for it
                   (format nil "C m s g RET C-a C-k ~a C-j" (tuatara.tests::rimu-typed copies))
                   #'wait-for-mailbox-export :tick)
             :width 140 :height 30)
            (is equal (list first) marked)
            (true (search "* 2026-10-06" (tuatara.buffer:buffer-string list)))
            ;; the line over the list says the keys, those of a message opened from it too
            ;; moves by word and selections but V's do nothing in the list
            (dolist (keys '("w" "b" "W" "B" "v" "%"))
              (is eq :undefined (tuatara.command:lookup-key (tuatara.modes:mode-keymap 'outlook::mailbox-mode)
                                                            (tuatara.frontend:parse-key-sequence keys))))
            (false (tuatara.command:lookup-key (tuatara.modes:mode-keymap 'outlook::mailbox-mode) (tuatara.frontend:parse-key-sequence "V")))
            (is eq list stayed)
            (has (tuatara.buffer:buffer-string list) "RET or l opens the item on the line, a folder's line folds; P in a message shows it as it looks, q comes back;")
            (has (first (rest shown)) "▸ Outlook Data File/Inbox  2")
            (false (search "Second" (first (rest shown))))
            (has (first shown) "Second")
            (is string= "Re: a/b?" opened)
            (is string= "Re: a/b?" (tuatara.buffer:buffer-name moved))
            (is eq list back)
            (let ((name (format nil "~a Re a-b.msg" (tuatara:format-time "%F %H%M" sent))))
              (is equal (list name) (copied copies))
              (is string= "the first as an Outlook item" (text-of copies name))))
          ;; as saved mail, a name taken getting (2); one with no Outlook item written is said
          (let ((copies (namestring (merge-pathnames "more/" dir))))
            (ensure-directories-exist copies)
            (dolist (type '("eml" "eml" "msg"))
              (outlook::mailbox-export-work (outlook::make-mailbox-export-job list (list first second) copies type)))
            (let ((stamp (tuatara:format-time "%F %H%M" sent)))
              (is equal (sort (list (format nil "~a Re a-b.eml" stamp) (format nil "~a Re a-b (2).eml" stamp)
                                    (format nil "~a Re a-b.msg" stamp) "Second.eml" "Second (2).eml")
                              #'string<)
                  (copied copies)))
            (let ((job (outlook::make-mailbox-export-job list (list second) copies "msg")))
              (outlook::mailbox-export-work job)
              (is = 0 (outlook::mailbox-export-job-copied job))
              (has (cdr (first (outlook::mailbox-export-job-not-copied job))) "no Outlook item"))))
        ;; h, answered yes: the list closed, its folder listed by rimu, point on the mailbox
        (let ((folder nil))
          (tuatara.tests::run-keys
           (list (lambda () (tuatara:show-buffer list))
                 "h y RET" (lambda () (setf folder (tuatara:current-buffer))))
           :width 140 :height 30)
          (false (tuatara.buffer:live-buffer-p list))
          (true (tuatara.defaults::rimu-list-p folder))
          (is string= "archive.pst" (tuatara.io:native-file-name (tuatara.defaults::shown-file folder)))
          (tuatara.buffer:kill-buffer folder))
        (when (tuatara.buffer:live-buffer-p list) (tuatara.buffer:kill-buffer list))))))

(define-test (outlook mailbox-without-readpst)
  (with-test-directory (dir)
    (let ((pst (write-test-octets (merge-pathnames "archive.pst" dir)
                                  (concatenate '(vector (unsigned-byte 8)) (sb-ext:string-to-octets "!BDN    SM" :external-format :latin-1)
                                               (make-array 600 :initial-element 0)))))
      (let ((tuatara-user:*outlook-readpst-program* "no-such-readpst"))
        ;; opened as any file, saying how to get readpst
        (multiple-value-bind (f editor) (outlook-session pst '())
          (true (tuatara.frontend.fake:fake-frame-showing f "libpst"))
          (is string= "archive.pst" (tuatara.buffer:buffer-name (session-buffer editor)))
          (tuatara.buffer:kill-buffer (session-buffer editor)))
        ;; started with it: opened as C-x C-f opens it, in the hex view, saying the same
        (let ((f (tuatara.frontend.fake:make-fake-frontend :width 400 :height 20)))
          (tuatara.command:run-editor f :files (list (namestring pst)) :setup #'tuatara.defaults:install-defaults)
          (true (tuatara.frontend.fake:fake-frame-showing f "Hex 0x00000000"))
          (true (tuatara.frontend.fake:fake-frame-showing f "libpst"))
          (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "archive.pst")))
          (has (outlook::outlook-health-words) "libpst")
          (has (outlook::mailbox-preview-words "archive.pst" 2048) "not installed"))
        (if (not (outlook::mailbox-readpst))
          (skip "readpst is not installed here" (true nil))
          (progn
              (has (outlook::outlook-health-words) "shut in")
            ;; readpst reads a copy: the mailbox's access time kept, the copy gone after
            (sb-posix:utimes (namestring pst) 946681200 946681200)
            (multiple-value-bind (f editor) (outlook-session pst '())
              (declare (ignore f))
              (tuatara.buffer:kill-buffer (session-buffer editor)))
            (is = 946681200 (sb-posix:stat-atime (sb-posix:stat (namestring pst))))
            (false (directory (merge-pathnames "*.pst" (tuatara.defaults::private-cache-directory "mailboxes/")))))))))

;;; Previews, loading

(define-test (outlook previews-and-loading)
  (with-test-directory (dir)
    (let ((path (write-test-octets (merge-pathnames "survey.msg" dir) (msg-test-octets))))
      (tuatara.io:call-with-file path
                                 (lambda (fd name size)
                                     (let ((text (outlook::outlook-preview-text fd name size :ole (tuatara.io:read-fd-start fd name size 4096))))
                                     (has text "Kea survey results")
                                     (has text "kea.png")
                                     (has text "Twelve kea"))))))
  ;; a path is no package's name
  (fail (tuatara-user:load-package "../outlook"))
  ;; the package's views and preview in place
  (true (member 'outlook::mail-visit tuatara.defaults::*file-views*))
  (true (member 'outlook::outlook-preview-text tuatara.defaults::*preview-text-functions*))
  (true (member 'outlook::outlook-health-words tuatara.defaults::*health-decoder-functions*)))

(define-test (outlook shared-and-deep)
  ;; an entry two storages name belongs to the first alone, and streams coming to more than twice the file
  ;; are refused, so that one stream is never copied for each storage that names it
    (let* ((entries (vector (outlook::make-cfb-entry :name "Root" :type :root :child 1 :index 0)
                            (outlook::make-cfb-entry :name "a" :type :storage :right 2 :child 3 :index 1)
                            (outlook::make-cfb-entry :name "b" :type :storage :child 3 :index 2)
                            (outlook::make-cfb-entry :name "shared" :type :stream :start 0 :size 60 :index 3)))
           (cfb (outlook::%make-cfb :octets (make-array 1024 :element-type '(unsigned-byte 8)) :sector-size 512 :sectors 1
                                           :entries entries :cutoff 4096
                                           :mini-stream (make-array 4096 :element-type '(unsigned-byte 8))
                                           :mini-fat (make-array 64 :element-type '(unsigned-byte 32) :initial-element #xFFFFFFFE))))
      (is equal '("shared") (mapcar #'outlook::cfb-entry-name (outlook::cfb-children cfb (aref entries 1))))
      (false (outlook::cfb-children cfb (aref entries 2)))
      (setf (outlook::cfb-entry-size (aref entries 3)) 60)
      (is = 60 (length (outlook::cfb-stream-octets cfb (aref entries 3))))
      (setf (outlook::cfb-given cfb) 2000)
      (fail (outlook::cfb-stream-octets cfb (aref entries 3))))
  ;; RTF nested a hundred thousand deep: read at once, not in time growing with the square of it
  (let ((start (get-internal-real-time)))
    (outlook::rtf-text (make-array 200000 :element-type '(unsigned-byte 8) :initial-element 123))
    (true (< (- (get-internal-real-time) start) (* 2 internal-time-units-per-second)))))
