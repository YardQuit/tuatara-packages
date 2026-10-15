;;;; outlook/msg.lisp --- Outlook items (.msg) read: their properties, recipients and attachments.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; An Outlook item is a compound file (cfb.lisp) laid out as Microsoft's
;;;; MS-OXMSG says: the item's properties, each a number and a type, those of
;;;; a fixed size in one stream (__properties_version1.0) and each of the
;;;; others in a stream of its own (__substg1.0_NNNNTTTT); a storage for each
;;;; recipient and each attachment, laid out alike; an attachment that is an
;;;; item itself, a storage within, read as one to a depth. Named properties
;;;; (those numbered from 0x8000) are left out.

(in-package #:outlook)

(defun msg-properties (cfb entry header)
  "The properties of the storage ENTRY of CFB, an item, a recipient or an
attachment, whose fixed-size properties' stream has HEADER octets before
them: a hash table from each property's number to (TYPE . VALUE), VALUE a
number, a universal time, T or NIL, octets, a string, or (:storage . ENTRY)."
  (let ((table (make-hash-table)) (children (cfb-children cfb entry)))
    (dolist (child children)
      (let ((name (cfb-entry-name child)))
        (cond
          ((and (eq (cfb-entry-type child) :stream) (string= name "__properties_version1.0"))
           (let ((octets (cfb-stream-octets cfb child)))
             (loop for at from header by 16
                   while (<= (+ at 16) (length octets))
                   do (let* ((tag (cfb-number octets at 4)) (type (ldb (byte 16 0) tag)) (id (ldb (byte 16 16) tag))
                             (value (case type
                                      (#x0002 (octets-signed (cfb-number octets (+ at 8) 2) 16))
                                      (#x0003 (octets-signed (cfb-number octets (+ at 8) 4) 32))
                                      (#x000B (/= 0 (cfb-number octets (+ at 8) 2)))
                                      (#x0014 (cfb-number octets (+ at 8) 8))
                                      (#x0040 (filetime-universal-time (cfb-number octets (+ at 8) 8)))
                                      (t :none))))
                        (unless (eq value :none)
                          (setf (gethash id table) (cons type value)))))))
          ((and (>= (length name) 20) (string= "__substg1.0_" name :end2 12))
           (let ((id (ignore-errors (parse-integer name :start 12 :end 16 :radix 16)))
                 (type (ignore-errors (parse-integer name :start 16 :end 20 :radix 16))))
             ;; multiple values (their own streams, -NNNNNNNN after) are left out
             (when (and id type (= (length name) 20) (< id #x8000))
               (let ((old (car (gethash id table))))
                 (case type
                   ;; a string of Unicode before one of the item's code page
                   ((#x001F #x001E #x0102)
                    (unless (and (eql old #x001F) (/= type #x001F))
                      (when (eq (cfb-entry-type child) :stream)
                        (let ((octets (cfb-stream-octets cfb child)))
                          (setf (gethash id table)
                                (cons type (if (= type #x001F) (utf-16-text octets) octets)))))))
                   (#x000D (setf (gethash id table) (cons type (cons :storage child))))))))))))
    table))

(defun msg-value (properties id)
  "The value of the property numbered ID in PROPERTIES (MSG-PROPERTIES), or NIL."
  (cdr (gethash id properties)))

(defun msg-string (properties id codepage)
  "The property numbered ID in PROPERTIES as text: a string as it is, one in
the item's code page (CODEPAGE, a number) decoded, a number written; NIL for
none or an empty one."
  (let* ((entry (gethash id properties))
         (value (cond ((null entry) nil)
                      ((stringp (cdr entry)) (cdr entry))
                      ((and (= (car entry) #x001E) (vectorp (cdr entry)))
                       (let ((text (charset-text (cdr entry) (or (mail-codepage-charset codepage) "windows-1252"))))
                         (subseq text 0 (or (position (code-char 0) text) (length text)))))
                      ((integerp (cdr entry)) (princ-to-string (cdr entry))))))
    (and value (plusp (length value)) value)))

(defun msg-children-named (cfb entry prefix)
  "The storages in ENTRY of CFB whose names begin with PREFIX, in the order
of their names."
  (sort (remove-if-not (lambda (child) (and (eq (cfb-entry-type child) :storage)
                                            (uiop:string-prefix-p prefix (cfb-entry-name child))))
                       (cfb-children cfb entry))
        #'string< :key #'cfb-entry-name))

(defparameter *msg-flags*
  '((#x1 "read" "not read") (#x8 "unsent (a draft)") (#x4 "being sent") (#x10 "with attachments")
    (#x20 "from the mailbox's own user") (#x40 "hidden, kept by a folder for itself") (#x80 "to be sent again")
    (#x2 "not changed since it was received"))
  "The message flags (PidTagMessageFlags) said in words, each (BIT WORDS
WORDS-WHEN-NOT-SET).")

(defun msg-flag-words (flags)
  "The message flags FLAGS, a number, in words."
  (and (integerp flags)
       (format nil "~{~a~^, ~}"
               (loop for (bit set unset) in *msg-flags*
                     for words = (if (logtest flags bit) set unset)
                     when words collect words))))

(defparameter *msg-item-limit* 1000
  "The most items one Outlook item is read as, itself and those attached to
it at every depth; those past it are listed as not read.")

(defvar *msg-items* nil
  "While an Outlook item is read, the storages read as items, a list.")

(defun msg-sender (mail properties codepage)
  "Set who MAIL is from by the item's PROPERTIES, text in code page CODEPAGE:
whom it was sent for, else who sent it; and note the item's kind, its flags
and the sender as the item gives it."
  (flet ((text (id) (msg-string properties id codepage))
         (value (id) (msg-value properties id)))
    (let ((sender (mail-person (text #x0C1A) (or (text #x5D01) (text #x0C1F)) (and (not (text #x5D01)) (text #x0C1E))))
          (for (mail-person (text #x0042) (or (text #x5D02) (text #x0065)) (and (not (text #x5D02)) (text #x0064)))))
      (setf (mail-from mail) (or for sender))
      (mail-detail mail "Item" (text #x001A))
      (mail-detail mail "Flags" (msg-flag-words (value #x0E07)))
      (when (and sender for (string/= sender for))
        (mail-detail mail "Sent by" (format nil "~a, for ~a" sender for)))
      (mail-detail mail "Sender as the item says" (mail-person (text #x0C1A) (text #x0C1F)))
      (mail-detail mail "Sender address type" (text #x0C1E)))))

(defun msg-recipients (mail cfb entry properties codepage)
  "Set whom MAIL is to, copied to and blind copied to, by the recipients'
storages in the item ENTRY of CFB, text in code page CODEPAGE; the item's
own lists in PROPERTIES where it has no recipients of a kind."
  (let ((to '()) (cc '()) (bcc '()))
    (dolist (recipient (msg-children-named cfb entry "__recip_version1.0_#"))
      (let* ((p (msg-properties cfb recipient 8))
             (person (mail-person (msg-string p #x3001 codepage)
                                  (or (msg-string p #x39FE codepage) (msg-string p #x3003 codepage))
                                  (and (not (msg-string p #x39FE codepage)) (msg-string p #x3002 codepage)))))
        (when person
          (case (msg-value p #x0C15)
            (2 (push person cc)) (3 (push person bcc)) (t (push person to))))))
    (flet ((joined (people display) (if people (format nil "~{~a~^, ~}" (reverse people)) display)))
      (setf (mail-to mail) (joined to (msg-string properties #x0E04 codepage))
            (mail-cc mail) (joined cc (msg-string properties #x0E03 codepage))
            (mail-bcc mail) (joined bcc (msg-string properties #x0E02 codepage))))))

(defun msg-body (mail properties codepage)
  "Set MAIL's body by the item's PROPERTIES, text in code page CODEPAGE: the
plain text, else the HTML, else the compressed RTF, each read as text; its
HTML, the item's own or the HTML its RTF keeps; and note which of them the
item holds."
  (let* ((plain (msg-string properties #x1000 codepage))
         (html (gethash #x1013 properties))
         (rtf (msg-value properties #x1009))
         (held (remove nil (list (and plain "plain text") (and html "HTML") (and rtf "RTF")))))
    (flet ((html-source ()
             (if (stringp (cdr html))
                 (cdr html)
                 (let* ((octets (cdr html))
                        (head (octets-latin-1 octets :end (min 2048 (length octets))))
                        (charset (multiple-value-bind (m g) (cl-ppcre:scan-to-strings "(?i)charset\\s*=\\s*[\"']?([-\\w:.]+)" head)
                                   (and m (aref g 0)))))
                   (charset-text octets (or charset (mail-codepage-charset (msg-value properties #x3FDE)) "utf-8"))))))
      (handler-case
          (cond (plain (setf (mail-body mail) plain (mail-body-kind mail) :plain))
                (html (setf (mail-body mail) (html-text (html-source)) (mail-body-kind mail) :html))
                ((vectorp rtf) (setf (mail-body mail) (rtf-text (lzfu-decompress rtf)) (mail-body-kind mail) :rtf)))
        (error (e) (mail-note mail "Its body could not be read: ~a" e)))
      (setf (mail-html mail) (cond (html #'html-source)
                                   ((vectorp rtf) (lambda () (nth-value 1 (rtf-text (lzfu-decompress rtf))))))))
    (mail-detail mail "Bodies it holds" (and held (format nil "~{~a~^, ~}" held)))))

(defun msg-attachments (mail cfb entry codepage depth)
  "Set MAIL's attachments by the attachments' storages in the item ENTRY of
CFB, text in code page CODEPAGE, the item being DEPTH deep: each one's name,
type and contents, an attached item read as one while DEPTH is below
*OUTLOOK-NESTING-LIMIT*; one that cannot be read is noted."
  (dolist (storage (msg-children-named cfb entry "__attach_version1.0_#"))
    (handler-case
        (let* ((p (msg-properties cfb storage 8))
               (data (gethash #x3701 p))
               (name (or (msg-string p #x3707 codepage) (msg-string p #x3704 codepage) (msg-string p #x3001 codepage)))
               (attachment (make-mail-attachment :name name :mime (msg-string p #x370E codepage)
                                                 :hidden (msg-value p #x7FFE) :content-id (msg-string p #x3712 codepage))))
          (cond ((null data) (setf (mail-attachment-why attachment) "no contents"))
                ((vectorp (cdr data)) (setf (mail-attachment-octets attachment) (cdr data)))
                ((and (consp (cdr data)) (eql (msg-value p #x3705) 5))
                 ;; an item attached: read as one, to a depth, never one being read already
                 (if (and (< depth *outlook-nesting-limit*) (< (length *msg-items*) *msg-item-limit*)
                          (not (member (cdr (cdr data)) *msg-items*)))
                     (let ((inner (msg-mail cfb (cdr (cdr data)) :depth (1+ depth) :header 24)))
                       (setf (mail-attachment-message attachment) inner)
                       (unless name (setf (mail-attachment-name attachment) (or (mail-subject inner) "an attached item"))))
                     (setf (mail-attachment-why attachment) "an item nested too deep, or too often, to read (*outlook-nesting-limit*)")))
                (t (setf (mail-attachment-why attachment) "an embedded object, not read")))
          (unless (mail-attachment-name attachment)
            (setf (mail-attachment-name attachment) (format nil "attachment ~a" (subseq (cfb-entry-name storage) 20))))
          (push attachment (mail-attachments mail)))
      (error (e) (mail-note mail "An attachment could not be read: ~a" e))))
  (setf (mail-attachments mail) (nreverse (mail-attachments mail))))

(defun msg-mail (cfb entry &key (depth 0) (header 32))
  "The MAIL the item in the storage ENTRY of CFB is (the root for an item of
its own, HEADER 32; an attached one, HEADER 24): its sender, recipients,
dates and body (its plain text, else its HTML read as text, else its
compressed RTF read as text), what it says of itself, its internet headers,
and its attachments, an attached item read as one when DEPTH is below
*OUTLOOK-NESTING-LIMIT*."
  (push entry *msg-items*)
  (let* ((properties (msg-properties cfb entry header))
         (codepage (or (msg-value properties #x3FFD) (msg-value properties #x3FDE)))
         (mail (make-mail)))
    (flet ((text (id) (msg-string properties id codepage))
           (value (id) (msg-value properties id)))
      (msg-sender mail properties codepage)
      (msg-recipients mail cfb entry properties codepage)
      (setf (mail-subject mail) (text #x0037)
            (mail-sent mail) (value #x0039)
            (mail-received mail) (value #x0E06)
            (mail-message-id mail) (text #x1035)
            (mail-headers mail) (let ((h (text #x007D))) (and h (string-right-trim '(#\Return #\Newline #\Nul) h))))
      (mail-detail mail "Created" (time-words (value #x3007)))
      (mail-detail mail "Last changed" (time-words (value #x3008)))
      (mail-detail mail "Last changed by" (text #x3FFA))
      (mail-detail mail "In reply to" (text #x1042))
      (mail-detail mail "Conversation" (text #x0070))
      (mail-detail mail "Importance" (case (value #x0017) (0 "low") (2 "high")))
      (mail-detail mail "Sensitivity" (case (value #x0036) (1 "personal") (2 "private") (3 "confidential")))
      (mail-detail mail "Code page" (and codepage (format nil "~d~@[ (~a)~]" codepage (mail-codepage-charset codepage))))
      (msg-body mail properties codepage)
      (msg-attachments mail cfb entry codepage depth)
      mail)))

(defun msg-read (octets)
  "The MAIL the Outlook item in OCTETS (a .msg) is; an error saying why when
it is not one, or is made to be read without end."
  (let ((cfb (cfb-read octets)) (*msg-items* '()))
    (msg-mail cfb (aref (cfb-entries cfb) 0))))
