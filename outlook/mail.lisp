;;;; outlook/mail.lisp --- A mail item as the outlook package reads it: its parts, settings, encodings and dates.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A message read from an Outlook item (.msg), a saved mail (.eml) or a
;;;; mailbox (.pst, .ost, by readpst) becomes one MAIL, shown one way
;;;; whichever it came from (view.lisp). Its texts come in many encodings,
;;;; named by a charset (MIME) or a code page (Outlook); both are read
;;;; leniently, an octet that is not of the encoding shown as such, never an
;;;; error.

(in-package #:outlook)

(define-package-setting *outlook-readpst-program* "readpst"
  "The program that reads Outlook mailboxes (.pst, .ost) into separate
messages, shut in the sandbox: readpst, of the libpst package, looked for in
the system's own directories of programs. Set it in the init file after the
outlook package is loaded.")

(define-package-setting *outlook-page-program* "weasyprint"
  "The program that draws a message written in HTML as it looks, as pages,
shut in the sandbox: WeasyPrint, looked for in the system's own directories
of programs. It runs no scripts, and shut in it fetches nothing from the
internet. Set it in the init file after the outlook package is loaded.")

(define-package-setting *outlook-include-deleted* t
  "True to list the items a mailbox (.pst, .ost) still holds that were
deleted, in the folders they were deleted from, beside the others; NIL to
list only the items it holds as not deleted.")

(define-package-setting *outlook-largest-file* (* 256 1024 1024)
  "The most octets an Outlook item (.msg) or a saved mail (.eml) may have to be
read as a message, and an attachment or a part of one may hold: 256 MB as
standard. A larger one is opened as any file.")

(define-package-setting *outlook-nesting-limit* 8
  "How deep messages attached to messages are read: an attachment that is a
message is shown as one up to this many levels down, deeper ones listed as
not read; a message of parts within parts is read as deep as twice this.")

(define-package-setting *outlook-mailbox-seconds* 3600
  "The most seconds readpst may take reading a mailbox (.pst, .ost): an hour as
standard, after which it is stopped and what it wrote by then is listed.")

(define-package-setting *outlook-search-text-limit* (* 16 1024 1024)
  "The most characters of a message of a mailbox (.pst, .ost) its list's
search looks through: its headers, the text of its text parts and its
attachments' names, 16 million as standard. A larger message is searched
as far as that, and the list says how many were.")

(define-package-setting *outlook-mailbox-space* (* 32 1024 1024 1024)
  "The most octets readpst may write reading a mailbox (.pst, .ost), its
messages in your cache directory: 32 GB as standard. It is stopped there, as
it is when the drive has less than 1 GB free, and what it wrote by then is
listed.")

(define-package-setting *outlook-preview* t
  "True for the list of a mailbox (.pst, .ost) to show the message on the
current line in a window beside it, read as any message is, nothing in it
run and nothing it points to fetched, following the cursor from line to
line; NIL to leave it off until \\<mailbox-mode>\\[list-toggle-preview] turns
it on in one list.")

;;; A message

(defstruct (mail (:copier nil))
  "A mail item read: FROM, TO, CC and BCC as written to be read (names with
their addresses); SUBJECT; SENT and RECEIVED, universal times or NIL, with
DATE-TEXT the date as the item writes it when it could not be read as one;
MESSAGE-ID; DETAILS, more of what the item says of itself, each (LABEL .
TEXT); BODY, its text to show, read from BODY-KIND (:plain, :html, :rtf, or
NIL for none); HTML, a function giving the HTML its body is written in, when
it has one, read only when asked; HEADERS, its internet headers as they came,
or NIL; ATTACHMENTS, each a MAIL-ATTACHMENT; NOTES, what could not be read of
it, in words."
  from to cc bcc subject sent received date-text message-id
  (details '()) body body-kind html headers (attachments '()) (notes '()))

(setf (documentation 'mail-p 'function) "True when OBJECT is a MAIL.")

(defstruct (mail-attachment (:copier nil))
  "An attachment of a MAIL: NAME as the item gives it; OCTETS, its contents,
or MESSAGE, a MAIL, when it is a message itself; MIME, the type the item
says it is; HIDDEN true when the item hides it from its reader; CONTENT-ID
when the body shows it in place; WHY, what it is in words when it holds
neither octets nor a message read (one too deep, an embedded object); COPY-PATH,
the path of the private copy of its octets once one is made."
  name octets message mime hidden content-id why copy-path)

(setf (documentation 'mail-attachment-p 'function) "True when OBJECT is a MAIL-ATTACHMENT.")

(defun mail-note (mail format &rest arguments)
  "Add to MAIL's notes what could not be read, FORMAT with ARGUMENTS."
  (setf (mail-notes mail) (append (mail-notes mail) (list (apply #'format nil format arguments)))))

(defun mail-detail (mail label text)
  "Add LABEL with TEXT to what MAIL says of itself, when TEXT is not NIL or empty."
  (when (and text (plusp (length (princ-to-string text))))
    (setf (mail-details mail) (append (mail-details mail) (list (cons label (princ-to-string text)))))))

(defun mail-person (name address &optional type)
  "A sender or a recipient as written to be read: NAME <ADDRESS>, the
address with its TYPE when that is not SMTP (EX: /O=...); either alone when
the other is missing or the same; NIL for neither."
  (let ((name (and name (plusp (length (string-trim " " name))) (string-trim " " name)))
        (address (and address (plusp (length (string-trim " " address)))
                      (if (and type (plusp (length type)) (string-not-equal type "SMTP"))
                          (format nil "~a: ~a" type (string-trim " " address))
                          (string-trim " " address)))))
    (cond ((and name address (string-not-equal name address)) (format nil "~a <~a>" name address))
          (t (or address name)))))

;;; Encodings

(defun mail-codepage-charset (codepage)
  "The charset Outlook's code page number CODEPAGE stands for, as
CHARSET-TEXT takes it (1252 windows-1252, 65001 utf-8); NIL for none."
  (and (integerp codepage)
       (case codepage
         (65001 "utf-8") ((20127 28591) "iso-8859-1") (1200 "utf-16le") (1201 "utf-16be") (20866 "koi8-r")
         (21866 "koi8-u") (932 "shift_jis") (936 "gbk") (51932 "euc-jp") (50220 nil)
         (t (cond ((<= 28592 codepage 28606) (format nil "iso-8859-~d" (- codepage 28590)))
                  ((<= 1250 codepage 1258) (format nil "windows-~d" codepage))
                  ((< 0 codepage 10000) (format nil "cp~d" codepage)))))))

;;; Times

(defun filetime-universal-time (filetime)
  "The universal time of FILETIME, a count of 100-nanosecond steps since
1601 as Windows keeps times, to the second; NIL for none (0) or one past the
year 9999."
  (and (integerp filetime) (< 0 filetime #x24C85A5ED1C04000)
       (- (floor filetime 10000000) 9435484800)))

(defun mail-short-time (time)
  "The universal TIME in local time, short, as a list of messages shows it:
2026-10-07 09:15; NIL for none."
  (and time (format-time "%F %R" time)))
