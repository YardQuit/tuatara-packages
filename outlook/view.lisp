;;;; outlook/view.lisp --- A message shown: its headers, attachments, text, details and internet headers.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; An Outlook item (.msg), a saved mail (.eml) and each message of a
;;;; mailbox are shown alike, read only: who it is from and to, its dates and
;;;; subject, its attachments, its text, then what else it says of itself and
;;;; its internet headers whole, as they matter to whoever examines it.
;;;; Nothing in it is run and nothing it points to is fetched. An attachment
;;;; is opened only when asked: here, safely, from a copy of it only you can
;;;; read (a message among them as a message), or in the desktop's program
;;;; after asking, as any file is.

(in-package #:outlook)

(define-major-mode mail-view-mode special-mode
  "A message read, read only: an Outlook item (.msg), a saved mail (.eml) or a
message of a mailbox. Who it is from and to, its dates and subject, its
attachments, its text (its plain text, else its HTML or RTF read as text),
what else it says of itself, and its internet headers whole. Nothing in it is
run, and nothing it points to fetched.
\\<mail-view-mode>\\[push-button] on an attachment opens it here, safely, from a copy only you
can read (one that is a message as a message); \\[mail-open-attachment-outside] opens that copy in
the desktop's program, after asking; \\[forward-button] and \\[backward-button] go from one
attachment to the next; \\[mail-view-file-text] shows the file of the message itself, as
text; \\[quit-window] closes."
  :lighter "Mail" :interactive nil)

(defvar *mail-views* '()
  "The buffers showing a message read from a file.")

(defun outlook-message-kind (native)
  "What the file at NATIVE is as a message, by its name and its first
octets: :msg for an Outlook item (a .msg that is a compound file), :eml for a
saved mail (a .eml that is not binary); NIL for any other."
  (let ((type (native-file-type native)))
    (cond ((string-equal type "msg")
           (let ((head (file-head native)))
             (and head (octets-at-p head *compound-file-signature*) :msg)))
          ((string-equal type "eml")
           (let ((head (file-head native)))
             (and head (not (binary-head-p head)) :eml))))))

(defun outlook-read-file (native)
  "The MAIL the file at NATIVE holds, an Outlook item or a saved mail (by its
first octets); an error saying why when it cannot be read as one or holds
more than *OUTLOOK-LARGEST-FILE*. Reads it whole: call it from a thread
working in the background."
  (let ((octets (call-with-file native (lambda (fd name size) (docx-file-octets fd name size *outlook-largest-file*)))))
    (if (octets-at-p octets *compound-file-signature*) (msg-read octets) (eml-read octets))))

;;; The text shown

(defun mail-line (text)
  "TEXT, a value an item gives, on one line: its line ends and tabs as spaces."
  (and text (substitute-if #\Space (lambda (c) (member c '(#\Newline #\Return #\Tab))) text)))

(defun mail-attachment-words (attachment)
  "What ATTACHMENT is, in words: a message and its subject; its size and
kind by its contents, said when its name says another; hidden from its
reader, or shown in the text, when it is."
  (let ((octets (mail-attachment-octets attachment))
        (message (mail-attachment-message attachment)))
    (format nil "~a~:[~;, hidden from its reader~]~:[~;, shown in the text~]"
            (cond (message (let ((subject (mail-line (mail-subject message))))
                             (format nil "a message~@[: ~a~]" (and (not (equal subject (mail-line (mail-attachment-name attachment)))) subject))))
                  (octets (multiple-value-bind (words kind) (octets-kind (subseq octets 0 (min (length octets) +binary-sample+)))
                            (format nil "~a, ~a~@[, said to be ~a~]~:[~;, NOT what its name says~]"
                                    (octets-words (length octets))
                                    (or words (if (binary-head-p (subseq octets 0 (min (length octets) +binary-sample+))) "binary" "text"))
                                    (and (null words) (mail-attachment-mime attachment))
                                    (and kind (kind-name-disagrees-p kind (or (mail-attachment-name attachment) ""))))))
                  (t (or (mail-attachment-why attachment) "nothing read")))
            (mail-attachment-hidden attachment)
            (mail-attachment-content-id attachment))))

(defparameter *mail-brief-lines* 40
  "The lines of a message's text a preview shows.")

(defun mail-pieces (mail &key brief (open (constantly nil)))
  "The pieces (as RENDER-PIECES takes them) showing MAIL: its headers, its
attachments, each a button doing OPEN, a function of the attachment; its
text; what it says of itself, what could not be read of it and its internet
headers. With BRIEF, its headers, its attachments' names and the start of its
text alone, no buttons."
  (let ((pieces '()))
    (labels ((put (text &optional face) (push (if face (cons text face) text) pieces))
             (line (label value &optional face)
               (when (and value (plusp (length value)))
                 (put (format nil "~11a " label) :type)
                 (put (mail-line value) face)
                 (put (string #\Newline))))
             (heading (text) (put (format nil "~%~a~%" text) :heading)))
      (line "From" (mail-from mail))
      (line "To" (mail-to mail))
      (line "Cc" (mail-cc mail))
      (line "Bcc" (mail-bcc mail))
      (line "Subject" (or (mail-subject mail) "(none)") :strong)
      (line "Sent" (or (time-words (mail-sent mail)) (mail-date-text mail)))
      (line "Received" (time-words (mail-received mail)))
      (line "Message-ID" (mail-message-id mail))
      (let ((attachments (mail-attachments mail)))
        (when attachments
          (heading (format nil "Attachments: ~d" (length attachments)))
          (dolist (attachment attachments)
            (put "  ")
            (let ((name (or (mail-line (mail-attachment-name attachment)) "(no name)")))
              (if brief
                  (put name :strong)
                  (push (list name :link (let ((a attachment)) (lambda () (funcall open a)))) pieces)))
            (let ((words (mail-attachment-words attachment)))
              (put (format nil "  ~a~%" words) (and (search "NOT what its name says" words) :warning))))))
      (heading (format nil "Text~@[ (~a)~]" (case (mail-body-kind mail) (:html "HTML read as text") (:rtf "RTF read as text"))))
      (let ((body (or (mail-body mail) "(none)")))
        (put (if brief
                 (let ((end (loop with at = 0 repeat *mail-brief-lines*
                                  do (setf at (let ((n (position #\Newline body :start at))) (if n (1+ n) (length body))))
                                  finally (return at))))
                   (format nil "~a~:[~;~%...~]" (string-right-trim '(#\Newline) (subseq body 0 end)) (< end (length body))))
                 (string-right-trim '(#\Newline) body)))
        (put (string #\Newline)))
      (unless brief
        (when (mail-notes mail)
          (heading "Not read")
          (dolist (note (mail-notes mail)) (put (format nil "  ~a~%" note) :warning)))
        (when (mail-details mail)
          (heading "What the message says of itself")
          (loop for (label . text) in (mail-details mail)
                do (put (format nil "  ~a: " label) :type)
                   (put (format nil "~a~%" (mail-line text)))))
        (when (mail-headers mail)
          (heading "Internet headers")
          (put (format nil "~a~%" (remove #\Return (mail-headers mail))) :comment))))
    (nreverse pieces)))

(defun mail-brief-text (mail)
  "MAIL in short, as a preview shows it: its headers, its attachments' names
and the start of its text (MAIL-PIECES)."
  (values (pieces-text (mail-pieces mail :brief t))))

(defun mail-render (buffer mail)
  "Show MAIL in BUFFER, a message's buffer: its text made of MAIL-PIECES, its
attachments buttons, point at its start."
  (let ((attachments (mail-attachments mail)))
    (multiple-value-bind (text spans buttons)
        (pieces-text (mail-pieces mail :open #'mail-open-attachment))
      (set-made-text buffer text :spans spans :buttons buttons)
      (setf (buffer-local buffer 'mail) mail
            ;; each attachment's button, (START END ATTACHMENT), for the keys acting on the one at point
            (buffer-local buffer 'mail-attachment-places)
            (loop for (start end) in buttons for attachment in attachments collect (list start end attachment))
            (buffer-point buffer) 0))))

(defun mail-show-message (mail name)
  "Show MAIL, a message read already (one attached to another), in a buffer
of its own called NAME (made unique)."
  (let ((buffer (make-buffer (unique-buffer-name (or (mail-line name) "(no subject)")))))
    (set-major-mode buffer 'mail-view-mode)
    (setf (buffer-local buffer :read-only) t)
    (mail-render buffer mail)
    (show-buffer buffer)
    buffer))

;;; Read in the background

(defstruct (mail-read-job (:include job) (:constructor make-mail-read-job (buffer path)) (:copier nil))
  "The message in the file at PATH being read for BUFFER, a job; MAIL, what was read."
  path mail)

(setf (documentation 'mail-read-job-p 'function) "True when OBJECT is a MAIL-READ-JOB.")

(define-job-words mail-read-job
  :doing "Reading the message"
    :description (format nil "Reading ~a as a message" (native-file-name (mail-read-job-path job)))
  :verb "read the message"
  :left-words "nothing read"
  :at-exit :quiet)

(defmethod job-take-in ((job mail-read-job))
  "Show the message read, or say why it could not be."
  (show-reading-result job (mail-read-job-path job) (lambda (buffer) (mail-render buffer (mail-read-job-mail job)))
                       "~a could not be read as a message: ~a~%"))

(defun mail-view-buffer (native &key safe name)
  "The buffer showing the message in the file at NATIVE (an Outlook item or a
saved mail), read in the background when there is none yet; called NAME, or
by the file's name; SAFE when the file is to be shown as text safely
\(\\[mail-view-file-text])."
  (let ((name (or (mail-line name) (native-file-name native))))
    (reading-buffer '*mail-views* (lambda (b) (car (buffer-local b 'mail-source))) native
                    name (format nil "Reading ~a...~%" name) 'mail-view-mode
                    (lambda (buffer)
                      (setf (buffer-local buffer 'mail-source) (cons native safe))
                      (start-job-thread (make-mail-read-job buffer native)
                                        (lambda (job) (setf (mail-read-job-mail job) (outlook-read-file (mail-read-job-path job))))
                                        :name "tuatara mail reading")))))

;;; Attachments

(defun mail-attachment-copy (attachment)
  "The path of the private copy of ATTACHMENT's octets (WRITE-PRIVATE-COPY),
for you alone and read only: made the first time, and again when it has gone."
  (let ((copy (mail-attachment-copy-path attachment)))
    (if (and copy (path-stamp copy))
        copy
        (setf (mail-attachment-copy-path attachment)
              (write-private-copy (attachment-file-name (mail-attachment-name attachment)) (mail-attachment-octets attachment))))))

(defun mail-open-attachment (attachment)
  "Open ATTACHMENT of a message shown, here: a message as one; any
other from its private copy (MAIL-ATTACHMENT-COPY), an Outlook item or a
saved mail as a message, the rest visited safely (read only, nothing in it
acted on, a binary one in the hex view). Nothing is saved where you keep
your files."
  (let ((name (or (mail-line (mail-attachment-name attachment)) "The attachment")))
    (cond ((mail-attachment-message attachment)
           (mail-show-message (mail-attachment-message attachment) name))
          ((mail-attachment-octets attachment)
           (let ((copy (mail-attachment-copy attachment)))
             (if (outlook-message-kind copy)
                 (show-buffer (mail-view-buffer copy :safe t))
                 (visit-path copy :safe t))
             (message "~a opened from a copy only you can read; \\<mail-view-mode>\\[mail-open-attachment-outside] in the message opens it outside" name)))
          (t (message "~a: ~a" name (or (mail-attachment-why attachment) "nothing to open"))))))

(defun mail-attachment-at-point ()
  "The attachment whose button point is on in the message shown, or NIL."
  (let ((point (buffer-point (current-buffer))))
    (third (find-if (lambda (place) (and (<= (first place) point) (< point (second place))))
                    (buffer-local (current-buffer) 'mail-attachment-places)))))

(define-command mail-open-attachment-outside ()
  "Open the attachment at point in the desktop's own program for its kind,
after asking: a copy of it only you can read, never the message, and only an
attachment of a kind that may be opened outside Tuatara (a picture, a PDF, a
sound, a video, an Office document, by its contents, with a name that
agrees: *open-externally-kinds*). That program is not shut in the sandbox."
  (let ((attachment (mail-attachment-at-point)))
    (cond ((null attachment) (message "No attachment at point: \\<mail-view-mode>\\[forward-button] goes to the next"))
          ((mail-attachment-message attachment)
           (message "~a is a message: \\<mail-view-mode>\\[push-button] opens it here" (mail-line (mail-attachment-name attachment))))
          ((null (mail-attachment-octets attachment))
           (message "~a: ~a" (mail-line (mail-attachment-name attachment)) (or (mail-attachment-why attachment) "nothing to open")))
          (t (open-externally-file (mail-attachment-copy attachment))))))

(define-command mail-view-file-text ()
  "Show the file of the message shown as its text, the file itself: opened
safely when the message was (one of a mailbox, an attachment), else as any
file is opened, as text. A message attached to another has no file of its
own."
  (let ((source (buffer-local (current-buffer) 'mail-source)))
    (cond ((null source) (message "This message is part of another: it has no file of its own"))
          ((cdr source) (visit-path (car source) :safe t))
          (t (visit-path-as-text (car source) nil (lambda (buffer) (show-buffer buffer)))))))

;;; Opening, previews, health

(defun mail-visit (path show)
  "Show an Outlook item (.msg) or a saved mail (.eml) visited at PATH as a
message (MAIL-VIEW-BUFFER), with SHOW, a function of the buffer; one larger
than *OUTLOOK-LARGEST-FILE* is left to be opened as any file, saying why.
Returns the buffer, or NIL for a file of another kind."
  (let* ((native (file-view-path path))
         (kind (and native (outlook-message-kind native))))
    (when kind
      (let ((size (stamp-size (path-stamp native))))
        (if (> size *outlook-largest-file*)
            (progn (file-view-refuses "~a opens as text: it has more than *outlook-largest-file* octets" (native-file-name native))
                   nil)
            (file-view-show native (remove-if-not #'live-buffer-p *mail-views*) (lambda (b) (car (buffer-local b 'mail-source)))
                            (lambda () (mail-view-buffer native)) show))))))

(defun outlook-preview-text (fd path size kind octets)
  "The preview of an Outlook item, a saved mail or a mailbox (a function of
*PREVIEW-TEXT-FUNCTIONS*, FD, PATH, SIZE, KIND and OCTETS as it gives them):
a message's headers, attachments and the start of its text; a mailbox's
size and how to read it; NIL for any other file."
  (let* ((name (native-file-name path))
         (type (native-file-type name)))
    (cond ((eq kind :pst) (mailbox-preview-words name size))
          ((and (<= size *outlook-largest-file*)
                (or (and (string-equal type "msg") (eq kind :ole))
                    (and (string-equal type "eml") (not (binary-head-p octets)))))
           (handler-case
               (let ((all (docx-file-octets fd path size *outlook-largest-file*)))
                 (mail-brief-text (if (eq kind :ole) (msg-read all) (eml-read all))))
             (error (e) (format nil "~a could not be read as a message: ~a" name e)))))))

(defun outlook-health-words ()
  "What reads Outlook's files here, as \\[check-health] says it."
  (let ((readpst (sandbox-find-program *outlook-readpst-program*)))
    (format nil "Outlook items (.msg) and saved mail (.eml) by Tuatara; mailboxes (.pst, .ost) by ~:[none: install readpst (Fedora: libpst)~;~:*~a, shut in~]"
            readpst)))
