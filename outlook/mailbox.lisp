;;;; outlook/mailbox.lisp --- Outlook mailboxes (.pst, .ost) read by readpst, shut in, and listed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A mailbox is read by readpst (libpst), shut in the sandbox: it sees a
;;;; copy of the mailbox alone, read only (the copy made by Tuatara, the
;;;; mailbox's access time left as it was), and writes each item as a separate message of
;;;; internet mail into a new folder for you alone in the cache directory, a
;;;; folder of messages for each folder of the mailbox. It runs in the
;;;; background, however large the mailbox, held to a time, to a space on the
;;;; drive and to the drive's free space; the folders and messages it wrote are
;;;; then listed, a line each, and opened as messages (view.lisp). As each
;;;; message is read for the list, the text a search of the list looks
;;;; through is written beside it, once: its headers, its text parts decoded
;;;; and its attachments' names, nothing else of an attachment decoded.
;;;; Tuatara's search filters narrow the list by it, matched in the
;;;; background. The mailbox itself is never changed; what readpst wrote goes
;;;; when its list is closed. readpst writes each message twice, as internet
;;;; mail (.eml) and as an Outlook item (.msg), so that those marked can be
;;;; copied out as either.

(in-package #:outlook)

(define-major-mode mailbox-mode sectioned-list-mode
  "An Outlook mailbox (.pst, .ost) read by readpst, shut in the sandbox: its
folders, and under each its messages, a line each: when it was sent, whom
from, how many attachments it has and its subject. \\<mailbox-mode>\\[list-visit] or \\`l' opens the
message on the line, read only, the matches of the list's filters lit in it,
\\`P' there shows it as it looks and \\`q' comes back; \\[list-visit-other] opens it in another window,
the list staying, \\[list-next-entry] and \\[list-previous-entry] go to the next item and the one before; on a folder's line it folds or unfolds the
folder, as \\[list-toggle-fold] does, and \\[list-toggle-all-folds] folds or unfolds them all.
\\[list-mark-entry] marks the message on the line (>), \\[list-unmark-entry] takes its mark off, \\[list-unmark-all] takes
every mark off, \\[list-toggle-marks] marks those not marked and unmarks those marked; \\[mailbox-export] copies those marked, or the one on the line, out
into a folder, as saved mail (.eml) or as Outlook items (.msg), from a menu.
The message on the line is shown beside the list as the cursor moves, read
as any message is (*OUTLOOK-PREVIEW*); \\[list-toggle-preview] turns that off and on.
\\[quit-window] closes the list, \\[mailbox-back-to-folder] too, after asking, going back to its folder in
rimu; the messages readpst wrote for it are
deleted with it.
\\[regex-search-forward] and \\[regex-search-backward] narrow the list as a regular
expression is typed to the messages whose headers, any of them, or contents
match: the text of each text part, decoded, and the names of the
attachments; \\<search>\\[search-exit] keeps the pattern as a filter, the next search
narrowing what it left, and \\[search-abort] lets the one typed go.
\\<mailbox-mode>\\[list-search-filters] lists the filters, to take one away, and
\\[remove-search-filters] takes them all away. The messages are searched in the
background, the mode line saying how far it has got; of a message larger
than *OUTLOOK-SEARCH-TEXT-LIMIT* characters only that much is searched, and
the list says how many there are."
  :lighter "Mailbox" :interactive nil)

(defvar *mailboxes* '()
  "The buffers listing a mailbox.")

(defstruct (mailbox-item (:copier nil))
  "An item of a mailbox as readpst wrote it: PATH, its file; MSG, the same
message written as an Outlook item, or NIL; MESSAGE true for
a message (else a contact, an appointment, written as is); SENT, a universal
time or NIL; FROM and SUBJECT as written to be read; ATTACHMENTS, how many;
TEXT-FILE, the file of what a search of the list looks through in it, or
NIL; WHOLE, NIL when that is only a part of it, the message being too
large."
  path (msg nil) message sent from subject (attachments 0) (text-file nil) (whole t))

(setf (documentation 'mailbox-item-p 'function) "True when OBJECT is a MAILBOX-ITEM.")

(defun mailbox-readpst ()
  "The full path of readpst (*READPST-PROGRAM*) where the sandbox finds it, or NIL."
  (sandbox-find-program *outlook-readpst-program*))

(defun mailbox-preview-words (name size)
  "What the preview says of the mailbox NAME, of SIZE octets."
  (format nil "~a is an Outlook mailbox, ~a.~%~%~
               ~:[readpst is not installed: install it (Fedora: libpst) to read it.~;~
               Opened (RET in a list), it is read by readpst, shut in the sandbox: ~
               its folders and messages listed~:[~;, deleted items among them~].~]~%"
          name (octets-words size) (mailbox-readpst) *outlook-include-deleted*))

;;; What readpst wrote

(defun mailbox-folders (root &key (limit 1000000) (in-order t))
  "The folders under ROOT, a directory readpst wrote in, and ROOT itself,
each (RELATIVE . FILES): RELATIVE its path from ROOT (\"\" for ROOT), FILES
each (NAME . SIZE), its regular files, in the order of their numbers unless
IN-ORDER is NIL. Links and other entries are passed over; at most LIMIT
files, 64 folders deep."
  (let ((found '()) (count 0))
    (labels ((walk (relative depth)
               (let ((files '()) (folders '()))
                 (dolist (entry (directory-entries (path-join root relative)))
                   (destructuring-bind (name kind size own &rest rest) entry
                     (declare (ignore rest))
                     (cond ((not (eq own kind)))                    ; a link, whatever it leads to: passed over
                           ((and (eq kind :regular) (< count limit)) (incf count) (push (cons name size) files))
                           ((and (eq kind :directory) (< depth 64)) (push name folders)))))
                 (push (cons relative (if in-order
                                          (sort files (lambda (a b)
                                                        (let ((x (parse-integer a :junk-allowed t)) (y (parse-integer b :junk-allowed t)))
                                                          (if (and x y (/= x y)) (< x y) (string< a b))))
                                                :key #'car)
                                          files))
                       found)
                 (dolist (folder (sort folders #'string<))
                   (walk (concatenate 'string relative folder "/") (1+ depth))))))
      (walk "" 0))
    (remove-if (lambda (folder) (null (cdr folder))) (nreverse found))))

(defun mailbox-written (root)
  "How many files readpst has written under ROOT, and their octets, as two values."
  (let ((files 0) (octets 0))
    (dolist (folder (ignore-errors (mailbox-folders root :in-order nil)))
      (dolist (file (cdr folder))
        (incf files)
        (incf octets (cdr file))))
    (values files octets)))

(defun mailbox-search-text (part)
  "What a search of a mailbox's list looks through in the message PART
\(MIME-READ), as two values: the text, and NIL when it was cut short at
*OUTLOOK-SEARCH-TEXT-LIMIT* characters, else true. The text is its headers,
every one on a line of its own, encoded words decoded; the name of each attachment; the text of
each text part, decoded from its transfer encoding and charset, HTML as its
text; and so on for each message attached, as far as they were read.
Nothing else of an attachment is decoded."
  (let ((out (make-string-output-stream)) (left *outlook-search-text-limit*) (whole t))
    (labels ((put (text)
               (let ((taken (min (length text) left)))
                 (write-string text out :end taken)
                 (write-char #\Newline out)
                 (decf left taken)
                 (when (< taken (length text)) (setf whole nil))))
             (walk (part)
               (loop for (name . value) in (mime-part-headers part)
                     while (plusp left)
                     do (put (format nil "~a: ~a" name (mail-line (mime-decode-words value)))))
               (dolist (leaf (mime-leaves part))
                 (when (plusp left)
                   (let ((name (mime-part-name leaf)) (type (mime-part-type leaf)))
                     (when name (put name))
                     (cond ((mime-part-message leaf) (walk (mime-part-message leaf)))
                           ((uiop:string-prefix-p "text/" type)
                            ;; written larger than what is left to look through: read as far as that goes
                            (let ((most (* 4 left)))
                              (when (> (- (mime-part-body-end leaf) (mime-part-body-start leaf)) most) (setf whole nil))
                              (put (if (string= type "text/html")
                                       (html-text (mime-part-text leaf :most most))
                                       (mime-part-text leaf :most most)))))))))))
      (walk part))
    (values (get-output-stream-string out) (and whole (plusp left)))))

(defun mailbox-item-of (path name &optional text-path)
  "The MAILBOX-ITEM the file NAME at PATH, written by readpst, is: a message
(a name with no ending, or .eml) read for its date, sender, subject and
attachments counted; any other as it is named. A message larger than
*OUTLOOK-LARGEST-FILE* is read for its headers alone. With TEXT-PATH, a
message's text for a search of the list (MAILBOX-SEARCH-TEXT) is written
there, a new file for you alone."
  (let ((type (native-file-type name)))
    (if (not (or (null type) (string-equal type "eml")))
        (make-mailbox-item :path path :subject name)
        (handler-case
            (let* ((large nil)
                   (octets (call-with-file
                            path (lambda (fd file size)
                                   (if (> size *outlook-largest-file*)
                                       (progn (setf large t) (read-fd-start fd file size 65536))
                                       (read-fd-octets fd :path file :size-hint (1+ size))))))
                   (part (mime-read octets))
                   (leaves (mime-leaves part))
                   (body (mime-body-part leaves))
                   (item (flet ((header (name) (let ((value (mime-header part name))) (and value (mail-line (mime-decode-words value))))))
                           (make-mailbox-item :path path :message t :sent (mail-date-time (mime-header part "date"))
                                              :from (header "from") :subject (header "subject")
                                              :attachments (count-if (lambda (leaf) (mime-attachment-p leaf body)) leaves)
                                              :whole (not large)))))
              (when text-path
                (multiple-value-bind (text whole) (mailbox-search-text part)
                  (setf (mailbox-item-text-file item) (write-new-file text-path text)
                        (mailbox-item-whole item) (and whole (not large)))))
              item)
          (error () (make-mailbox-item :path path :message t :subject (format nil "~a (could not be read)" name)))))))

(defun mailbox-item-search-text (item where)
  "What a search of a mailbox's list looks through in ITEM, an entry of the
list: a message's headers and contents as written for it (MAILBOX-ITEM-OF);
NIL for anything else, or when that cannot be read. WHERE, a part of it to
look at, is not told apart: all of it is looked through."
  (declare (ignore where))
  (let ((file (and (mailbox-item-p item) (mailbox-item-text-file item))))
    (and file
         (ignore-errors
          (call-with-file file (lambda (fd file size)
                                 (octets-string (read-fd-octets fd :path file :size-hint (1+ size)))))))))

;;; Reading it

(defstruct (mailbox-job (:include job) (:constructor make-mailbox-job (buffer path folder &optional text-folder)) (:copier nil))
  "The mailbox at PATH being read for BUFFER, a job: first COPIED of SIZE
octets of it copied for readpst; then readpst writing into FOLDER, FILES of
OCTETS so far; then LISTED of TOTAL files read for the list, the text a
search of the list looks through in each message written into TEXT-FOLDER,
when there is one. FOLDERS, what it lists once done, each (NAME . ITEMS);
NOTES, what to say of how it went."
  path folder text-folder (copied nil) (size 0) (files 0) (octets 0) (listed 0) (total nil) folders (notes '()))

(setf (documentation 'mailbox-job-p 'function) "True when OBJECT is a MAILBOX-JOB.")

(define-job-words mailbox-job
  :doing (cond ((mailbox-job-total job) "Listing the mailbox")
               ((mailbox-job-copied job) "Copying the mailbox")
               (t "Reading the mailbox"))
    :description (format nil "Reading the mailbox ~a with readpst" (native-file-name (mailbox-job-path job)))
  :verb "read the mailbox"
  :left-words "nothing listed")

(defmethod job-percent ((job mailbox-job))
  "How far copying the mailbox or listing the messages has got; NIL while readpst writes them."
  (let ((total (mailbox-job-total job)))
    (cond (total (running-percent (mailbox-job-listed job) total))
          ((mailbox-job-copied job) (running-percent (mailbox-job-copied job) (max 1 (mailbox-job-size job)))))))

(defmethod job-state-words ((job mailbox-job))
  "How far reading the mailbox has got: the per cent copied, the messages readpst has written, then the per cent listed."
  (if (or (mailbox-job-total job) (mailbox-job-copied job))
      (call-next-method)
      (format nil "Reading the mailbox: ~:d items, ~a" (mailbox-job-files job) (octets-words (mailbox-job-octets job)))))

(defun mailbox-copy (job)
  "Copy JOB's mailbox beside its folder for readpst, read without changing
the mailbox's access time, and return the copy's path: refused when the drive
would keep less than 1 GB free, stopped when JOB is asked to stop."
  (let* ((copy (concatenate 'string (string-right-trim "/" (mailbox-job-folder job)) ".pst"))
         (size (stamp-size (path-stamp (mailbox-job-path job))))
         (free (ignore-errors (free-bytes (parent-path copy)))))
    (when (and free (< free (+ size (expt 2 30))))
      (error "The drive of the cache directory has room for no copy of this mailbox (~a, and 1 GB to spare)" (octets-words size)))
    (setf (mailbox-job-size job) size (mailbox-job-copied job) 0)
    (handler-bind ((error (lambda (e) (declare (ignore e)) (ignore-errors (remove-file copy)))))
      (unless (copy-regular-file (mailbox-job-path job) copy :cancel (job-cancel job)
                                 :progress (lambda (done) (setf (mailbox-job-copied job) done)))
        (ignore-errors (remove-file copy))
        (job-check-stop)))
    (setf (mailbox-job-copied job) nil)
    copy))

(defun mailbox-run-readpst (job copy)
  "Run readpst on COPY, JOB's mailbox copied (MAILBOX-COPY), shut in, writing
into JOB's folder, and watch it: what it has written counted each second, and
it stopped when JOB is asked to stop, when it has written more than
*OUTLOOK-MAILBOX-SPACE*, or when the drive has less than 1 GB free. Returns
how it went (SANDBOX-RUN's second value) and what it said."
  (let* ((folder (mailbox-job-folder job))
         (stop (list nil)) (full nil) (result nil)
         ;; -j 0: readpst alone, no processes forked, which the sandbox would end, unfinished, as readpst ends
         (arguments (append '("-m" "-j" "0" "-b" "-q") (and *outlook-include-deleted* '("-D")) '("-o" "/tmp/out" "/tmp/mailbox.pst"))))
    (make-guarded-thread
     (lambda ()
       (unwind-protect
            (setf result (handler-case
                             (multiple-value-list
                              (sandbox-run *outlook-readpst-program* arguments
                                           :directories (list (cons copy "/tmp/mailbox.pst"))
                                           :writable (list (cons (string-right-trim "/" folder) "/tmp/out"))
                                           :seconds *outlook-mailbox-seconds* :memory (* 2 1024 1024 1024)
                                           :output-limit nil :file-limit *outlook-mailbox-space* :cancel stop))
                           (error (e) (list nil :failed (princ-to-string e)))))
         (unless result (setf result (list nil :failed "it ended unexpectedly")))))
     :name "tuatara readpst")
    (loop for second from 1
          until result
          do (sleep 1)
             ;; counted each second, and once many are written, every fifth
             (when (or (< (mailbox-job-files job) 10000) (zerop (mod second 5)))
               (multiple-value-bind (files octets) (mailbox-written folder)
                 (setf (mailbox-job-files job) files (mailbox-job-octets job) octets)
                 (let ((free (ignore-errors (free-bytes folder))))
                   (when (and (not full) (or (> octets *outlook-mailbox-space*) (and free (< free (expt 2 30)))))
                     (setf full (if (> octets *outlook-mailbox-space*)
                                    (format nil "readpst was stopped once it had written ~a (*outlook-mailbox-space*)" (octets-words octets))
                                    "readpst was stopped: the drive of the cache directory has less than 1 GB free")
                           (car stop) t)))))
             (when (car (job-cancel job)) (setf (car stop) t)))
    (job-check-stop)
    (destructuring-bind (output outcome said) result
      (declare (ignore output))
      (when full (push full (mailbox-job-notes job)))
      (values outcome said))))

(defun mailbox-work (job)
  "What a job reading a mailbox does: readpst writes its items
\(MAILBOX-RUN-READPST) from a copy of the mailbox (MAILBOX-COPY), then each is read for the list (MAILBOX-LIST)."
  (multiple-value-bind (outcome said) (let ((copy (mailbox-copy job)))
                                        (unwind-protect (mailbox-run-readpst job copy)
                                          (ignore-errors (remove-file copy))))
    (let ((folder (mailbox-job-folder job)))
      (case outcome
        (:done)
        (:unavailable (error "~a" said))
        (:timeout (push (format nil "readpst was stopped after ~:d seconds (*outlook-mailbox-seconds*): what it wrote by then is listed"
                                *outlook-mailbox-seconds*)
                        (mailbox-job-notes job)))
        (:cancelled)
        (t (if (plusp (mailbox-written folder))
               (push (format nil "readpst ended with an error~@[: ~a~]; what it wrote is listed" (and (plusp (length said)) said))
                     (mailbox-job-notes job))
               (error "readpst could not read it~@[: ~a~]" (and (plusp (length said)) said)))))
      (mailbox-list job))))

(defun mailbox-listed-files (files)
  "Those of FILES, a folder's as MAILBOX-FOLDERS gives them, that are items of
the mailbox, each (NAME . MSG): MSG the name of the same message written as
an Outlook item beside it, or NIL. An Outlook item beside a message of the
same number is not an item of its own, and readpst's leftover
temp_file_attachment none at all."
  (let ((names (make-hash-table :test 'equal)))
    (dolist (file files) (setf (gethash (car file) names) t))
    (loop for (name) in files
          for type = (native-file-type name)
          for msg = (format nil "~a.msg" (native-file-stem name))
          unless (or (string= name "temp_file_attachment")
                     (and (string-equal type "msg") (gethash (format nil "~a.eml" (native-file-stem name)) names)))
            collect (cons name (and (string-equal type "eml") (gethash msg names) msg)))))

(defun mailbox-list (job)
  "Read each item readpst wrote in JOB's folder for the list (MAILBOX-ITEM-OF,
MAILBOX-LISTED-FILES), the folders in order, counting them as they are read,
each message's text for a search of the list written into JOB's text folder;
JOB's FOLDERS then hold them."
  (let* ((folder (mailbox-job-folder job))
         (folders (loop for (relative . files) in (mailbox-folders folder)
                        for listed = (mailbox-listed-files files)
                        when listed collect (cons relative listed)))
         (root (string-right-trim "/" folder))
         (texts (mailbox-job-text-folder job)))
    (setf (mailbox-job-total job) (reduce #'+ folders :key (lambda (f) (length (cdr f)))))
    (setf (mailbox-job-folders job)
          (loop for (relative . files) in folders
                collect (cons (string-right-trim "/" relative)
                              (loop for (name . msg) in files
                                    collect (progn (job-check-stop)
                                                   (incf (mailbox-job-listed job))
                                                   (let ((item (mailbox-item-of (format nil "~a/~a~a" root relative name) name
                                                                                (and texts (path-join texts (princ-to-string (mailbox-job-listed job)))))))
                                                     (when msg (setf (mailbox-item-msg item) (format nil "~a/~a~a" root relative msg)))
                                                     item))))))))

(defparameter *mailbox-columns*
  '((" " :width 1) ("Sent" :width 16) ("From" :width 30) ("Att" :width 3 :right t) ("Subject"))
  "The columns of a mailbox's list, as TABLE-HEADING-PIECES takes them: the
mark (>), when it was sent, whom from, its attachments and its subject.")

(defun mailbox-folder-title (folder)
  "What a mailbox's list calls FOLDER, the path readpst wrote it at."
  (if (plusp (length folder)) (mail-line folder) "(the mailbox itself)"))

(defun mailbox-row (list item)
  "The pieces of the line of ITEM in LIST, a mailbox's list: its mark, when
it was sent, whom from, its attachments and its subject."
  (let ((mark (entry-mark list item)))
    (table-row-pieces *mailbox-columns*
                      (list (list-mark-sign mark)
                            (cons (or (mail-short-time (mailbox-item-sent item)) "") :list-date)
                            (or (mailbox-item-from item) "")
                            (if (plusp (mailbox-item-attachments item)) (princ-to-string (mailbox-item-attachments item)) "")
                            (cons (or (mailbox-item-subject item) "(no subject)") (list-mark-face mark))))))

(defun mailbox-render (buffer job &optional (point :first))
  "Fill BUFFER, a mailbox's list, with what JOB read: a line on the mailbox,
then each folder, a section that folds, and its items, their marks kept
\(SECTIONED-PIECES); point on the first item, or with POINT :keep where it
was. Its searches narrow it, matching each message's headers and contents
\(MAILBOX-ITEM-SEARCH-TEXT), the folders' lines kept over the items shown."
  (let* ((folders (mailbox-job-folders job))
         (count (reduce #'+ folders :key (lambda (f) (length (cdr f)))))
         (intro (list (cons (format nil "Outlook mailbox ~a, ~a: ~:d item~:p in ~:d folder~:p, read by readpst~:[~;, deleted items included~]~%"
                                    (native-file-name (mailbox-job-path job))
                                    (octets-words (or (ignore-errors (stamp-size (path-stamp (mailbox-job-path job)))) 0))
                                    count (length folders) *outlook-include-deleted*)
                            :heading)
                      (format nil "~a~%" (substitute-command-keys "\\<mailbox-mode>\\`RET' or \\`l' opens the item on the line, a folder's line folds; \\<mail-view-mode>\\[mail-view-pages] in a message shows it as it looks, \\[quit-window] comes back; \\<mailbox-mode>\\[regex-search-forward] narrows the list to the items whose headers or contents match; \\[list-mark-entry] marks, \\[mailbox-export] copies out."))))
         (partial (loop for (nil . items) in folders sum (count-if-not #'mailbox-item-whole items)))
         (notes (loop for note in (append (reverse (mailbox-job-notes job))
                                          (and (plusp partial)
                                               (list (format nil "~:d message~:p too large to be searched whole: only the first ~:d characters, or the headers of one larger than ~a, are searched (*outlook-search-text-limit*, *outlook-largest-file*)"
                                                             partial *outlook-search-text-limit* (octets-words *outlook-largest-file*)))))
                      collect (cons (format nil "~a~%" note) :warning)))
         (heading (append (list (string #\Newline)) (table-heading-pieces *mailbox-columns*))))
    (set-search-filtering buffer :text (lambda (entry where) (mailbox-item-search-text (entry-member entry) where))
                                 :nouns "items" :searches t)
    (multiple-value-bind (entries pieces)
        (sectioned-pieces buffer folders #'mailbox-folder-title (lambda (item folder) (declare (ignore folder)) (mailbox-row buffer item)))
      (let* ((before (append intro notes heading))
             (first-line (count #\Newline (pieces-text before))))
        (render-pieces buffer (append before pieces)
                       ;; point on the first item, under its folder's line
                       :point (if (eq point :first) (+ first-line (if (rest entries) 1 0)) point)
                       :entries entries :first-line first-line :mark-key #'entry-member
                       :refill (lambda (buffer) (mailbox-render buffer job :keep)))))))

(defmethod job-take-in ((job mailbox-job))
  "List what the mailbox held, or say why it could not be read."
  (show-reading-result job (mailbox-job-path job) (lambda (buffer) (mailbox-render buffer job))))

(defun mailbox-buffer (native)
  "The buffer listing the mailbox at NATIVE: the one there is, else a new one,
the mailbox read for it in the background (MAILBOX-WORK), the messages
readpst writes, and the text searched in each, kept for you alone in a folder
of its own in mailboxes/ in the cache directory, deleted when it is closed;
its searches narrowing it once it is listed (MAILBOX-RENDER)."
  (let ((name (native-file-name native)))
    (reading-buffer '*mailboxes* (lambda (b) (buffer-local b 'mailbox-path)) native
                    (format nil "~a (mailbox)" name) (format nil "Reading ~a with readpst, shut in the sandbox...~%" name) 'mailbox-mode
                    (lambda (buffer)
                      (setf (buffer-local buffer 'mailbox-path) native
                            (buffer-local buffer :list-preview) *outlook-preview*)
                      ;; readpst's messages in one folder, the text searched in each in another
                      (let ((messages (path-join (buffer-private-folder buffer) "mail/"))
                            (texts (path-join (buffer-private-folder buffer) "text/")))
                        (ensure-directories messages)
                        (ensure-directories texts)
                        (start-job-thread (make-mailbox-job buffer native messages texts) #'mailbox-work :name "tuatara mailbox")))
                    :private-folder "mailboxes/")))

(defun mailbox-visit (path show)
  "Show an Outlook mailbox visited at PATH (a .pst or .ost, by its contents)
as the list of what it holds (MAILBOX-BUFFER), with SHOW, a function of the
buffer; without readpst or the sandbox, leave it to be opened as any file,
saying how to get them. Returns the buffer, or NIL for a file of another kind."
  (let ((native (file-view-path path)))
    (when (and native (eq :pst (nth-value 1 (file-kind native)))
               (file-view-can-draw-p native (mailbox-readpst)
                                     (format nil "readpst reads Outlook mailboxes, and is not installed (Fedora: sudo dnf install libpst; ~a)"
                                             *outlook-readpst-program*)))
      (file-view-show native (remove-if-not #'live-buffer-p *mailboxes*) (lambda (b) (buffer-local b 'mailbox-path))
                      (lambda () (mailbox-buffer native)) show))))

(define-command mailbox-back-to-folder ()
  "Close the list of the mailbox, after asking, and list its folder with rimu,
point on the mailbox: the messages readpst wrote for it are deleted, as
when it is closed any other way."
  (let* ((buffer (current-buffer))
         (path (buffer-local buffer 'mailbox-path)))
    (when (and path (yes-or-no-p* (format nil "Close ~a and go back to its folder in rimu?" (buffer-name buffer))))
      (rimu-show-file path)
      (kill-buffer buffer))))

(defmethod list-entry-noun ((mode mailbox-mode))
  "An item of a mailbox."
  "item")

(defmethod visit-entry ((mode mailbox-mode) list item other)
  "Open ITEM of the mailbox listed in LIST: a message as one, read only, the
matches of the filters narrowing the list lit in it; a contact or an
appointment as its text, safely; with OTHER in another window, the list
staying where it is."
  (let ((show (if other (lambda (buffer) (pop-to-buffer buffer :other t)) (lambda (buffer) (show-buffer buffer)))))
    (if (mailbox-item-message item)
        (let ((view (mail-view-buffer (mailbox-item-path item) :safe t :name (mailbox-item-subject item))))
          (funcall show view)
          (light-search-filters list view))
        (visit-path (mailbox-item-path item) :safe t :show show))))

(defmethod preview-entry ((mode mailbox-mode) list item)
  "ITEM of the mailbox listed in LIST, beside the list: a message read in the
background into a buffer of its own for the preview, as a message is shown,
nothing in it run and nothing it points to fetched, the matches of the
list's filters lit in it; a contact or an appointment said to be one."
  (if (mailbox-item-message item)
      (let ((view (mail-view-buffer (mailbox-item-path item) :safe t :name (mailbox-item-subject item) :preview t)))
        (light-search-filters list view)
        (values view t))
      (values (side-preview-note (native-file-name (mailbox-item-path item))
                                 (substitute-command-keys "A contact or an appointment: \\<mailbox-mode>\\[list-visit] opens it as its text."))
              t)))

(defmethod entry-preview-width ((mode mailbox-mode) list)
  "The columns of a mailbox's list, a subject's first words among them."
  (declare (ignore list))
  80)

;;; Copied out

(defun mailbox-export-name (item type)
  "The name a copy of ITEM, a message, takes copied out as TYPE (\"eml\" or
\"msg\"): when it was sent and its subject, a / or \\ in it made -, as a
file's name may hold them (ATTACHMENT-FILE-NAME), else the number readpst
gave it; any other item keeps its own name."
  (if (mailbox-item-message item)
      (let ((subject (mailbox-item-subject item)) (sent (mailbox-item-sent item)))
        (format nil "~a.~a"
                (attachment-file-name (substitute-if #\- (lambda (c) (find c "/\\"))
                                                     (format nil "~@[~a ~]~@[~a~]" (and sent (format-time "%F %H%M" sent))
                                                             (and subject (subseq subject 0 (min 120 (length subject))))))
                                      (native-file-stem (mailbox-item-path item)))
                type))
      (native-file-name (mailbox-item-path item))))

(defstruct (mailbox-export-job (:include job) (:constructor make-mailbox-export-job (buffer items folder type)) (:copier nil))
  "ITEMS of the mailbox listed in BUFFER being copied out into FOLDER as
TYPE (\"eml\" or \"msg\"), a job: COPIED of them so far, and NOT-COPIED,
those that could not be, each (ITEM . WHY)."
  items folder type (copied 0) (not-copied '()))

(setf (documentation 'mailbox-export-job-p 'function) "True when OBJECT is a MAILBOX-EXPORT-JOB.")

(define-job-words mailbox-export-job
  :doing "Copying messages out"
  :description (format nil "Copying ~:d message~:p out of a mailbox into ~a as .~a"
                       (length (mailbox-export-job-items job)) (mailbox-export-job-folder job) (mailbox-export-job-type job))
  :verb "copy the messages out"
  :left-words (format nil "~:d copied" (mailbox-export-job-copied job)))

(defmethod job-percent ((job mailbox-export-job))
  "How many of the messages have been copied out, in per cent."
  (running-percent (+ (mailbox-export-job-copied job) (length (mailbox-export-job-not-copied job)))
                   (max 1 (length (mailbox-export-job-items job)))))

(defun mailbox-export-work (job)
  "Copy each of JOB's items out into its folder, a new file each
\(MAILBOX-EXPORT-NAME, COPY-TO-FREE-NAME), as JOB's type: an Outlook item
from the one readpst wrote beside the message; noting those that cannot be."
  (let ((folder (mailbox-export-job-folder job)) (type (mailbox-export-job-type job)))
    (dolist (item (mailbox-export-job-items job))
      (job-check-stop)
      (let ((from (if (and (string= type "msg") (mailbox-item-message item)) (mailbox-item-msg item) (mailbox-item-path item))))
        (handler-case
            (cond ((null from) (error "readpst wrote no Outlook item of it"))
                  ((copy-to-free-name from folder (mailbox-export-name item type) :cancel (job-cancel job))
                   (incf (mailbox-export-job-copied job)))
                  (t (job-check-stop)))
          (error (e) (push (cons item (princ-to-string e)) (mailbox-export-job-not-copied job))))))))

(defmethod job-take-in ((job mailbox-export-job))
  "Say how many messages were copied out and where, and which could not be."
  (let ((missed (reverse (mailbox-export-job-not-copied job))))
    (message "~:[~a~;~*Copied ~:d message~:p into ~a~]~@[; not copied: ~{~a~^, ~}~]"
             (eq (job-status job) :done) (ignore-errors (job-ending-words job))
             (mailbox-export-job-copied job) (mailbox-export-job-folder job)
             (and missed (loop for (item . why) in missed
                               collect (format nil "~a (~a)" (or (mailbox-item-subject item) "(no subject)") why))))))

(defun mailbox-export-as (type)
  "Copy the messages marked in the mailbox's list, or with none marked the
one on the current line, out into a folder asked for, as TYPE, \"eml\" or
\"msg\" (MAILBOX-EXPORT-WORK, in the background)."
  (let* ((list (current-buffer))
         (items (remove-if-not #'mailbox-item-p (list-acted-on list))))
    (if (null items)
        (message "No message on this line")
        (let* ((words (list-question nil "message" items :name-of (lambda (item) (or (mailbox-item-subject item) "(no subject)"))))
               (folder (or (read-native-path (format nil "Copy ~a as .~a into: " words type))
                           (progn (message "No folder given: nothing copied") nil))))
          (when (and folder (null (path-kind folder :follow nil)))
            (handler-case (make-directory folder)
              (error (e) (message "~a: nothing copied" e) (setf folder nil))))
          (cond ((null folder))
                ((not (eq :directory (path-kind folder)))
                 (message "~a is not a folder: nothing copied" folder))
                (t (start-job-thread (make-mailbox-export-job list items folder type) #'mailbox-export-work
                                     :name "tuatara mailbox export")
                   (message "Copying ~a out into ~a as .~a..." words folder type)))))))

(define-command mailbox-export-eml ()
  "Copy the messages marked in the mailbox's list (\\<mailbox-mode>\\[list-mark-entry]), or with
none marked the one on the current line, out into a folder asked for, as
saved mail (.eml). A folder not there yet is made. Each is a new file named
by when it was sent and its subject, (2) and so on added where that name is
taken; nothing there is overwritten. They are copied in the background, as
readpst wrote them; the mailbox itself is not touched. A contact or an
appointment is copied as it is."
  (mailbox-export-as "eml"))

(define-command mailbox-export-msg ()
  "Copy the messages marked in the mailbox's list (\\<mailbox-mode>\\[list-mark-entry]), or with
none marked the one on the current line, out into a folder asked for, as
Outlook items (.msg), as \\[mailbox-export-eml] copies them as saved mail."
  (mailbox-export-as "msg"))

(define-menu mailbox-export (:title "Copy out")
  "Open the menu of copying the messages marked in the mailbox's list, or with
none marked the one on the current line, out into a folder: \\`e' as saved
mail (.eml), \\`m' as Outlook items (.msg), the folder asked for then.
\\`C-h' there lists every action."
  ("Copy out as"
   ("e" "Saved mail (.eml)" mailbox-export-eml)
   ("m" "Outlook items (.msg)" mailbox-export-msg)))
