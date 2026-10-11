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
;;;; then listed, a line each, and opened as messages (view.lisp). The
;;;; mailbox itself is never changed; what readpst wrote goes when its list
;;;; is closed.

(in-package #:tuatara.defaults)

(define-major-mode mailbox-mode special-mode
  "An Outlook mailbox (.pst, .ost) read by readpst, shut in the sandbox: its
folders, and under each its messages, a line each: when it was sent, whom
from, how many attachments it has and its subject. \\<mailbox-mode>\\[mailbox-open-item] opens the
message on the line, read only; \\[quit-window] closes the list, and the
messages readpst wrote for it are deleted with it."
  :lighter "Mailbox" :interactive nil)

(defvar *mailboxes* '()
  "The buffers listing a mailbox.")

(defstruct (mailbox-item (:copier nil))
  "An item of a mailbox as readpst wrote it: PATH, its file; MESSAGE true for
a message (else a contact, an appointment, written as is); SENT, a universal
time or NIL; FROM and SUBJECT as written to be read; ATTACHMENTS, how many."
  path message sent from subject (attachments 0))

(setf (documentation 'mailbox-item-p 'function) "True when OBJECT is a MAILBOX-ITEM.")

(defun mailbox-readpst ()
  "The full path of readpst (*READPST-PROGRAM*) where the sandbox finds it, or NIL."
  (tuatara.io:sandbox-find-program *readpst-program*))

(defun mailbox-preview-words (name size)
  "What the preview says of the mailbox NAME, of SIZE octets."
  (format nil "~a is an Outlook mailbox, ~a.~%~%~
               ~:[readpst is not installed: install it (Fedora: libpst) to read it.~;~
               Opened (RET in a list), it is read by readpst, shut in the sandbox: ~
               its folders and messages listed~:[~;, deleted items among them~].~]~%"
          name (octets-words size) (mailbox-readpst) *outlook-include-deleted*))

;;; What readpst wrote

(defun mailbox-folders (root &key (limit 1000000))
  "The folders under ROOT, a directory readpst wrote in, and ROOT itself,
each (RELATIVE . FILES): RELATIVE its path from ROOT (\"\" for ROOT), FILES
each (NAME . SIZE), its regular files in the order of their numbers. Links
and other entries are passed over; at most LIMIT files, 64 folders deep."
  (let ((found '()) (count 0))
    (labels ((walk (relative depth)
               (let ((files '()) (folders '()))
                 (dolist (entry (tuatara.io:directory-entries (tuatara.io:path-join root relative)))
                   (destructuring-bind (name kind size own &rest rest) entry
                     (declare (ignore rest))
                     (cond ((not (eq own kind)))                    ; a link, whatever it leads to: passed over
                           ((and (eq kind :regular) (< count limit)) (incf count) (push (cons name size) files))
                           ((and (eq kind :directory) (< depth 64)) (push name folders)))))
                 (push (cons relative (sort files (lambda (a b)
                                                    (let ((x (parse-integer a :junk-allowed t)) (y (parse-integer b :junk-allowed t)))
                                                      (if (and x y (/= x y)) (< x y) (string< a b))))
                                            :key #'car))
                       found)
                 (dolist (folder (sort folders #'string<))
                   (walk (concatenate 'string relative folder "/") (1+ depth))))))
      (walk "" 0))
    (remove-if (lambda (folder) (null (cdr folder))) (nreverse found))))

(defun mailbox-written (root)
  "How many files readpst has written under ROOT, and their octets, as two values."
  (let ((files 0) (octets 0))
    (dolist (folder (ignore-errors (mailbox-folders root)))
      (dolist (file (cdr folder))
        (incf files)
        (incf octets (cdr file))))
    (values files octets)))

(defun mailbox-item-of (path name)
  "The MAILBOX-ITEM the file NAME at PATH, written by readpst, is: a message
(a name with no ending, or .eml) read for its date, sender, subject and
attachments counted; any other as it is named. A message larger than
*OUTLOOK-LARGEST-FILE* is read for its headers alone."
  (let ((type (tuatara.io:native-file-type name)))
    (if (not (or (null type) (string-equal type "eml")))
        (make-mailbox-item :path path :subject name)
        (handler-case
            (let* ((octets (tuatara.io:call-with-file
                            path (lambda (fd file size)
                                   (if (> size *outlook-largest-file*)
                                       (tuatara.io:read-fd-start fd file size 65536)
                                       (tuatara.io:read-fd-octets fd :path file :size-hint (1+ size))))))
                   (part (mime-read octets))
                   (leaves (mime-leaves part))
                   (body (mime-body-part leaves)))
              (flet ((header (name) (let ((value (mime-header part name))) (and value (mail-line (mime-decode-words value))))))
                (make-mailbox-item :path path :message t :sent (mail-date-time (mime-header part "date"))
                                   :from (header "from") :subject (header "subject")
                                   :attachments (count-if (lambda (leaf) (mime-attachment-p leaf body)) leaves))))
          (error () (make-mailbox-item :path path :message t :subject (format nil "~a (could not be read)" name)))))))

;;; Reading it

(defstruct (mailbox-job (:include tuatara.runtime:job) (:constructor make-mailbox-job (buffer path folder)) (:copier nil))
  "The mailbox at PATH being read for BUFFER, a job: first COPIED of SIZE
octets of it copied for readpst; then readpst writing into FOLDER, FILES of
OCTETS so far; then LISTED of TOTAL files read for the list.
FOLDERS, what it lists once done, each (NAME . ITEMS); NOTES, what to say of
how it went."
  path folder (copied nil) (size 0) (files 0) (octets 0) (listed 0) (total nil) folders (notes '()))

(setf (documentation 'mailbox-job-p 'function) "True when OBJECT is a MAILBOX-JOB.")

(tuatara.runtime:define-job-words mailbox-job
  :doing (cond ((mailbox-job-total job) "Listing the mailbox")
               ((mailbox-job-copied job) "Copying the mailbox")
               (t "Reading the mailbox"))
  :description (format nil "Reading the mailbox ~a with readpst" (tuatara.io:native-file-name (mailbox-job-path job)))
  :verb "read the mailbox"
  :left-words "nothing listed")

(defmethod tuatara.runtime:job-percent ((job mailbox-job))
  "How far copying the mailbox or listing the messages has got; NIL while readpst writes them."
  (let ((total (mailbox-job-total job)))
    (cond (total (tuatara.runtime:running-percent (mailbox-job-listed job) total))
          ((mailbox-job-copied job) (tuatara.runtime:running-percent (mailbox-job-copied job) (max 1 (mailbox-job-size job)))))))

(defmethod tuatara.runtime:job-state-words ((job mailbox-job))
  "How far reading the mailbox has got: the per cent copied, the messages readpst has written, then the per cent listed."
  (if (or (mailbox-job-total job) (mailbox-job-copied job))
      (call-next-method)
      (format nil "Reading the mailbox: ~:d items, ~a" (mailbox-job-files job) (octets-words (mailbox-job-octets job)))))

(defun mailbox-copy (job)
  "Copy JOB's mailbox beside its folder for readpst, read without changing
the mailbox's access time, and return the copy's path: refused when the drive
would keep less than 1 GB free, stopped when JOB is asked to stop."
  (let* ((copy (concatenate 'string (string-right-trim "/" (mailbox-job-folder job)) ".pst"))
         (size (tuatara.io:stamp-size (tuatara.io:path-stamp (mailbox-job-path job))))
         (free (ignore-errors (tuatara.io:free-bytes (tuatara.io:parent-path copy)))))
    (when (and free (< free (+ size (expt 2 30))))
      (error "The drive of the cache directory has room for no copy of this mailbox (~a, and 1 GB to spare)" (octets-words size)))
    (setf (mailbox-job-size job) size (mailbox-job-copied job) 0)
    (handler-bind ((error (lambda (e) (declare (ignore e)) (ignore-errors (tuatara.io:remove-file copy)))))
      (unless (tuatara.io:copy-regular-file (mailbox-job-path job) copy :cancel (tuatara.runtime:job-cancel job)
                                            :progress (lambda (done) (setf (mailbox-job-copied job) done)))
        (ignore-errors (tuatara.io:remove-file copy))
        (tuatara.runtime:job-check-stop)))
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
         (arguments (append '("-e" "-b" "-q") (and *outlook-include-deleted* '("-D")) '("-o" "/tmp/out" "/tmp/mailbox.pst"))))
    (tuatara.runtime:make-guarded-thread
     (lambda ()
       (unwind-protect
            (setf result (handler-case
                             (multiple-value-list
                              (tuatara.io:sandbox-run *readpst-program* arguments
                                                      :directories (list (cons copy "/tmp/mailbox.pst"))
                                                      :writable (list (cons (string-right-trim "/" folder) "/tmp/out"))
                                                      :seconds *outlook-mailbox-seconds* :memory (* 2 1024 1024 1024)
                                                      :output-limit nil :file-limit *outlook-mailbox-space* :cancel stop))
                           (error (e) (list nil :failed (princ-to-string e)))))
         (unless result (setf result (list nil :failed "it ended unexpectedly")))))
     :name "tuatara readpst")
    (loop until result
          do (sleep 1)
             (multiple-value-bind (files octets) (mailbox-written folder)
               (setf (mailbox-job-files job) files (mailbox-job-octets job) octets)
               (let ((free (ignore-errors (tuatara.io:free-bytes folder))))
                 (when (and (not full) (or (> octets *outlook-mailbox-space*) (and free (< free (expt 2 30)))))
                   (setf full (if (> octets *outlook-mailbox-space*)
                                  (format nil "readpst was stopped once it had written ~a (*outlook-mailbox-space*)" (octets-words octets))
                                  "readpst was stopped: the drive of the cache directory has less than 1 GB free")
                         (car stop) t))))
             (when (car (tuatara.runtime:job-cancel job)) (setf (car stop) t)))
    (tuatara.runtime:job-check-stop)
    (destructuring-bind (output outcome said) result
      (declare (ignore output))
      (when full (push full (mailbox-job-notes job)))
      (values outcome said))))

(defun mailbox-work (job)
  "What a job reading a mailbox does: readpst writes its items
\(MAILBOX-RUN-READPST) from a copy of the mailbox (MAILBOX-COPY), then each is read for the list (MAILBOX-LIST)."
  (multiple-value-bind (outcome said) (let ((copy (mailbox-copy job)))
                                        (unwind-protect (mailbox-run-readpst job copy)
                                          (ignore-errors (tuatara.io:remove-file copy))))
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

(defun mailbox-list (job)
  "Read each file readpst wrote in JOB's folder for the list (MAILBOX-ITEM-OF),
the folders in order, counting them as they are read; JOB's FOLDERS then
hold them."
  (let* ((folder (mailbox-job-folder job))
         (folders (mailbox-folders folder))
         (root (string-right-trim "/" folder)))
    (setf (mailbox-job-total job) (reduce #'+ folders :key (lambda (f) (length (cdr f)))))
    (setf (mailbox-job-folders job)
          (loop for (relative . files) in folders
                collect (cons (string-right-trim "/" relative)
                              (loop for (name) in files
                                    collect (progn (tuatara.runtime:job-check-stop)
                                                   (incf (mailbox-job-listed job))
                                                   (mailbox-item-of (format nil "~a/~a~a" root relative name) name))))))))

(defparameter *mailbox-columns*
  '(("Sent" :width 16) ("From" :width 30) ("Att" :width 3 :right t) ("Subject"))
  "The columns of a mailbox's list, as TABLE-HEADING-PIECES takes them.")

(defun mailbox-render (buffer job)
  "Fill BUFFER, a mailbox's list, with what JOB read: a line on the mailbox,
then each folder and its items."
  (let* ((folders (mailbox-job-folders job))
         (count (reduce #'+ folders :key (lambda (f) (length (cdr f)))))
         (intro (list (cons (format nil "Outlook mailbox ~a, ~a: ~:d item~:p in ~:d folder~:p, read by readpst~:[~;, deleted items included~]~%"
                                    (tuatara.io:native-file-name (mailbox-job-path job))
                                    (octets-words (or (ignore-errors (tuatara.io:stamp-size (tuatara.io:path-stamp (mailbox-job-path job)))) 0))
                                    count (length folders) *outlook-include-deleted*)
                            :heading)
                      (format nil "~a~%" (substitute-command-keys "\\<mailbox-mode>\\[mailbox-open-item] opens the item on the line, read only."))))
         (notes (loop for note in (reverse (mailbox-job-notes job)) collect (cons (format nil "~a~%" note) :warning)))
         (heading (append (list (string #\Newline)) (table-heading-pieces *mailbox-columns*)))
         (pieces '()) (entries '()))
    (dolist (folder folders)
      (push (cons (format nil "~a (~:d)~%" (if (plusp (length (car folder))) (mail-line (car folder)) "(the mailbox itself)") (length (cdr folder))) :heading) pieces)
      (push (cons :folder (car folder)) entries)
      (dolist (item (cdr folder))
        (setf pieces (revappend (table-row-pieces *mailbox-columns*
                                                  (list (cons (or (mail-short-time (mailbox-item-sent item)) "") :comment)
                                                        (or (mailbox-item-from item) "")
                                                        (if (plusp (mailbox-item-attachments item)) (princ-to-string (mailbox-item-attachments item)) "")
                                                        (or (mailbox-item-subject item) "(no subject)")))
                                pieces))
        (push item entries)))
    (let* ((before (append intro notes heading))
           (first-line (count #\Newline (pieces-text before))))
      ;; point on the first item, under its folder's line
      (render-pieces buffer (append before (nreverse pieces))
                     :point (+ first-line (if (rest entries) 1 0)) :entries (nreverse entries) :first-line first-line))))

(defmethod tuatara.runtime:job-take-in ((job mailbox-job))
  "List what the mailbox held, or say why it could not be read."
  (show-reading-result job (mailbox-job-path job) (lambda (buffer) (mailbox-render buffer job))))

(defun mailbox-buffer (native)
  "The buffer listing the mailbox at NATIVE: the one there is, else a new one,
the mailbox read for it in the background (MAILBOX-WORK)."
  (let ((name (tuatara.io:native-file-name native)))
    (reading-buffer '*mailboxes* (lambda (b) (tuatara.buffer:buffer-local b 'mailbox-path)) native
                    (format nil "~a (mailbox)" name) (format nil "Reading ~a with readpst, shut in the sandbox...~%" name) 'mailbox-mode
                    (lambda (buffer)
                      ;; for you alone, in mailboxes/ in the cache directory
                      (let ((folder (new-private-folder (private-cache-directory "mailboxes/"))))
                        (setf (tuatara.buffer:buffer-local buffer 'mailbox-path) native
                              (tuatara.buffer:buffer-local buffer 'mailbox-folder) folder)
                        (tuatara.runtime:start-job-thread (make-mailbox-job buffer native folder) #'mailbox-work :name "tuatara mailbox"))))))

(defun mailbox-forget (buffer)
  "Kill-buffer function: when BUFFER lists a mailbox, delete the messages
readpst wrote for it, in the background, once its job, stopped as the buffer
goes, has ended."
  (let ((folder (tuatara.buffer:buffer-local buffer 'mailbox-folder))
        (job (find-if (lambda (job) (and (mailbox-job-p job) (eq buffer (tuatara.runtime:job-buffer job)))) tuatara.runtime:*jobs*)))
    (when folder
      (setf (tuatara.buffer:buffer-local buffer 'mailbox-folder) nil)
      (tuatara.runtime:make-guarded-thread
       (lambda ()
         (loop while (and job (tuatara.runtime:job-running-p job)) do (sleep 0.2))
         (ignore-errors (tuatara.io:delete-tree folder)))
       :name "tuatara mailbox deleted"))))

(defun mailbox-visit (path show)
  "Show an Outlook mailbox visited at PATH (a .pst or .ost, by its contents)
as the list of what it holds (MAILBOX-BUFFER), with SHOW, a function of the
buffer; without readpst or the sandbox, leave it to be opened as any file,
saying how to get them. Returns the buffer, or NIL for a file of another kind."
  (let ((native (file-view-path path)))
    (when (and native (eq :pst (nth-value 1 (file-kind native)))
               (file-view-can-draw-p native (mailbox-readpst)
                                     (format nil "readpst reads Outlook mailboxes, and is not installed (Fedora: sudo dnf install libpst; ~a)"
                                             *readpst-program*)))
      (file-view-show native (remove-if-not #'live-buffer-p *mailboxes*) (lambda (b) (tuatara.buffer:buffer-local b 'mailbox-path))
                      (lambda () (mailbox-buffer native)) show))))

(define-command mailbox-open-item ()
  "Open the item of the mailbox on the current line: a message as one, read
only; a contact or an appointment as its text, safely."
  (let ((entry (list-entry-at)))
    (cond ((mailbox-item-p entry)
           (if (mailbox-item-message entry)
               (show-buffer *editor* (mail-view-buffer (mailbox-item-path entry) :safe t :name (mailbox-item-subject entry)))
               (visit-path (mailbox-item-path entry) :safe t)))
          ((consp entry) (message "A folder of the mailbox: its items are on the lines under it"))
          (t (message "No item on this line")))))
