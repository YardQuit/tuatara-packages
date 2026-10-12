;;;; outlook/outlook.lisp --- The outlook package: Outlook items, saved mail and mailboxes read safely.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; One line in the init file loads it: (load-package "outlook").

(defpackage #:outlook
  (:use #:cl #:tuatara)
  (:documentation "Outlook items, saved mail and mailboxes, read safely."))

(in-package #:outlook)

(dolist (file '("mail" "cfb" "mime" "msg" "view" "mailbox" "install"))
  (load-package-file (format nil "~a.lisp" file)))
