;;;; outlook/outlook.lisp --- The outlook package: Outlook items, saved mail and mailboxes read safely.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; One line in the init file loads it: (load-package "outlook").

(in-package #:tuatara-user)

(dolist (file '("mail" "cfb" "mime" "msg" "view" "mailbox" "install"))
  (load-package-file (format nil "~a.lisp" file)))
