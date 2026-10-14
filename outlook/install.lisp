;;;; outlook/install.lisp --- The outlook package put in place: its views, keys, preview and health.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package, not part of the editor: loaded by (load-package "outlook") in
;;;; the init file. Once loaded, an Outlook item (.msg), a saved mail (.eml)
;;;; and an Outlook mailbox (.pst, .ost) opened are shown as messages and as
;;;; a list of them; rimu's preview shows them too, and M-x check-health
;;;; says what reads them here.

(in-package #:outlook)

(defun install-outlook ()
  "Have messages and mailboxes visited shown as such (MAIL-VISIT,
MAILBOX-VISIT) and previewed in rimu, their keys bound, and check-health told
what reads them."
  (buttons-in-mode 'mail-view-mode "a message (an Outlook item, a saved mail, a message of a mailbox)")
  (define-key (mode-keymap 'mail-view-mode) "O" 'mail-open-attachment-outside)
  (define-key (mode-keymap 'mail-view-mode) "T" 'mail-view-file-text)
  (define-key (mode-keymap 'mail-view-mode) "P" 'mail-view-pages)
  (commands-for-mode 'mail-view-mode "a message (an Outlook item, a saved mail, a message of a mailbox)"
                     'mail-open-attachment-outside 'mail-view-file-text 'mail-view-pages)
  (entry-list-keys 'mailbox-mode "the list of a mailbox")
  (define-keys (mode-keymap 'mailbox-mode) '(("h" mailbox-back-to-folder) ("C" mailbox-export) ("d" :undefined)))
  (commands-for-mode 'mailbox-mode "the list of a mailbox" 'mailbox-export 'mailbox-export-eml 'mailbox-export-msg
                     'mailbox-back-to-folder)
  (entry-preview-keys 'mailbox-mode "the list of a mailbox")
  (pushnew 'mail-visit *file-views*)
  (pushnew 'mailbox-visit *file-views*)
  (pushnew 'outlook-preview-text *preview-text-functions*)
  (pushnew 'outlook-health-words *health-decoder-functions*))

(install-outlook)
