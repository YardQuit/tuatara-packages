;;;; palimpsest/install.lisp --- The palimpsest package put in place: its keys and commands.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package, not part of the editor: loaded by (load-package "palimpsest")
;;;; in the init file. Once loaded, M-x palimpsest examines a PDF, and its
;;;; report's keys are bound.

(in-package #:palimpsest)

(defun install-palimpsest ()
  "Bind the keys of the report on a PDF, name its commands, and have closing a
report ask first when its revisions were not saved, then delete them."
  (buttons-in-mode 'palimpsest-mode "the report on a PDF (its revisions' files and logs, its objects)")
  (define-key (mode-keymap 'palimpsest-mode) "o" 'palimpsest-show-object)
  (define-key (mode-keymap 'palimpsest-mode) "s" 'palimpsest-save)
  (define-key (mode-keymap 'palimpsest-mode) "w" 'palimpsest-write-revision)
  (define-key (mode-keymap 'palimpsest-mode) "C-x C-w" 'palimpsest-write-report)
  (commands-for-mode 'palimpsest-mode "the report on a PDF" 'palimpsest-show-object 'palimpsest-save 'palimpsest-write-report)
  (pushnew 'palimpsest-close-query *kill-buffer-query-functions*)
  (pushnew 'palimpsest-forget *kill-buffer-functions*))

(install-palimpsest)
