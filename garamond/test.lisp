;;;; garamond/test.lisp --- The garamond package: the spaces between sentences a document keeps itself.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test garamond
  :description "The garamond package: two spaces typed after a sentence's end in prose (text modes, comments of other languages), never in code, tables, links, after abbreviations or list numbers; the spacing said, written in the file, followed when a file says it, and the text made to match.")

(defun garamond-typed-text (mode keys &key (text ""))
  "The text a buffer in MODE (keys typed after M-x) holding TEXT has after
C-u 2 M-x garamond-set-spacing and KEYS, typed with modal editing off; and the
frontend, as a second value."
  (let ((tuatara.defaults::*modal-editing* nil))
    (multiple-value-bind (f editor)
        (run-keys (format nil "M-x ~{~a~^ ~} RET M-> C-u 2 M-x g a r a m o n d - s e t - s p a c i n g RET ~a"
                          (map 'list #'string mode) keys)
                  :text text :width 80)
      (values (session-text editor) f))))

(define-test (garamond abbreviations)
  ;; Tuatara's abbreviations and your own end no sentence as a space is typed; one taken out does
  (let ((tuatara-user:*extra-sentence-abbreviations* '("Inc."))
        (tuatara-user:*removed-sentence-abbreviations* '("St.")))
    (is string= "See Dr. Who at Acme Inc. on Main St.  Go"
        (garamond-typed-text "text-mode" "S e e SPC D r . SPC W h o SPC a t SPC A c m e SPC I n c . SPC o n SPC M a i n SPC S t . SPC G o"))))

(define-test (garamond typing-in-text)
  ;; one space typed after a sentence's end is two, more are two; not after an abbreviation or a list's number
  (multiple-value-bind (text f)
      (garamond-typed-text "text-mode" "O n e . SPC T w o ? \" SPC SPC SPC S e e SPC e . g . SPC i t . RET 1 . SPC I t e m")
    (is string= (format nil "One.  Two?\"  See e.g. it.~%1. Item") text)
    (true (tuatara.frontend.fake:fake-frame-showing f "2S")))
  ;; a buffer one space apart: left alone, the mode line saying 1S
  (let ((tuatara.defaults::*modal-editing* nil))
    (multiple-value-bind (f editor)
        (run-keys "M-x t e x t - m o d e RET C-u 1 M-x g a r a m o n d - s e t - s p a c i n g RET O n e . SPC T w o" :width 80)
      (is string= "One. Two" (session-text editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "1S"))))
  ;; typing left alone by the setting: bracketed
  (let ((tuatara-user:*garamond-double-space-on-typing* nil))
    (multiple-value-bind (text f) (garamond-typed-text "text-mode" "O n e . SPC T w o")
      (is string= "One. Two" text)
      (true (tuatara.frontend.fake:fake-frame-showing f "(2S)")))))

(define-test (garamond typing-in-org)
  ;; prose doubled; a source block, a table, a link being typed and inline code left alone
  (is string= (format nil "One.  Two~%")
      (garamond-typed-text "org-mode" "C-p O n e . SPC T w o" :text (format nil "~%")))
  (is string= (format nil "#+begin_src sh~%echo a. b~%#+end_src~%")
      (garamond-typed-text "org-mode" "C-p C-p C-e SPC b" :text (format nil "#+begin_src sh~%echo a.~%#+end_src~%")))
  (is string= (format nil "#+begin_src sh~%echo a. b")
      (garamond-typed-text "org-mode" "SPC b" :text (format nil "#+begin_src sh~%echo a.")))
  (is string= (format nil "| a. b |~%")
      (garamond-typed-text "org-mode" "C-p C-e C-b C-b SPC b" :text (format nil "| a. |~%")))
  (is string= "See [[https://a.b/c. d"
      (garamond-typed-text "org-mode" "SPC d" :text "See [[https://a.b/c."))
  (is string= "Run ~ls a. b~"
      (garamond-typed-text "org-mode" "C-b C-b SPC" :text "Run ~ls a.b~")))

(define-test (garamond typing-in-comments)
  ;; in another language, only in its comments
  (is string= (format nil ";; One.  Two~%(list 1. 2)")
      (garamond-typed-text "lisp-mode" "M-< C-e SPC T w o" :text (format nil ";; One.~%(list 1. 2)")))
  (is string= "(format t \"Hi. there\")"
      (garamond-typed-text "lisp-mode" "M-< C-e C-b C-b SPC t h e r e" :text "(format t \"Hi.\")")))

(define-test (garamond adjusting)
  (let ((tuatara.defaults::*modal-editing* nil)
        (text (format nil "One.  Two. Three!   Four e.g.  five.~%1.  Item.~%  Indented.~%")))
    (multiple-value-bind (f editor)
        (run-keys "M-x t e x t - m o d e RET C-u 2 M-x g a r a m o n d - a d j u s t - s p a c i n g RET" :text text :width 80)
      ;; one space after an abbreviation, as filling leaves it
      (is string= (format nil "One.  Two.  Three!  Four e.g. five.~%1.  Item.~%  Indented.~%") (session-text editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "2 sentence breaks made 2 spaces, 1 after an abbreviation one"))
      (true (tuatara.frontend.fake:fake-frame-showing f "2S")))
    (is string= (format nil "One. Two. Three! Four e.g. five.~%1.  Item.~%  Indented.~%")
        (session-text (nth-value 1 (run-keys "M-x t e x t - m o d e RET C-u 1 M-x g a r a m o n d - a d j u s t - s p a c i n g RET" :text text :width 80))))
    ;; an empty selection is none: the whole buffer is adjusted
    (is string= (format nil "One.  Two.  Three!  Four e.g. five.~%1.  Item.~%  Indented.~%")
        (session-text (nth-value 1 (run-keys "M-x t e x t - m o d e RET C-SPC C-u 2 M-x g a r a m o n d - a d j u s t - s p a c i n g RET" :text text :width 80))))
    ;; in Org, a source block's text is left as it is
    (is string= (format nil "A.  B.~%#+begin_src sh~%echo a. b~%#+end_src~%")
        (session-text (nth-value 1 (run-keys "M-x o r g - m o d e RET C-u 2 M-x g a r a m o n d - a d j u s t - s p a c i n g RET"
                                             :text (format nil "A. B.~%#+begin_src sh~%echo a. b~%#+end_src~%") :width 80))))))

(define-test (garamond files-that-say)
  (with-test-directory (dir)
    (let ((tuatara.defaults::*modal-editing* nil)
          (path (merge-pathnames "two.txt" dir)))
      ;; a file that says: Garamond on as it is opened, typing doubled
      (alexandria:write-string-into-file (format nil "One.~%~%# Local Variables:~%# sentence-end-double-space: t~%# End:~%") path)
      (multiple-value-bind (f editor)
          (run-keys (format nil "C-x C-f C-a C-k ~a RET C-e SPC T w o" (rimu-typed (namestring path))) :width 160)
        (is string= (format nil "One.  Two~%~%# Local Variables:~%# sentence-end-double-space: t~%# End:~%") (session-text editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "2S"))
        (forget-file-buffers))
      ;; not followed when told not to
      (let ((tuatara-user:*garamond-follow-declarations* nil))
        (multiple-value-bind (f editor)
            (run-keys (format nil "C-x C-f C-a C-k ~a RET C-e SPC T w o" (rimu-typed (namestring path))) :width 160)
          (declare (ignore f))
          (is string= "One. Two" (subseq (session-text editor) 0 8))
          (forget-file-buffers)))
      ;; said and written in a file that says nothing
      (let ((plain (merge-pathnames "plain.txt" dir)))
        (alexandria:write-string-into-file (format nil "One.~%") plain)
        (multiple-value-bind (f editor)
            (run-keys (format nil "C-x C-f C-a C-k ~a RET C-u 2 M-x g a r a m o n d - s e t - s p a c i n g RET y RET"
                              (rimu-typed (namestring plain)))
                      :width 160)
          (is string= (format nil "One.~%~%Local Variables:~%sentence-end-double-space: t~%End:~%") (session-text editor))
          (true (tuatara.frontend.fake:fake-frame-showing f "written in the file"))
          (forget-file-buffers)))
      ;; mode: garamond on the first line
      (let ((first (merge-pathnames "first.txt" dir)))
        (alexandria:write-string-into-file (format nil "-*- mode: garamond -*-~%One.~%") first)
        (multiple-value-bind (f editor)
            (run-keys (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring first))) :width 160)
          (declare (ignore f))
            (true (garamond::garamond-on-p (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (forget-file-buffers))))))

(define-test (garamond refusals-and-report)
  (let ((b (tuatara.buffer:make-buffer "*garamond-refusal*" :text "")))
    (unwind-protect
         (progn
           (tuatara.defaults::set-major-mode b 'tuatara.defaults::special-mode)
             (true (garamond::garamond-refusal b))
           (tuatara.defaults::set-major-mode b 'tuatara.defaults::text-mode)
             (false (garamond::garamond-refusal b)))
      (tuatara.buffer:kill-buffer b)))
  (let ((tuatara.defaults::*modal-editing* nil))
    (run-keys "M-x t e x t - m o d e RET M-x g a r a m o n d - d o c t o r RET" :width 100 :height 30)
    (let ((report (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Help*"))))
      (true (search "Garamond in *scratch*" report))
      (true (search "says its spacing            no" report))
      (true (search "nothing says its spacing" report))
      ;; the diagnosis filled as help is, the key of the command it names written in
      (true (search "M-x garamond-set-spacing" report))
      (true (every (lambda (line) (<= (length line) tuatara.defaults::*help-column*))
                   (uiop:split-string (subseq report (search "Diagnosis" report)) :separator '(#\Newline)))))))

(define-test (garamond half-typed-and-windows-lines)
  ;; inline code or verbatim being typed, not closed yet: no second space
  (is string= "Run ~ls a. b"
      (garamond-typed-text "org-mode" "SPC b" :text "Run ~ls a."))
  (is string= "Say =x. y"
      (garamond-typed-text "org-mode" "SPC y" :text "Say =x."))
  ;; a blank before a carriage return and a newline is no gap after a sentence
  (is equal '((12 . 14)) (garamond::garamond-gap-runs (format nil "End. ~c~%Next.  More" #\Return)))
  (is equal '((4 . 6) (11 . 12)) (garamond::garamond-gap-runs "Ünë.  Two?” Three"))
  (is equal '((6 . 8) (15 . 16)) (garamond::runs-octets "Ünë.  Two?” Three" '((4 . 6) (11 . 12)) 0))
  ;; adjusted, a file with such lines keeps them as they are
  (let ((b (tuatara.buffer:make-buffer "*garamond-crlf*" :text (format nil "One. ~c~%Two.  Three. Four~c~%" #\Return #\Return))))
    (unwind-protect
         (progn
           (tuatara.defaults::set-major-mode b 'tuatara.defaults::text-mode)
           (is equal '((19 20 . 2)) (garamond::garamond-gaps b 0 (tuatara.buffer:buffer-size b) 2)))
      (tuatara.buffer:kill-buffer b))))
