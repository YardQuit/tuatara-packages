;;;; kitty/test.lisp --- The kitty package: windows the editor opens, reads and types into.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test kitty
  :description "The kitty package: the windows kitty lists, a window's text with its colours as faces, and a kitty of the editor's own opened, typed into and read, which takes no request over its terminal.")

(define-test (kitty windows-listed)
  ;; what kitty's ls answers: its windows in each tab of each of its windows, the editor's own left out
  (let ((listing "[{\"id\": 1, \"tabs\": [{\"windows\": [{\"id\": 1, \"title\": \"edit\", \"cwd\": \"/a\"},
                                                     {\"id\": 2, \"title\": \"sh \\u00e9\", \"cwd\": \"/b\"}]}]},
                   {\"id\": 2, \"tabs\": []}]"))
      (is equal '((1 "edit" "/a") (2 "sh é" "/b")) (kitty::kitty-listed-windows listing nil))
      (is equal '((2 "sh é" "/b")) (kitty::kitty-listed-windows listing 1))
      (is equal '() (kitty::kitty-listed-windows "[]" nil))))

(define-test (kitty ansi-colours)
  (let ((pieces (tuatara.defaults::ansi-pieces
                 (format nil "~c[31mred~c[0m plain ~c[1;38;2;255;0;0mbold~c[m~c]133;A~c\\ ~c[22;1;36mx~c[m"
                         #\Escape #\Escape #\Escape #\Escape #\Escape #\Escape #\Escape #\Escape))))
    (is string= "red plain bold x" (apply #'concatenate 'string (mapcar (lambda (p) (if (stringp p) p (first p))) pieces)))
    ;; red, a palette number; plain, no face; bold in #ff0000; the shell's marks (OSC 133) left out
    (is equal '("red" (:terminal-fg-1)) (first pieces))
    (is equal " plain " (second pieces))
    (true (member :terminal-bold (second (third pieces))))
    (true (member :|TERMINAL-FG-#FF0000| (second (third pieces))))
    (is equal (or (tuatara.defaults::theme-terminal-colour 1) 1) (tuatara.display:face-attribute :terminal-fg-1 :fg))
    ;; as a buffer shows them: faces, all of them, and no button
    (multiple-value-bind (text faces buttons) (tuatara.defaults::pieces-text pieces)
      (is string= "red plain bold x" text)
      (is equal '() buttons)
      (true (find-if (lambda (span) (and (= 10 (first span)) (consp (third span)) (member :terminal-bold (third span)))) faces))))
  ;; colons: a curly underline, an underline's colour, a colour written 38:2::R:G:B
  (let ((pieces (tuatara.defaults::ansi-pieces
                 (format nil "~c[4:3;58:2::255:0:0mwavy~c[4:0mflat ~c[38:2::0:0:255mblue~c[0m"
                         #\Escape #\Escape #\Escape #\Escape))))
    (is equal '("wavy" (:terminal-underline)) (first pieces))
    (is equal "flat " (second pieces))
    (is equal '("blue" (:|TERMINAL-FG-#0000FF|)) (third pieces)))
  ;; a colour of 24 bits taken to the nearest of 4,096: shades close together share a face
  (is eq (tuatara.defaults::terminal-colour-face :fg "#123457") (tuatara.defaults::terminal-colour-face :fg "#113355")))

(defun kitty-here-p ()
  "True when kitty can be started here, hidden: it is installed and there is a display."
  (and (tuatara.io:find-program "kitty") (tuatara.io:find-program "kitten")
       (or (uiop:getenv "WAYLAND_DISPLAY") (uiop:getenv "DISPLAY"))))

(define-test (kitty kitty-opened-sent-read)
  (if (not (kitty-here-p))
      (skip "kitty cannot be started here" (true nil))
      (with-test-directory (dir)
        (let ((old (uiop:getenv "XDG_CONFIG_HOME"))
              (outside (namestring (merge-pathnames "outside.txt" dir))))
          (sb-posix:setenv "XDG_CONFIG_HOME" (namestring (merge-pathnames "config/" dir)) 1)
          (unwind-protect
                 (let ((kitty::*kitty-start-arguments* '("--hidden"))
                       (kitty::*kitty-socket* nil)
                       (kitty::*kitty-window* nil))
                 ;; opened: a kitty of the editor's own, its configuration made with examples only
                 (multiple-value-bind (f editor)
                     (run-keys "M-x k i t t y RET" :text "" :width 80 :height 10)
                   (declare (ignore editor))
                   (true (tuatara.frontend.fake:fake-frame-showing f "kitty opened in")))
                   (let ((socket kitty::*kitty-socket*))
                   (true (search "/tuatara-kitty-" socket))
                     (true (kitty::kitty-alive-p socket))
                   (true (search "Remote control is set by Tuatara"
                                (alexandria:read-file-into-string (merge-pathnames "config/tuatara/kitty.conf" dir))))
                   (unwind-protect
                        (progn
                          ;; the line point is on sent and run; a request over the terminal itself (not its socket) refused
                          (run-keys "M-x k i t t y - i n p u t RET"
                                    :text (format nil "echo tuatara-marker-$((6*7)); KITTY_LISTEN_ON= kitten @ ls > ~a 2>&1; echo exit=$? >> ~a" outside outside)
                                    :width 80 :height 10)
                          (loop repeat 100 until (and (probe-file outside) (search "exit=" (alexandria:read-file-into-string outside)))
                                do (sleep 1/20))
                          (true (search "exit=" (alexandria:read-file-into-string outside)))
                          (false (search "exit=0" (alexandria:read-file-into-string outside)))
                          ;; its text read back, colours and all
                          (multiple-value-bind (f editor) (run-keys "M-x k i t t y - t e x t RET" :text "" :width 80 :height 10)
                            (declare (ignore f))
                            (true (search "tuatara-marker-42" (session-text editor)))
                            (true (uiop:string-prefix-p "*kitty: " (tuatara.buffer:buffer-name (session-buffer editor))))))
                       (ignore-errors (kitty::kitty-call socket '("close-window" "--match" "all"))))))
            (if old (sb-posix:setenv "XDG_CONFIG_HOME" old 1) (sb-posix:unsetenv "XDG_CONFIG_HOME")))))))
