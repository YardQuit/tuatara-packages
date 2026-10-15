;;;; kitty/kitty.lisp --- kitty windows the editor opens, reads and types into: M-x kitty.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package, not part of the editor: loaded by (load-package "kitty") in
;;;; the init file.
;;;;
;;;; A kitty window is one of a kitty the editor starts itself, never your own
;;;; kitty: drawn by the graphics card, with kitty's ligatures and pictures.
;;;; The editor talks to it over a socket of its own in your runtime folder,
;;;; and only there: kitty takes no request over a terminal (allow_remote_control
;;;; socket-only, given on its command line, which no configuration file can
;;;; undo), so a program run in it, or a file shown in it, cannot steer it. Its
;;;; looks are your kitty.conf, then ~/.config/tuatara/kitty.conf over it. The
;;;; text of a window comes into a buffer with its colours, to move through,
;;;; select, search and copy as any text; a line or a selection goes to it as
;;;; if typed. A window runs only what you type there. A program you
;;;; run in it runs as you, and like any such program may reach the socket;
;;;; what a window only shows (a file's escape sequences) cannot.

(defpackage #:kitty
  (:use #:cl #:tuatara)
  (:documentation "kitty windows the editor opens, reads and types into: M-x kitty."))

(in-package #:kitty)

(define-package-setting *kitty-program* "kitty"
  "The kitty program \\[kitty] opens windows of, found on the PATH. Set it in the
init file after the package is loaded.")

(defparameter *kitty-launcher*
  (package-file "tuatara-kitty")
  "The program that starts a kitty of the editor's own (kitty/tuatara-kitty),
for \\[kitty] and for Tuatara itself in such a kitty.")

(defvar *kitty-socket* nil
  "The socket of the kitty the editor started, unix:PATH, while it may run; NIL before.")

(defparameter *kitty-start-arguments* '()
  "Options given first to the launcher (*KITTY-LAUNCHER*) when \\[kitty] starts a
kitty: none, but for the tests, whose kitty starts hidden (--hidden).")

(defvar *kitty-window* nil
  "The kitty window (its id) \\[kitty-text] and \\[kitty-input] took last.")

(defun kitty-socket-of-environment ()
  "The socket of the kitty the editor runs in, when that is one of its own
\(\\[kitty] there opens a window beside the editor's); else NIL."
  (let ((listen (uiop:getenv "KITTY_LISTEN_ON")))
    (and listen (search "/tuatara-kitty-" listen) listen)))

(defparameter *kitty-seconds* 5
  "How long a request to kitty may take before it is given up.")

(defun kitty-call (socket arguments &key input)
  "Run kitten @ ARGUMENTS against the kitty at SOCKET, INPUT given on its
standard input; its output as a string, and as a second value true when it
went well. At most *KITTY-SECONDS*; for a command the editor keeps reading
keys meanwhile, \\`C-g' giving up."
  (let ((kitten (or (find-program "kitten") (program-missing "kitten" :install "kitty 0.28 or later has it"))))
    (program-output (list* kitten "@" "--to" socket arguments)
                    :input input :seconds *kitty-seconds*
                    :wait (lambda (thread seconds)
                            (wait-interruptibly thread :message "Waiting for kitty..." :seconds seconds)))))

(defun kitty-alive-p (socket)
  "True when the kitty at SOCKET answers."
  (and socket (ignore-errors (nth-value 1 (kitty-call socket '("ls"))))))

(defun kitty-socket ()
  "The socket of a kitty the editor started that answers: the one it runs in,
else the one it opened; NIL when there is none."
  (let ((here (kitty-socket-of-environment)))
    (cond ((kitty-alive-p here) here)
          ((kitty-alive-p *kitty-socket*) *kitty-socket*)
          (t nil))))

(defun kitty-new-socket ()
  "A new socket for a kitty of the editor's own, in your runtime folder (only
you may enter it), its name random: unix:PATH."
  (format nil "unix:~a/tuatara-kitty-~a"
          (string-right-trim "/" (or (uiop:getenv "XDG_RUNTIME_DIR") (namestring (uiop:temporary-directory))))
          (random-name)))

(defun kitty-program ()
  "The kitty program, or an error when it is not installed."
  (or (find-program *kitty-program*) (program-missing *kitty-program*)))

(defun kitty-start (directory)
  "Start a kitty of the editor's own with its first window in DIRECTORY,
listening on a new socket in your runtime folder; wait for it to answer, at
most three seconds. Returns its socket."
  (let* ((program (kitty-program)) (socket (kitty-new-socket))
         (path (subseq socket (length "unix:")))
         (deadline (deadline-in *kitty-seconds*)))
    (start-detached (namestring *kitty-launcher*)
                    (append *kitty-start-arguments* (list "--program" program "--shell" directory socket))
                    :label "kitty")
    ;; its socket's file is there once it listens
    (wait-until-interruptibly (lambda () (or (path-stamp path) (deadline-passed-p deadline)))
                              :message "Starting kitty...")
    (unless (kitty-alive-p socket) (error "kitty did not start (~a)" program))
    (setf *kitty-socket* socket)))

(define-command kitty ()
  "Open a kitty window in the current buffer's directory: a window of a kitty
the editor starts itself, with your shell (beside the editor's window when the
editor runs in such a kitty, started with kitty/tuatara-kitty). Your
kitty.conf gives its looks, and ~/.config/tuatara/kitty.conf over it; the
editor alone can steer it, over a socket of its own. \\[kitty-text] brings its
text here, \\[kitty-input] types a line or the selection into it."
  (let ((directory (native (buffer-directory (current-buffer)))))
    (unless (find-program *kitty-program*)
      (program-missing *kitty-program* :why "no window to open"))
    (unless (find-program "kitten")
      (program-missing "kitten" :install "kitty 0.28 or later has it" :why "no window to open"))
    (let ((here (kitty-socket-of-environment)) (socket (kitty-socket)))
      (cond ((and socket (equal socket here))
             (kitty-call socket (list "launch" "--type=window" "--location=vsplit" "--cwd" directory)))
            (socket
             (kitty-call socket (list "launch" "--type=os-window" "--cwd" directory)))
            (t (kitty-start directory)))
      (message "kitty opened in ~a" (abbreviated-path directory)))))

(defun kitty-listed-windows (listing own)
  "The windows LISTING, what kitty's ls answers (JSON text), names, each (ID
TITLE DIRECTORY), the window OWN (an id, or NIL) left out."
  (loop for os-window across (json-items (json-read listing))
        append (loop for tab across (json-items os-window "tabs")
                     append (loop for window across (json-items tab "windows")
                                  for id = (json-get window "id")
                                  unless (eql id own)
                                  collect (list id (json-get window "title") (json-get window "cwd"))))))

(defun kitty-windows (socket)
  "The windows of the kitty at SOCKET, each (ID TITLE DIRECTORY), the editor's
own window (when it runs there) left out."
  (kitty-listed-windows (kitty-call socket '("ls"))
                        (let ((id (uiop:getenv "KITTY_WINDOW_ID")))
                          (and id (kitty-socket-of-environment) (ignore-errors (parse-integer id))))))

(defun read-kitty-window (socket prompt)
  "A window of the kitty at SOCKET: the only one, else the one chosen with
PROMPT (the last taken offered first)."
  (let ((windows (kitty-windows socket)))
    (cond ((null windows) (error "The kitty has no window open"))
          ((null (rest windows)) (setf *kitty-window* (first (first windows))))
          (t (let* ((candidates (mapcar (lambda (w) (make-candidate (format nil "~a" (second w)) :annotation (third w) :value (first w)))
                                        (let ((last (find *kitty-window* windows :key #'first)))
                                          (if last (cons last (remove last windows)) windows))))
                    (id (completing-read prompt candidates :require-match t :keep-order t)))
               (setf *kitty-window* id))))))

(defun kitty-socket-or-error ()
  "The socket of the editor's kitty, or an error saying there is none."
  (or (kitty-socket) (error "No kitty of the editor's is open: \\[kitty] opens one")))

;;; Colours: a window's text with its SGR colours as faces (ANSI-PIECES, the editor's own)

(define-major-mode kitty-text-mode special-mode
  "A kitty window's text as it was read, its colours kept, to move through,
select, search and copy: \\<kitty-text-mode>\\[revert-buffer] reads it again."
  :lighter "kitty" :interactive nil)

(define-command kitty-text ()
  "Bring the text of a kitty window the editor opened (\\[kitty]) into a buffer,
all of it, the scrollback too, with its colours: to move through, select,
search and copy as any text. Asked which window when there are several.
\\<kitty-text-mode>\\[revert-buffer] there reads it again."
  (let* ((socket (kitty-socket-or-error))
         (id (read-kitty-window socket "Text of the kitty window: "))
         (title (second (find id (kitty-windows socket) :key #'first)))
         (buffer (buffer-in-mode (format nil "*kitty: ~a*" title) 'kitty-text-mode)))
    (render-pieces buffer (lambda ()
                            (multiple-value-bind (text ok)
                                (kitty-call socket (list "get-text" "--match" (format nil "id:~d" id) "--extent" "all" "--ansi"))
                              (if ok (ansi-pieces (string-right-trim '(#\Space #\Newline) text)) (list "The kitty window is no longer open."))))
                   :point :start)
    (setf (buffer-point buffer) (buffer-size buffer))
    (show-buffer buffer)))

(define-command kitty-input ()
  "Type into a kitty window the editor opened (\\[kitty]) the selection, or
without one the line point is on and a return, so that the shell runs it.
Asked which window when there are several."
  (let* ((socket (kitty-socket-or-error))
         (buffer (current-buffer))
         (selected (selected-region buffer))
         (text (if selected
                   (buffer-string buffer :start (first selected) :end (second selected))
                   (buffer-line-text buffer (buffer-line-start buffer (buffer-point buffer))))))
    ;; what a file holds is sent as text, never as keys: a carriage return or an
    ;; escape in it could run more than is shown
    (when (find-if (lambda (c) (and (< (char-code c) 32) (not (member c '(#\Tab #\Newline))))) text)
      (error "The ~:[line~;selection~] holds control characters: not sent" selected))
    (let ((id (read-kitty-window socket "Type into the kitty window: ")))
      (flet ((send (text &rest options)
               (nth-value 1 (kitty-call socket (append (list "send-text" "--match" (format nil "id:~d" id)) options (list "--stdin"))
                                        :input text))))
        ;; pasted, so that its lines are not run one by one; then a return of its own for the line
        (if (and (send text "--bracketed-paste=auto") (or selected (send (string #\Return))))
            (message "Typed into the kitty window")
            (message "The kitty window took nothing: is it still open?"))))))
