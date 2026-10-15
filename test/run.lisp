;;;; test/run.lisp --- Command-line test runner: sbcl --non-interactive --load test/run.lisp
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loads Tuatara and its tests from the source tree $TUATARA_SOURCE names
;;;; (tools/test.sh sets it), then each package of this repository, compiled
;;;; as load-package compiles it, and its tests (NAME/test.lisp). Each package
;;;; is checked to use only Tuatara's API (OUTSIDE-API), and refused by
;;;; Tuatara when it uses a name Tuatara lacks. Exits 0 when every package
;;;; loads, keeps to the API and passes its tests, 1 otherwise.

(defvar *packages-root* (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*)))

(defvar *tuatara-source*
  (let ((given (sb-ext:posix-getenv "TUATARA_SOURCE")))
    (if (and given (plusp (length given)))
        (pathname (concatenate 'string (string-right-trim "/" given) "/"))
        (merge-pathnames "../tuatara/" *packages-root*))))

(load (merge-pathnames "tools/quicklisp.lisp" *tuatara-source*))
(push *tuatara-source* asdf:*central-registry*)
(ql:quickload :tuatara/tests :silent t)

(defvar *package-names*
  (loop for folder in (uiop:subdirectories *packages-root*)
        for name = (car (last (pathname-directory folder)))
        when (probe-file (merge-pathnames (concatenate 'string name ".lisp") folder))
          collect name))

(defvar *refused* '()
  "The packages Tuatara refused to load, each (NAME . WHY): one uses a name
this Tuatara lacks.")

(dolist (name *package-names*)
  (let ((folder (merge-pathnames (concatenate 'string name "/") *packages-root*)))
    ;; compiled afresh, as load-package compiles a package fetched anew, its names checked
    (dolist (file (uiop:directory-files folder "*.lisp"))
      (uiop:delete-file-if-exists (tuatara.defaults::package-fasl file)))
    (handler-case
        (let ((tuatara.defaults::*package-folder* folder)
              (tuatara.defaults::*loading-package-name* name))
          (tuatara-user:load-package-file (concatenate 'string name ".lisp"))
          (let ((tests (merge-pathnames "test.lisp" folder)))
            (when (probe-file tests) (load tests))))
      (tuatara.defaults::package-lacks-names (refusal)
        (push (cons name (princ-to-string refusal)) *refused*)))))

(defun tuatara-name-p (symbol)
  "True when SYMBOL is one of Tuatara's own, of a package named TUATARA...."
  (let ((home (symbol-package symbol)))
    (and home (uiop:string-prefix-p "TUATARA" (package-name home)))))

(defun public-p (symbol)
  "True when a package may use SYMBOL: exported from TUATARA, or a setting
TUATARA-USER exports."
  (some (lambda (package) (eq (nth-value 1 (find-symbol (symbol-name symbol) package)) :external))
        '(:tuatara :tuatara-user)))

(defun defined-p (symbol)
  "True when SYMBOL names a function, a variable or a class."
  (or (fboundp symbol) (boundp symbol) (find-class symbol nil)))

(defun tuatara-defines-p (symbol)
  "True when Tuatara's defaults define a function, variable or class of
SYMBOL's name."
  (let ((tuatara (find-symbol (symbol-name symbol) :tuatara.defaults)))
    (and tuatara (defined-p tuatara))))

(defvar *tuatara-keys*
  (let ((keys (make-hash-table :test 'equal)))
    (dolist (folder '("src/" "lisp/") keys)
      (dolist (file (directory (merge-pathnames (concatenate 'string folder "**/*.lisp") *tuatara-source*)))
        (cl-ppcre:do-register-groups (name) ("buffer-local [^()']*'([a-z0-9*+%-]+)" (uiop:read-file-string file))
          (setf (gethash (string-upcase name) keys) t)))))
  "The names Tuatara keeps buffers' values under, as its source writes them:
(buffer-local BUFFER 'NAME).")

(defun shared-key-p (symbol)
  "True when Tuatara keeps a buffer's value under SYMBOL's name (*TUATARA-KEYS*)."
  (gethash (symbol-name symbol) *tuatara-keys*))

(defun named-symbol (form)
  "The symbol FORM names as data, 'NAME, or as the key of (buffer-local
BUFFER 'NAME); else NIL."
  (cond ((and (eq (car form) 'quote) (consp (cdr form)) (symbolp (second form))) (second form))
        ((and (eq (car form) 'tuatara.buffer:buffer-local) (consp (cdr form)) (consp (cddr form))
              (consp (third form)) (eq (car (third form)) 'quote) (symbolp (second (third form))))
         (second (third form)))))

(defun outside-api (name)
  "The names the package NAME uses that are not Tuatara's API, written with
Tuatara's package in front (read from its files but test.lisp), each with how;
one called or read though defined nowhere is refused as it loads (*REFUSED*)."
  (let ((own (find-package (string-upcase name)))
        (found '()))
    (dolist (file (uiop:directory-files (merge-pathnames (concatenate 'string name "/") *packages-root*) "*.lisp"))
      (unless (string= (pathname-name file) "test")
        (with-open-file (in file)
          (let ((*package* own) (*read-eval* t))
            (loop for form = (read in nil in) until (eq form in)
                  do (when (and (consp form) (eq (car form) 'in-package)) (setf *package* (find-package (second form))))
                  (labels ((note (format symbol)
                             (pushnew (format nil format symbol) found :test #'string=))
                           (walk (x)
                             (cond ((and (symbolp x) (tuatara-name-p x) (not (public-p x)))
                                    (note "~(~s~), written with its package" x))
                                   ((consp x)
                                    (let ((named (named-symbol x)))
                                      (when (and named (eq (symbol-package named) own))
                                        (cond ((and (eq (car x) 'quote) (tuatara-defines-p named) (not (defined-p named)))
                                               (note "~(~a~), Tuatara's own named as data, not in its API" named))
                                              ((and (consp (cdr x)) (eq (car x) 'tuatara.buffer:buffer-local) (shared-key-p named))
                                               (note "~(~a~), a buffer's value kept by Tuatara under that name, not in its API" named)))))
                                    (walk (car x)) (walk (cdr x))))))
                    (walk form)))))))
    (sort found #'string<)))

(let ((failed nil))
  (loop for (name . why) in (reverse *refused*)
        do (format t "~&~a is refused: ~a~%" name why)
           (setf failed t))
  (dolist (name (set-difference *package-names* (mapcar #'car *refused*) :test #'string=))
    (dolist (use (outside-api name))
      (format t "~&~a uses ~a~%" name use)
      (setf failed t)))
  (dolist (name *package-names*)
    (let ((test (find-symbol (string-upcase name) :tuatara.tests)))
      (when test
        (unless (eql (parachute:status (parachute:test test :report 'parachute:plain)) :passed)
          (setf failed t)))))
  (tuatara.tests::remove-test-directories)
  (sb-ext:exit :code (if failed 1 0)))
