;;;; outlook/cfb.lisp --- Compound files read, and the compressed RTF an Outlook item keeps its body in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; An Outlook item (.msg) is a compound file, the format of Office
;;;; documents of before 2007 (Microsoft's MS-CFB): a file system of its own
;;;; within one file, of sectors chained by a table (the FAT), small streams
;;;; in sectors of 64 octets of their own (the mini FAT), and a directory of
;;;; storages and streams kept as a tree. A file from elsewhere may be made to
;;;; harm its reader, so every chain is followed with each sector seen once
;;;; and none past the file's end, every stream is held to what its chain
;;;; holds, and the directory tree is walked with each entry seen once,
;;;; never by recursion.
;;;;
;;;; The body of an item may be kept only as RTF, compressed (Microsoft's
;;;; MS-OXRTFCP, "LZFu"); its text is taken out roughly: control words and
;;;; groups that are not text left out, characters given by number decoded,
;;;; and HTML kept in RTF (as Outlook keeps HTML mail) taken back out to be
;;;; read as HTML.

(in-package #:outlook)

(defstruct (cfb (:constructor %make-cfb) (:copier nil))
  "A compound file read from OCTETS: SECTOR-SIZE octets a sector (512 or
4,096) and SECTORS of them; FAT and MINI-FAT, the tables chaining sectors
and mini sectors, vectors of numbers; ENTRIES, its directory, a vector of
CFB-ENTRY; MINI-STREAM, the octets the small streams are kept in; CUTOFF,
the size from which a stream is kept in sectors of its own; OWNERS, the
storage each entry was first found in, which alone it belongs to; GIVEN, the
octets of its streams read so far."
  octets sector-size sectors fat mini-fat entries mini-stream cutoff
  (owners (make-hash-table)) (given 0))

(setf (documentation 'cfb-p 'function) "True when OBJECT is a CFB.")

(defstruct (cfb-entry (:copier nil))
  "An entry of a compound file's directory: NAME; TYPE, :storage, :stream or
:root; LEFT, RIGHT and CHILD, the indices of its siblings either side and of
the first of its children in the tree (NIL for none); START, its first
sector, and SIZE, its octets; INDEX, its own place in the directory."
  name type left right child start size index)

(setf (documentation 'cfb-entry-p 'function) "True when OBJECT is a CFB-ENTRY.")

(defun cfb-number (octets at count)
  "The number in COUNT octets of OCTETS from AT, least significant first; an
error saying the file is cut short when they are past its end."
  (or (octets-le octets at count) (error "The compound file ends too soon")))

(defun cfb-chain (start table limit &key (what "a sector chain"))
  "The sectors chained from START by TABLE (a FAT or mini FAT), in order, as a
vector: each sector below LIMIT (the sectors there are) and seen once; an
error when the chain loops, leaves the table or goes past LIMIT."
  (let ((seen (make-array limit :element-type 'bit :initial-element 0))
        (sectors (make-array 16 :adjustable t :fill-pointer 0)))
    (loop for sector = start then (aref table sector)
          until (= sector #xFFFFFFFE)                       ; the end of the chain
          do (cond ((>= sector limit) (error "~a of the compound file leads past its end" what))
                   ((>= sector (length table)) (error "~a of the compound file leaves its table" what))
                   ((= 1 (sbit seen sector)) (error "~a of the compound file loops" what)))
             (setf (sbit seen sector) 1)
             (vector-push-extend sector sectors))
    sectors))

(defun cfb-sector-octets (octets sectors sector-size &key size)
  "The octets of SECTORS of a compound file's OCTETS, SECTOR-SIZE each, the
first after the header, joined: SIZE of them when given (an error when they
hold fewer), else all; a last sector cut short by the file's end taken as far
as it goes."
  (let* ((held (* (length sectors) sector-size))
         (size (or size held)))
    (when (> size held)
      (error "A stream of the compound file says it holds ~:d octets, more than its ~:d" size held))
    (let ((out (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))
      (loop for sector across sectors
            for at from 0 by sector-size
            while (< at size)
            do (let* ((from (* (1+ sector) sector-size))
                      (count (min sector-size (- size at) (max 0 (- (length octets) from)))))
                 (replace out octets :start1 at :end1 (+ at count) :start2 from)))
      out)))

(defun cfb-read (octets)
  "The compound file in OCTETS, a CFB; an error saying why when it is not one,
or is made so that following it would not end or would read past it."
  (unless (and (>= (length octets) 512) (octets-at-p octets *compound-file-signature*))
    (error "Not a compound file"))
  (let* ((shift (cfb-number octets 30 2))
         (mini-shift (cfb-number octets 32 2))
         (sector-size (if (member shift '(9 12)) (ash 1 shift) (error "A compound file with sectors of 2^~d octets" shift)))
         (sectors (max 0 (ceiling (- (length octets) sector-size) sector-size)))
         (fat-count (cfb-number octets 44 4))
         (per-sector (floor sector-size 4)))
    (unless (= mini-shift 6) (error "A compound file with mini sectors of 2^~d octets" mini-shift))
    (when (> fat-count sectors) (error "The compound file says it has ~:d FAT sectors, more than its ~:d" fat-count sectors))
    ;; the FAT's sectors: 109 named in the header, the rest in a chain of DIFAT sectors
    (let ((fat-sectors (make-array fat-count :fill-pointer 0)))
      (loop for i below (min 109 fat-count)
            do (vector-push (cfb-number octets (+ 76 (* 4 i)) 4) fat-sectors))
      (let ((next (cfb-number octets 68 4)) (seen 0))
        (loop while (and (< (fill-pointer fat-sectors) fat-count) (< next sectors))
              do (when (> (incf seen) sectors) (error "The DIFAT chain of the compound file loops"))
                 (let ((base (* (1+ next) sector-size)))
                   (loop for i below (1- per-sector)
                         while (< (fill-pointer fat-sectors) fat-count)
                         do (vector-push (cfb-number octets (+ base (* 4 i)) 4) fat-sectors))
                   (setf next (cfb-number octets (+ base (* 4 (1- per-sector))) 4)))))
      (let ((fat (make-array (* (fill-pointer fat-sectors) per-sector) :element-type '(unsigned-byte 32))))
        (loop for sector across fat-sectors
              for k from 0
              do (when (>= sector sectors) (error "A FAT sector of the compound file lies past its end"))
                 (let ((base (* (1+ sector) sector-size)))
                   (loop for i below per-sector
                      do (setf (aref fat (+ (* k per-sector) i)) (or (octets-le octets (+ base (* 4 i)) 4) #xFFFFFFFF)))))
        (let* ((directory (cfb-sector-octets octets (cfb-chain (cfb-number octets 48 4) fat sectors :what "The directory")
                                             sector-size))
               (entries (cfb-directory directory (= sector-size 4096)))
               (root (and (plusp (length entries)) (aref entries 0))))
          (unless (and root (eq (cfb-entry-type root) :root)) (error "The compound file has no root"))
          (let* ((mini-fat-octets (let ((start (cfb-number octets 60 4)))
                                    (if (>= start sectors) (make-array 0 :element-type '(unsigned-byte 8))
                                        (cfb-sector-octets octets (cfb-chain start fat sectors :what "The mini FAT") sector-size))))
                 (mini-fat (let ((table (make-array (floor (length mini-fat-octets) 4) :element-type '(unsigned-byte 32))))
                             (dotimes (i (length table) table)
                               (setf (aref table i) (cfb-number mini-fat-octets (* 4 i) 4)))))
                 (mini-stream (if (or (zerop (cfb-entry-size root)) (>= (cfb-entry-start root) sectors))
                                  (make-array 0 :element-type '(unsigned-byte 8))
                                  (cfb-sector-octets octets (cfb-chain (cfb-entry-start root) fat sectors :what "The mini stream")
                                                     sector-size :size (cfb-entry-size root)))))
            (%make-cfb :octets octets :sector-size sector-size :sectors sectors :fat fat :mini-fat mini-fat
                       :entries entries :mini-stream mini-stream :cutoff (cfb-number octets 56 4))))))))

(defun cfb-directory (octets wide)
  "The entries of a compound file's directory, held in OCTETS, 128 octets
each, as a vector of CFB-ENTRY; an unused entry as NIL. WIDE when its
streams' sizes take eight octets (sectors of 4,096), else four, the rest
not looked at."
  (let ((entries (make-array (floor (length octets) 128) :initial-element nil)))
    (dotimes (i (length entries) entries)
      (let* ((at (* i 128))
             (type (case (aref octets (+ at 66)) (1 :storage) (2 :stream) (5 :root))))
        (when type
          (flet ((link (offset) (let ((n (cfb-number octets (+ at offset) 4))) (and (< n (length entries)) n))))
            (setf (aref entries i)
                  (make-cfb-entry :name (utf-16-text octets :start at :end (+ at (min 64 (max 0 (- (cfb-number octets (+ at 64) 2) 2)))))
                                  :type type :left (link 68) :right (link 72) :child (link 76)
                                  :start (cfb-number octets (+ at 116) 4) :size (cfb-number octets (+ at 120) (if wide 8 4))
                                  :index i))))))))

(defun cfb-children (cfb entry)
  "The entries in the storage ENTRY of CFB, as a list in the directory's
order: its tree walked with each entry seen once, a loop in it gone round
never, and an entry another storage holds too left to that one, so that no
stream is read again for each storage that names it."
  (let ((entries (cfb-entries cfb))
        (seen (make-hash-table))
        (found '())
        (stack (list (cfb-entry-child entry))))
    ;; a walk of the siblings' tree with a stack of its own, however deep or crooked it is made
    (loop while stack
          do (let ((index (pop stack)))
               (when (and index (not (gethash index seen)))
                 (setf (gethash index seen) t)
                 (let ((child (aref entries index))
                       (owner (gethash index (cfb-owners cfb))))
                   (when (and child (not (eql index (cfb-entry-index entry)))
                              (or (null owner) (eql owner (cfb-entry-index entry))))
                     (setf (gethash index (cfb-owners cfb)) (cfb-entry-index entry))
                     (push child found)
                     (push (cfb-entry-left child) stack)
                     (push (cfb-entry-right child) stack))))))
    (sort found #'< :key #'cfb-entry-index)))

(defun cfb-stream-octets (cfb entry)
  "The octets of the stream ENTRY of CFB: from the mini stream when it is
smaller than the file's cutoff, else from sectors of its own; an error when
it says it holds more than its chain does or than *OUTLOOK-LARGEST-FILE*, or
when its streams together come to more than twice the file, as streams
sharing their sectors would."
  (let ((size (cfb-entry-size entry)))
    (when (> size *outlook-largest-file*)
      (error "A stream of the compound file says it holds ~:d octets, more than *outlook-largest-file*" size))
    (when (> (incf (cfb-given cfb) size) (* 2 (length (cfb-octets cfb))))
      (error "The streams of the compound file come to more than twice its size: its streams share their octets"))
    (cond ((zerop size) (make-array 0 :element-type '(unsigned-byte 8)))
          ((< size (cfb-cutoff cfb))
           (let* ((mini (cfb-mini-stream cfb))
                  (chain (cfb-chain (cfb-entry-start entry) (cfb-mini-fat cfb) (ceiling (length mini) 64) :what "A small stream")))
             (when (> size (* 64 (length chain)))
               (error "A stream of the compound file says it holds ~:d octets, more than its ~:d" size (* 64 (length chain))))
             (let ((out (make-array size :element-type '(unsigned-byte 8))))
               (loop for sector across chain
                     for at from 0 by 64
                     while (< at size)
                     do (replace out mini :start1 at :end1 (min size (+ at 64)) :start2 (* sector 64)))
               out)))
          (t (cfb-sector-octets (cfb-octets cfb) (cfb-chain (cfb-entry-start entry) (cfb-fat cfb) (cfb-sectors cfb) :what "A stream")
                                (cfb-sector-size cfb) :size size)))))

;;; The body kept as compressed RTF

(defparameter *lzfu-dictionary*
  (concatenate 'string "{\\rtf1\\ansi\\mac\\deff0\\deftab720{\\fonttbl;}{\\f0\\fnil \\froman \\fswiss \\fmodern \\fscript \\fdecor MS Sans SerifSymbolArialTimes New RomanCourier{\\colortbl\\red0\\green0\\blue0"
               (coerce '(#\Return #\Newline) 'string) "\\par \\pard\\plain\\f0\\fs20\\b\\i\\u\\tab\\tx")
  "The text compressed RTF's dictionary starts with, 207 characters, which
its references may point into from the first.")

(defun lzfu-decompress (octets &key (limit *outlook-largest-file*))
  "The RTF compressed in OCTETS, as an Outlook item keeps its body: as it is
when it is stored uncompressed (MELA), else taken out of its references
back into the text written so far (LZFu); at most LIMIT octets, and never
more than it says it holds. An error when it is neither."
  (when (< (length octets) 16) (error "The compressed RTF is cut short"))
  (let* ((kind (cfb-number octets 8 4))
         (end (min (length octets) (+ 4 (cfb-number octets 0 4))))
         ;; no more than it says, nor than its references could make of what there is
         (raw (min limit (cfb-number octets 4 4) (* 9 (max 0 (- end 16))))))
    (case kind
      (#x414C454D (subseq octets 16 (min (length octets) (+ 16 raw))))                 ; MELA: stored as it is
      (#x75465A4C                                                                      ; LZFu
       (let ((dictionary (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0))
             (out (make-array raw :element-type '(unsigned-byte 8) :fill-pointer 0))
             (write (length *lzfu-dictionary*))
             (i 16))
         (loop for c across *lzfu-dictionary* for k from 0 do (setf (aref dictionary k) (char-code c)))
         (flet ((put (octet)
                  (vector-push octet out)
                  (setf (aref dictionary write) octet write (logand (1+ write) 4095))))
           (loop while (and (< i end) (< (fill-pointer out) raw))
                 do (let ((control (aref octets i)))
                      (incf i)
                      (dotimes (bit 8)
                        (when (or (>= i end) (>= (fill-pointer out) raw)) (return))
                        (if (logbitp bit control)
                            (progn
                              (when (>= (1+ i) end) (return))
                              (let* ((reference (logior (ash (aref octets i) 8) (aref octets (1+ i))))
                                     (offset (ash reference -4))
                                     (length (+ 2 (logand reference 15))))
                                (incf i 2)
                                (when (= offset write) (setf i end) (return))     ; the end of the text
                                (dotimes (k length)
                                  (when (>= (fill-pointer out) raw) (return))
                                  (put (aref dictionary (logand (+ offset k) 4095))))))
                            (progn (put (aref octets i)) (incf i)))))))
         (coerce out '(simple-array (unsigned-byte 8) (*)))))
      (t (error "Not compressed RTF")))))

(defparameter *rtf-skipped-groups*
  '("fonttbl" "colortbl" "stylesheet" "info" "pict" "object" "header" "headerl" "headerr" "headerf" "footer"
    "footerl" "footerr" "footerf" "themedata" "colorschememapping" "datastore" "latentstyles" "listtable"
    "listoverridetable" "rsidtbl" "generator" "xmlnstbl" "mmathPr" "fldinst" "filetbl" "revtbl" "pgdsctbl" "nonshppict")
  "Groups of RTF that hold no text to read: tables of fonts, colours and
styles, pictures, objects, headers, a field's instruction.")

(defun rtf-control-word (octets i)
  "The RTF control word in OCTETS whose letters begin at I, as three values:
the word, its number (NIL when it has none) and where what follows it begins,
past the space that may end it."
  (let* ((n (length octets))
         (start i)
         (word (progn (loop while (and (< i n) (let ((o (aref octets i))) (or (<= 97 o 122) (<= 65 o 90)))) do (incf i))
                      (octets-latin-1 octets :start start :end i)))
         (parameter (let ((p i))
                      (when (and (< p n) (= (aref octets p) 45)) (incf p))
                      (loop while (and (< p n) (<= 48 (aref octets p) 57) (< (- p i) 10)) do (incf p))
                      (prog1 (and (> p i) (ignore-errors (parse-integer (octets-latin-1 octets :start i :end p))))
                        (setf i p)))))
    (when (and (< i n) (= (aref octets i) 32)) (incf i))
    (values word parameter i)))

(defun rtf-word-text (word)
  "The text the RTF control word WORD stands for: a line break, a tab, a dash,
a bullet or a quotation mark; NIL for any other word."
  (cond ((member word '("par" "line" "row" "sect" "page") :test #'string=) (string #\Newline))
        ((member word '("tab" "cell") :test #'string=) (string #\Tab))
        ((string= word "emdash") "—") ((string= word "endash") "–")
        ((string= word "bullet") "•") ((string= word "lquote") "‘")
        ((string= word "rquote") "’") ((string= word "ldblquote") "“")
        ((string= word "rdblquote") "”")))

(defun rtf-text (octets &key (limit *outlook-largest-file*))
  "The text of the RTF in OCTETS, roughly, at most LIMIT characters: control
words left out, but those that break lines and tabs; groups that hold no
text (*RTF-SKIPPED-GROUPS*, and those marked as ignorable) left out;
characters given by their number (\\uN, and \\'XX in the document's code
page) decoded. RTF that keeps HTML (\\fromhtml) gives back that HTML's text,
read as HTML (HTML-TEXT), its markup taken from where it keeps it, and the
HTML itself as a second value."
  (let ((out (make-string-output-stream)) (written 0)
        (pending (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (charset "windows-1252") (html nil)
        ;; the group's state: skipped, skipped as RTF of HTML, characters to skip after \u, star seen
        (skip nil) (html-skip nil) (uc 1) (star nil) (to-skip 0)
        ;; groups deeper than 2000 skipped and only counted, so that their closing braces cost nothing
        (stack '()) (depth 0) (deeper 0) (steps 0) (n (length octets)) (i 0))
    (labels ((flush ()
               (when (plusp (fill-pointer pending))
                 (emit (charset-text (coerce pending '(simple-array (unsigned-byte 8) (*))) charset))
                 (setf (fill-pointer pending) 0)))
             (emit (text)
               (when (< written limit)
                 (write-string text out :end (min (length text) (- limit written)))
                 (incf written (length text))))
             (text-p () (not (or skip html-skip)))
             (consumed-by-u ()
               (when (plusp to-skip) (decf to-skip) t)))
      (loop while (and (< i n) (< written limit))
            do (when (zerop (mod (incf steps) 65536)) (job-check-stop))
               (let ((c (aref octets i)))
                 (cond
                   ((= c 123)                                              ; {
                    (flush)
                    (if (< depth 2000)
                        (progn (push (list skip html-skip uc) stack) (incf depth))
                        (progn (incf deeper) (setf skip t)))
                    (setf star nil)
                    (incf i))
                   ((= c 125)                                              ; }
                    (flush)
                    (cond ((plusp deeper) (decf deeper))
                          (stack (destructuring-bind (s h u) (pop stack) (setf skip s html-skip h uc u)) (decf depth)))
                    (setf star nil)
                    (incf i))
                   ((= c 92)                                               ; a backslash
                    (incf i)
                    (when (>= i n) (return))
                    (let ((next (aref octets i)))
                      (cond
                        ((or (<= 97 next 122) (<= 65 next 90))
                         (multiple-value-bind (word parameter after) (rtf-control-word octets i)
                           (setf i after)
                           (unless (string= word "u") (flush))
                           (when star
                             (setf star nil)
                             (unless (and html (member word '("htmltag" "mhtmltag") :test #'string=))
                               (setf skip t)))
                           (cond ((member word *rtf-skipped-groups* :test #'string=) (setf skip t))
                                 ((string= word "fromhtml") (setf html t))
                                 ((string= word "htmlrtf") (setf html-skip (not (eql parameter 0))))
                                 ((string= word "ansicpg") (setf charset (or (mail-codepage-charset parameter) charset)))
                                 ((string= word "uc") (setf uc (max 0 (min 10 (or parameter 1)))))
                                 ((string= word "u")
                                  (flush)
                                  (when (and parameter (text-p))
                                    (let ((code (if (minusp parameter) (+ parameter 65536) parameter)))
                                      (emit (string (if (or (<= #xD800 code #xDFFF) (>= code char-code-limit)) (code-char #xFFFD) (code-char code))))))
                                  (setf to-skip uc))
                                 ((text-p)
                                  (let ((said (rtf-word-text word)))
                                    (when said (emit said)))))))
                        ((= next 39)                                       ; \'XX, an octet of the code page
                      (let ((value (and (<= (+ i 3) n) (ignore-errors (parse-integer (octets-latin-1 octets :start (1+ i) :end (+ i 3)) :radix 16)))))
                           (incf i 3)
                           (when (and value (not (consumed-by-u)) (text-p))
                             (vector-push-extend value pending))))
                        ((= next 42) (setf star t) (incf i))               ; \*, an ignorable group
                        ((member next '(10 13)) (flush) (when (text-p) (emit (string #\Newline))) (incf i))
                        ((= next 126) (flush) (when (text-p) (emit (string (code-char #xA0)))) (incf i))
                        ((= next 95) (flush) (when (text-p) (emit "-")) (incf i))
                        ((= next 45) (incf i))
                        (t (flush)                                          ; \\ \{ \} and the rest, the character itself
                           (when (and (text-p) (not (consumed-by-u))) (emit (string (code-char next))))
                           (incf i)))))
                   ((member c '(10 13)) (incf i))
                   (t (when (and (text-p) (not (consumed-by-u)))
                        (vector-push-extend c pending))
                      (incf i)))))
      (flush)
      (let ((text (get-output-stream-string out)))
        (if html (values (html-text text) text) (string-trim '(#\Newline #\Space) text))))))
