(defpackage :build-index
  (:use :cl)
  (:export :build-index))

(in-package :build-index)

(defvar *info-separator* "")

(defun parse-info-csv-line (line &optional (offset 0))
  "Parse text that looks like A: <foo>, B: <bar>, C: <baz> into '((A . foo) (B
. bar) (C . baz)). Assumes that the keys do not contain commas or spaces and
there is exactly one space after a key's colon before the value. Starts at
offset."
  (let* ((colon1 (search ": " line :start2 offset))
         (colon2 (when colon1 (position #\: line :start (1+ colon1))))
         (comma (when colon2 (position #\, line :from-end t :end colon2)))
         (next (when colon2
                 (1+ (position #\Space line :from-end t :end colon2)))))
    (when colon1
      (cons (cons (subseq line offset colon1)
                  (subseq line (+ colon1 2) comma))
            (when next (parse-info-csv-line line next))))))

(defun parse-info-node-line (line)
  "Parse the header for an info node and return the File and Node values if they
exist or NIL if the line is malformed."
  (let* ((parsed (parse-info-csv-line line))
         (file (cdr (assoc "File" parsed :test #'string=)))
         (node (cdr (assoc "Node" parsed :test #'string=))))
    (when (and file node)
      (values file node))))

(defmacro set-next-line (stream line &optional from)
  "Used to set the next line from stream in a loop, returning if we've got EOF."
  `(when (eq (setf ,line (read-line ,stream nil :eof)) :eof)
     ,(if from
          `(return-from ,from)
          '(return))))

(defun info-stream-skip-section (stream)
  "Jump past the next ^_ in the stream. Returns the number of lines skipped."
  (let ((line) (line-number 0))
    (loop
       (set-next-line stream line)
       (incf line-number)
       (when (string= line *info-separator*) (return)))
    line-number))

(defmacro collecting-loop (&body forms)
  "Run FORMS in a loop with #'COLLECT bound to a collector."
  (let ((acc (gensym)))
    `(let ((,acc))
       (flet ((collect (x) (push x ,acc)))
         (loop ,@forms)
         (nreverse ,acc)))))

(defun function-line-offsets (filespec)
  "Return a list of the offsets of the lines in the file pointed to by FILESPEC
that define functions. That is, they should be of the form \" -- <stuff>\". The
list returned consists of pairs (LINE-NUMBER . OFFSET). The result is returned in
decreasing order in LINE-NUMBER."
  (let ((line-number 1) (line) (acc))
    (with-open-file (stream filespec :direction :input)
      (loop
         (let ((pos (file-position stream)))
           (set-next-line stream line)
           (when (and (starts-with-p line " -- ")
                      (>= (length line) 5)
                      (not (eq (elt line 5) #\Space)))
             (push (cons line-number pos) acc))
           (incf line-number))))
    acc))

(defun parse-section-header (line)
  "Parse a line of the form \"nn.mm <name>\" and return <name> on a match or nil
  otherwise."
  (when (and (>= (length line) 5)
             (not (eq #\Space (elt line 0))))
    (multiple-value-bind (major end1) (parse-integer line :junk-allowed t)
      (when (and major
                 (> (length line) end1)
                 (eq #\. (elt line end1)))
        (multiple-value-bind (minor end2)
            (parse-integer line :junk-allowed t :start (+ end1 1))
          (when (and minor
                     (> (length line) (1+ end2))
                     (eq #\Space (elt line end2)))
            (subseq line (+ end2 1))))))))

;; An ugly brute of a "parsing" function, but refactoring is rather difficult
;; because of interactions like expect-node etc.
(defun grok-info-file (filespec)
  "Read through an info file, taking note of the positions of nodes and the
positions of functions / variable definitions. Returns a list matching (&key
NFLLS SES LINE-POSITIONS SECTIONS).

NFLLS is a list of tuples (NODE FILESPEC LINE-START LINE-END) where NODE is the
name of the node we spotted, LINE-START is the number of the first line and
LINE-END is the start of the first line not in NODE. Here and in SES, line
numbers start at 1.

SES is a list of conses (LINE-START . LINE-END), giving the starting and ending
line for each function or variable definition in the file. SES is ordered so the
numbers are decreasing.

LINE-POSITIONS is a vector of integers whose n'th element is the file-position
of the start of the n'th line.

SECTIONS is a list of lists (TITLE LINE-NUMBER POS), one for each section
found."
  (with-open-file (stream filespec :direction :input)
    (let ((line) (line-number 0) (pos)
          (nflls) (ses) (lps)
          (se-starts) (this-nfll)
          (expect-node t) (last-blank t) (file) (node) (sections))
      (flet ((finish-nfll (&optional (delta 0))
               (when this-nfll
                 (push (append this-nfll (list (- line-number delta))) nflls)
                 (setf this-nfll nil)))
             (finish-se (&optional (delta 0))
               (when se-starts
                 (dolist (start (nreverse se-starts))
                   (push (cons start (- line-number delta)) ses))
                 (setf se-starts nil))))
        (loop
           (setf pos (file-position stream))
           (set-next-line stream line)
           (push pos lps)
           (incf line-number)
           (cond
             ;; Blank line
             ((= 0 (length line)))

             ;; Node separator
             ((string= line *info-separator*)
              (setf expect-node t)
              (finish-nfll 1)
              (finish-se 1))

             ;; Node
             ((when expect-node
                (multiple-value-setq (file node) (parse-info-node-line line))
                (setf expect-node nil)
                file)
              (setf this-nfll (list node filespec line-number)))

             ;; Section header
             ((let ((header (parse-section-header line)))
                (when header
                  (push (list header line-number pos) sections)
                  t)))
           
             ;; Function / variable definition.
             ((and (starts-with-p line " -- ")
                   (>= (length line) 5)
                   (not (eq (elt line 5) #\Space)))
              (when last-blank (finish-se 2))
              (push line-number se-starts)))
           ;; Used next time around!
           (setf last-blank (= 0 (length line))))
        (finish-se 1)
        (finish-nfll 1)
        (list :nflls (nreverse nflls)
              :ses ses
              :line-positions (coerce (nreverse lps) 'simple-vector)
              :sections (nreverse sections))))))

(defun info-indirect-files (filespec)
  "Search the info file at FILESPEC for an \"Indirect:\" section. If found,
return the list of files included."
  (with-open-file (stream filespec :direction :input)
    (let ((line))
      (loop
         named outer
         do (set-next-line stream line outer)
         do (when (string= line "Indirect:")
              (return-from outer
                (collecting-loop
                  (set-next-line stream line)
                  (when (string= line *info-separator*) (return))
                  (let ((parsed (parse-info-csv-line line)))
                    (unless (= 1 (length parsed))
                      (format *error-output*
                              "Warning! Malformed Indirect line:~%~A~%~%" line))
                    (collect (caar parsed))))))))))

(defun deep-grok-info-file (filespec)
  "Search through the info file at FILESPEC. For it, and any info files listed
in an Indirect section, calculate NFOLS and FLSES. See GROK-INFO-FILE for their
formats.

Returns a list matching (&key NFLLS FSES FLPS FSECTIONS), which are in the same
format as from GROK-INFO-FILE, but the latter three are alists keyed by file
name."
  (let (all-nflls fses flps fsections)
    (dolist (filespec (cons filespec (info-indirect-files filespec)))
      (destructuring-bind (&key nflls ses line-positions sections)
          (grok-info-file filespec)
        (setf all-nflls (nconc all-nflls nflls))
        (push (cons filespec ses) fses)
        (push (cons filespec line-positions) flps)
        (push (cons filespec sections) fsections)))
    (list :nflls all-nflls :fses fses :flps flps :fsections fsections)))

(defun starts-with-p (str start)
  (and (>= (length str) (length start))
       (string= (subseq str 0 (length start)) start)))

;; Something similar was about 8 lines of perl, but we haven't got regular
;; expressions to hand. :-(
(defun info-read-index (stream)
  "Read an Info index from STREAM. We assume that this is the last section in
the stream, so we don't bother checking for ^_ and stopping. Return a
list (TOPIC-NAME NODE-NAME DELTA-LINES), where DELTA-LINES is the number of
lines from the start of the node for the start of our topic."
  (flet ((space-p (ch) (eq ch #\Space))
         (find-bracket (line start)
           (search "(line" line :from-end t :start2 start)))
    (let ((line))
      (collecting-loop
        (set-next-line stream line)
        ;; The lines we're interested in look like
        ;;  '* <topic-name>:     <node-name>. (line <line-number>)
        ;; except sometimes the last bit is on the following line.
        (when (starts-with-p line "*")
          (let* ((colon (search ": " line :start2 2))
                 (name-start (when colon
                               (position-if-not
                                #'space-p line :start (1+ colon))))
                 (bracket (when name-start (find-bracket line name-start))))
            (when (and colon name-start)
              (let ((topic-name (subseq line 2 colon))
                    (node-name
                     (string-right-trim
                      "."
                      (subseq line name-start
                              (when bracket
                                (1+ (position-if-not #'space-p line
                                                     :from-end t
                                                     :end (1- bracket))))))))
                (unless bracket
                  (set-next-line stream line)
                  (setf bracket (find-bracket line 0)))
                (when bracket
                  (let ((line-number
                         (parse-integer line
                                        :start (+ bracket 5) :junk-allowed t)))
                    (when line-number
                      ;; Subtract 2 from line number, since it is given wrt the
                      ;; preceding ^_ line and is indexed starting at 1.
                      (collect (list topic-name
                                     node-name
                                     (- line-number 2))))))))))))))

(defun info-get-index (nflls flps)
  "Take NFLLS and FLPS as returned by DEEP-GROK-INFO-FILE and return a list of
index topics in the format (TOPIC-NAME NODE-NAME DELTA-LINES)."
  (destructuring-bind (name file line-start line-end)
      (car (last nflls))
    (declare (ignore name line-end))
    (with-open-file (s file :direction :input)
      (file-position s (elt (cdr (assoc file flps :test #'string=))
                            (1- line-start)))
      (info-read-index s))))

(defun idx-list-line-range (idx-list nflls fses)
  "Given an entry from the index, IDX-LIST, together with NFLLS and FSES,
return a list (TOPIC FILE LINE-START . LINE-END) with the file containing the
relevant topic together with a start and end lines for the contents."
  (destructuring-bind (topic node-name delta-lines) idx-list
    (destructuring-bind (&optional filename node-line-start node-line-end)
        (cdr (assoc node-name nflls :test #'string=))
      (unless filename
        (error "Couldn't find ~A in the offset table." node-name))
      ;; Elements of (assoc fses filename) are of the form (LINE-START
      ;; . LINE-END) and they are in descending order. Search for the largest
      ;; that comes with or before the topic in the file, within the given
      ;; node. If that fails, we resort to starting at the given line from the
      ;; node and taking data to either the end of the node or the start of the
      ;; next topic, whichever comes first.
      (let ((last-pair
             (or (find-if (lambda (line-num)
                            (<= node-line-start
                                line-num
                                (+ delta-lines node-line-start)))
                          (cdr (assoc filename fses))
                          :key #'car)
                 ;; We didn't find a definition! This is unusual (in fact, as I
                 ;; write this there are only two instances in the index), so
                 ;; the code needn't run fast.
                 (let ((next-topic
                        (car (find-if (lambda (line-num)
                                        (<= (+ delta-lines node-line-start)
                                            line-num))
                                      (cdr (assoc filename fses))
                                      :key #'car
                                      :from-end t))))
                   (cons (+ delta-lines node-line-start)
                         (if (and next-topic (< next-topic node-line-end))
                             (1- next-topic)
                             node-line-end))))))
        (list topic filename (car last-pair) (cdr last-pair) node-name)))))

(defun topic-ranges (index nflls fses flps)
  "Calculate the correct ranges and file for each topic in the index. Returns a
list of lists (TOPIC FILE START-POS LENGTH NODE-NAME)."
  (mapcar (lambda (idx-list)
            (destructuring-bind (topic file line-start line-end node-name)
                (idx-list-line-range idx-list nflls fses)
              (let* ((lps (cdr (assoc file flps :test #'string=)))
                     (start-pos (elt lps (1- line-start)))
                     (end-pos (elt lps (1- line-end))))
                (when (< end-pos start-pos)
                  (print (list topic file line-start line-end start-pos end-pos)))
                (unless lps (error "Couldn't find line offsets for ~A" file))
                (list topic file start-pos (- end-pos start-pos) node-name))))
          index))

(defun section-ranges (nflls flps fsections)
  "Calculate the ranges for each section (by working out which node it's in and
then taking the end of that node as a bound). Returns a list with elements of
the form (SECTION-TITLE FILENAME OFFSET LENGTH)."
  (mapcan
   (lambda (file-sections)
     (let ((filename (car file-sections)))
       (mapcar
        (lambda (section)
          (destructuring-bind (title line-number pos) section
            ;; Find the containing node
            (let ((node
                   (find-if
                    (lambda (node)
                      (destructuring-bind (nfile line-start line-end) node
                        (and (string= nfile filename)
                             (<= line-start line-number line-end))))
                    nflls :key #'cdr)))
              (unless node
                (error "Couldn't find a containing node for section ~A, ~
                        which should be in file ~A, line ~A."
                       title filename line-number))
              (list title filename pos
                    (- (elt (cdr (assoc filename flps :test #'string=))
                            (fourth node))
                       pos)))))
        (cdr file-sections))))
   fsections))

(defun build-index (filespec target-file)
  (destructuring-bind (&key nflls fses flps fsections)
      (deep-grok-info-file filespec)
    (with-open-file (stream target-file :direction :output
                            :if-exists :supersede)
      (prin1 `((in-package :cl-info)
               (let ((deffn-defvr-pairs
                      ',(topic-ranges (info-get-index nflls flps) nflls fses flps))
                     (section-pairs
                      ',(section-ranges nflls flps fsections))))
               (load-info-hashtables
                (pathname-directory
                 (find-symbol "*LOAD-PATHNAME*"
                              (if (member :gcl *features*) :sys :cl)))
                deffn-defvr-pairs section-pairs))
             stream)))
  (values))
