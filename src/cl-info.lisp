(in-package :cl-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A (reasonably) generic documentation system                                ;;
;;                                                                            ;;
;; To implement a documentation type, you must provide implementations for    ;;
;; the three generic functions directly below and then call                   ;;
;; register-documentation-type to tell the documentation system about it.     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass doc ()
  ((name :reader doc-name :initarg :name)))

(defmethod print-object ((d doc) stream)
  (print-unreadable-object (d stream :type t)
    (princ (doc-name d) stream)))

(defclass doc-topic ()
  ((name :reader doc-topic-name :initarg :name)
   (section :reader doc-topic-section :initarg :section :initform nil))
  (:documentation
   "A documentation system should probably use this class to return
topics. SECTION, if non-nil, is the name of a containing chapter or other
division."))

(defmethod print-object ((dt doc-topic) stream)
  (print-unreadable-object (dt stream :type t)
    (princ (doc-topic-name dt) stream)))

(defgeneric documentation-matching-topics (doc predicate)
  (:documentation "Return a list of all the topics in DOC that match
PREDICATE. These should implement DOC-TOPIC-NAME and DOC-TOPIC-SECTION (ie
should probably be instances of a subclass of DOC-TOPIC)."))

(defgeneric documentation-for-topic (doc topic)
  (:documentation "Given TOPIC which is stored by DOC, return the corresponding
documentation text."))

;; Infrastructure for doc types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *documentation-types* nil "See REGISTER-DOCUMENTATION-TYPE.")

(defun register-documentation-type (name predicate register)
  "NAME is a symbol to identify the documentation type. PREDICATE should be a
function of one argument, X, which returns T if X is a valid input to REGISTER,
which should be a function that return a DOC object corresponding to X."
  (let ((hit (assoc name *documentation-types* :test #'eq)))
    (if hit
        (setf (cdr hit) (list predicate register))
        (push (list name predicate register) *documentation-types*)))
  (values))

;; Register-document and associated infrastructure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *documents* nil
  "A list of DOC objects representing the documents we have in the index.")

(defun info-pathname (lang-subdir)
  (merge-pathnames
   (make-pathname :name "maxima" :type "info"
                  :directory (when lang-subdir (list :relative lang-subdir)))
   ;; Append / so that the namestring does actually refer to the directory. A
   ;; hack, but I don't want to change *maxima-infodir* yet and possibly break
   ;; stuff elsewhere.
   (parse-namestring (format nil "~A/" maxima::*maxima-infodir*))))

(defvar *standard-document-locations*
  (list (info-pathname maxima::*maxima-lang-subdir*))
  "A list of documents that ensure are loaded the first time someone searches
for documentation.")

(defun ensure-standard-documents ()
  (mapc (lambda (descriptor)
          (unless (assoc descriptor *documents*)
            (register-document descriptor)))
        *standard-document-locations*)
  (values))

(defun register-document (x)
  "Tell the documentation system that X represents some document that we should
search. Throws an error if no documentation type handles X."
  (let ((triple (find-if (lambda (doctype-triple)
                           (funcall (second doctype-triple) x))
                         *documentation-types*)))
    (if triple
        (push (cons x (funcall (third triple) x)) *documents*)
        (error "No handler for document described by ~A." x))
    (values)))

;; The two documentation search entry points ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun search-documentation-exact (x)
  (let ((exact-matches (topic-match x t))
        (inexact-matches (topic-match x nil))
        (no-match-msg
         (format nil (intl:gettext "No exact match found for topic `~a'.") x))
        (yes-inexact-msg
         (format nil (intl:gettext
                      "There are some inexact matches for `~a'.") x)))
    (cond
      ((not exact-matches)
       (format t "  ~A~%~:[~2*~;  ~A~%  ~A~]~%~%"
               no-match-msg inexact-matches yes-inexact-msg
               (format nil (intl:gettext
                            "Try `?? ~a' (inexact match) instead.") x))
       nil)
      (t
       (display-items exact-matches)
       (when inexact-matches
         (format t "  ~A~%  ~A~%~%"
                 yes-inexact-msg
                 (format nil (intl:gettext "Try `?? ~a' to see them.") x)))
       t))))

(defun search-documentation-inexact (x)
  (let ((inexact-matches (topic-match x nil)))
    (when inexact-matches
      (display-items inexact-matches))
    (not (null inexact-matches))))

;; User interaction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *prompt-prefix* "")
(defvar *prompt-suffix* "")

(defun print-prompt (prompt-count)
  (format t "~&~a~a~a"
	  *prompt-prefix*
	  (if (zerop prompt-count)
	      (intl:gettext "Enter space-separated numbers, `all' or `none': ")
	      (intl:gettext "Still waiting: "))
	  *prompt-suffix*))

(defvar +select-by-keyword-alist+
  '((noop "") (all "a" "al" "all") (none "n" "no" "non" "none")))

(defun parse-user-choice (nitems)
  (loop
   with line = (read-line) and nth and pos = 0
   while (multiple-value-setq (nth pos)
	   (parse-integer line :start pos :junk-allowed t))
   if (or (minusp nth) (>= nth nitems))
   do (format *debug-io* (intl:gettext "~&Discarding invalid number ~d.") nth)
   else collect nth into list
   finally
   (let ((keyword
	  (car (rassoc
		(string-right-trim
		 '(#\space #\tab #\newline #\;) (subseq line pos))
		+select-by-keyword-alist+
		:test #'(lambda (item list)
			  (member item list :test #'string-equal))))))
     (unless keyword
       (setq keyword 'noop)
       (format *debug-io* (intl:gettext "~&Ignoring trailing garbage in input.")))
     (return (cons keyword list)))))

(defun select-doc-items (selection items)
  (case (pop selection)
    (noop (loop
	   for i in selection
	   collect (nth i items)))
    (all items)
    (none 'none)))

;; Outputting documentation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MATCHES looks like ((D1 I11 I12 I12 ...) (D2 I21 I22 I23 ...))
;; Rearrange it to
;;   ((D1 I11) (D1 I12) (D1 I13) ... (D2 I21) (D2 I22) (D2 I23) ...)
(defun rearrange-matches (matches)
  (mapcan #'(lambda (di)
              (let ((d (car di)) (i (cdr di)))
                (mapcar #'(lambda (i1) (list d i1)) i)))
          matches))

;; Items looks like ((D1 I11 I12 ...) (D2 I21 I22 ...) ..) where D1, D2 are DOC
;; objects and I11, I12, I21, I22 are DOC-TOPIC objects.
(defun display-items (items)
  (let*
    ((items-list (rearrange-matches items))
     (nitems (length items-list))
     (wanted))

    (when (> nitems 1)
      (loop
         for i from 0 for item in items-list
         do (let ((heading-title (doc-topic-section (second item)))
                  (item-name (doc-topic-name (second item))))
              (format t "~% ~d: ~a~@[  (~a)~]" i item-name heading-title))))

    (setq wanted
          (if (> nitems 1)
              (loop
                 for prompt-count from 0
                 thereis (progn
                           (finish-output *debug-io*)
                           (print-prompt prompt-count)
                           (force-output)
                           (clear-input)
                           (select-doc-items
                            (parse-user-choice nitems) items-list)))
              items-list))
    (clear-input)
    (finish-output *debug-io*)
    (when (consp wanted)
      (format t "~%")
      (loop for item in wanted
        do (format t "~A~%~%" (apply #'documentation-for-topic item))))))

;; Searching within a DOC implementation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun all-doc-regex-matches (regex-strings)
  "Return all matches from documentation in the system for the given regular
expressions: a list keyed by document with matches from that document as the
CDR."
  (let ((regexes (mapcar (lambda (regex-string)
                           (coerce (maxima-nregex::regex-compile
                                    regex-string :case-sensitive nil)
                                   'function))
                         regex-strings)))
    (remove nil
            (mapcar
             (lambda (document)
               (cons document
                     (mapcan (lambda (regex)
                               (documentation-matching-topics document regex))
                             regexes)))
             (mapcar #'cdr *documents*))
            :key #'cdr)))

(defun regex-sanitize (s)
  "Precede any regex special characters with a backslash."
  (let
    ((L (coerce maxima-nregex::*regex-special-chars* 'list)))

    ; WORK AROUND NREGEX STRANGENESS: CARET (^) IS NOT ON LIST *REGEX-SPECIAL-CHARS*
    ; INSTEAD OF CHANGING NREGEX (WITH POTENTIAL FOR INTRODUCING SUBTLE BUGS)
    ; JUST APPEND CARET TO LIST HERE
    (setq L (cons #\^ L))

    (coerce (apply #'append
                   (mapcar #'(lambda (c) (if (member c L :test #'eq)
					     `(#\\ ,c) `(,c))) (coerce s 'list)))
            'string)))

(defun topic-match (topic exact-p)
  "Find matches for TOPIC, in the format returned by ALL-DOC-REGEX-MATCHES."
  (ensure-standard-documents)
  (all-doc-regex-matches
   (if exact-p
       (list (concatenate 'string "^" topic "$")
             (concatenate 'string "^" topic " *<[0-9]+>$"))
       (list (regex-sanitize topic)))))
