;; This file contains code for reading in documentation in the .info format. We
;; have already generated it using Texinfo at compile time. The complicated
;; parsing code in lisp-utils/parse-info.lisp is run (also at compile-time) to
;; generate maxima-info-offsets-<foo>.lisp, where <foo> denotes the name of the
;; lisp implementation.
;;
;; The code in this file, by contrast, is very simple. All this does is read in
;; the offsets file and use it to calculate the chunk to print for the user when
;; he or she hits ?? or ?.
(in-package :cl-info)

;; Note: There is also an info-doc class in the :parse-info package, defined in
;; lisp-utils/parse-info.lisp. While these both represent an info document, they
;; are definitely not the same thing. That one stores careful data about nodes /
;; sections and their positions in the various files. This one just stores a
;; load of offsets for file-position.
(defclass info-doc (doc)
  ((topics :reader info-doc-topics :initarg :topics)
   (pathname :reader info-doc-pathname :initarg :pathname)))

(defclass info-topic (doc-topic)
  ((start :reader info-topic-start :initarg :start)
   (length :reader info-topic-length :initarg :length)
   (relpath :reader info-topic-relpath :initarg :relpath)))

;; TODO: This currently throws a warning on compilation since *maxima-lispname*
;; is both defined and given in init-cl, which is loaded much later. Maybe we
;; need to rejig that a bit?
(defun info-offset-name (pathname)
  (merge-pathnames
   (make-pathname
    :name (format nil "~A-info-offsets-~A"
                  (pathname-name pathname) maxima::*maxima-lispname*)
    :type "lisp")
   pathname))

(defmacro with-open-info-file ((stream pathname &rest options) &body body)
  "Basically WITH-OPEN-FILE, but gets the EXTERNAL-FORMAT argument right if
possible. On a lisp that doesn't support the given external format, we shouldn't
error, but the resulting text stream might well be garbage. The encoding is
guessed from the directory name - if we don't understand it we default
to :latin1."
  (let ((pn (gensym)))
    `(let ((,pn ,pathname)
           ;; I think that CMUCL doesn't compile in character conversion
           ;; routines until they get called. As a result, there's an ugly
           ;; signed word -> integer conversion compiler note that appears when
           ;; we first call READ-LINE or similar here. As a workaround, we just
           ;; tell CMUCL to shut up about it...
           #+cmucl (extensions:*efficiency-note-cost-threshold* 50))
       (with-open-file
           ;; On GCL, complicated things like external formats are passed over. In
           ;; fact, there's not even a keyword argument with that name.
           #+gcl (,stream ,pathname ,@options)
           #-gcl (,stream ,pn ,@options
                  :external-format (maxima::locale-subdir-external-format
                                    (car (last (pathname-directory ,pn)))))
           ,@body))))

;; Load up the offsets for the info file at PATHNAME
(defun load-info (pathname)
  (with-open-info-file (stream (info-offset-name pathname)
                               :if-does-not-exist nil)
    (unless stream
      (maxima::merror "Cannot read offset file ~S for info document."
                      (info-offset-name pathname)))
    (let ((*read-eval* nil))
      (info-offsets-to-doc pathname (read stream)))))

;; Make an info-doc object from the list of offsets (presumably just read from
;; some offsets file)
(defun info-offsets-to-doc (pathname offsets)
  (make-instance
   'info-doc
   :name "maxima.info"
   :pathname pathname
   :topics (mapcar
            (lambda (lst)
              (destructuring-bind (name filename start length section) lst
                (make-instance 'info-topic
                               :name name :start start :length length
                               :relpath (parse-namestring filename)
                               :section section)))
            offsets)))

;; TODO: Update this API!
(defmethod documentation-all-topics ((doc info-doc))
  (list (info-doc-topics doc)))

(defmethod documentation-for-topic ((doc info-doc) (topic info-topic))
  (read-info-text (merge-pathnames (info-topic-relpath topic)
                                   (info-doc-pathname doc))
                  (info-topic-start topic)
                  (info-topic-length topic)))

(defun read-info-text (pathname position length)
  (with-open-info-file (in pathname)
    (file-position in position)
    ;; Read a line at a time and check to see whether we've got enough. This
    ;; will definitely do the right thing wrt to FILE-POSITION and it doesn't
    ;; really matter how fast it is: this is text we're outputting to the
    ;; user!
    (reduce (lambda (a b) (concatenate 'string a `(#\Newline) b))
            (collecting-loop
              (let ((line (read-line in :eof nil)))
                (when (eq line :eof) (return))
                (collect line)
                (when (>= (file-position in) (+ position length)) (return)))))))

(defun info-pathnamep (x)
  (and (pathnamep x)
       (string= (pathname-type x) "info")))

(register-documentation-type 'info #'info-pathnamep #'load-info)
