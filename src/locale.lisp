;; This file contains code that is shared by both Maxima (loaded up through our
;; entry in maxima.system) and the stand-alone .info file parser.
;;
;; Useful "entry points":
;;
;;   - locale-subdir
;;   - locale-subdir-external-format
;;   - adjust-character-encoding

(in-package :maxima)

;; This list is in order of preference (in case we match multiple
;; entries). General format:
;;
;;   (SUBDIR LANGUAGE TERRITORY DEFAULT-CODESET OTHER-CODESETS)
;;
;; An entry matches on LANGUAGE and TERRITORY if both components are either nil
;; or equal to the environment as strings. If not given, DEFAULT-CODESET is
;; assumed to be :LATIN-1. OTHER-CODESETS lists the other codesets that might
;; appear in the form (name string1 string2 ...). We always prepend a UTF-8
;; recipe to it.
;;
;; There is also a short-hand format where you just give a one-element list:
;; (x). This is the same as (x x).
(defvar *locale-defns*
  '((nil "en")
    ("pt_BR" "pt" "br")
    ("es") ("pt") ("fr") ("de") ("it")
    ("ru" "ru" nil :cp1251 ((:koi8-r ".koi8r" "koi8-r" "koi8r")))))

;; Conses keyed by :latin1, :utf-8, :cp1251, :koi8-r (add more here). This would
;; neatly solve the problem of opening with external formats except (surprise,
;; surprise) GCL doesn't allow an :external-format argument to its OPEN function
;; so we have to special case it elsewhere. AAAARGH.
(defvar *external-formats*
  (mapcar 'cons '(:latin1 :utf-8 :cp1251 :koi8-r)
          #+(or sbcl cmucl scl) '(:latin1 :utf-8 :cp1251 :koi8-r)
          #+(and clisp unicode) '(charset:iso-8859-1 charset:utf-8
                                  charset:cp1251 charset:koi8-r)
          #+allegro '(:latin1 :utf-8 :1251 :koi8-r)
          #+ccl '(:latin1 :utf-8)
          #+(and ecl unicode) '(:latin-1 :utf-8 :|windows-1251| :|koi8r|)
          #+(or (and clisp (not unicode))
                (and ecl (not unicode))
                gcl) nil
          #+abcl '(:iso-8859-1 :utf-8)))

(defun locale-match-p (test-tuple language territory)
  (if (null (cdr test-tuple))
      (equal (car test-tuple) language)
      (every (lambda (test env) (or (not test) (equal test env)))
             (cdr test-tuple)
             (list language territory))))

(defun dir-from-codeset (codeset other-codesets)
  (second
   (find-if (lambda (lst) (find codeset (cddr lst) :test #'equal))
            (cons '(:utf-8 ".utf8" "utf-8" "utf8") other-codesets))))

;; Calculate an external format flag suitable for OPEN in the given locale
;; subdir.
(defun locale-subdir-external-format (subdir-name)
  (let* ((dot-pos (position #\. subdir-name))
         (locale-defn
          (find (subseq subdir-name 0 dot-pos) *locale-defns*
                :key #'car :test #'equal)))
    (or (cdr (assoc
              (or (and dot-pos
                       (car (find (subseq subdir-name dot-pos)
                                  (cons '(:utf-8 ".utf8") (fifth locale-defn))
                                  :key #'second :test #'equal)))
                  (fourth locale-defn)
                  :latin1)
              *external-formats*))
        :default)))

;; Determine the appropriate value of *maxima-lang-subdir*
;;   1. from MAXIMA_LANG_SUBDIR environment variable
;;   2. from INTL::*LOCALE* if (1) fails
(defun locale-subdir ()
  (or (maxima-getenv "MAXIMA_LANG_SUBDIR")
      (unless
          (member intl::*locale* '("" "C" "POSIX" "c" "posix") :test #'equal)
        (let* ((language (string-downcase (subseq intl::*locale* 0 2)))
               (territory (when (eql (position #\_ intl::*locale*) 2)
                            (string-downcase (subseq intl::*locale* 3 5))))
               (codeset (when (eql (position #\. intl::*locale*) 5)
                          (string-downcase (subseq intl::*locale* 6)))))
          (destructuring-bind (&optional subdir l tr c other-codesets)
              (find-if (lambda (defn)
                         (locale-match-p defn language territory))
                       *locale-defns*)
            (declare (ignore l tr c))
            (when subdir
              (concatenate 'string
                           subdir
                           (dir-from-codeset codeset other-codesets))))))))

;; Return T if KEYWORD names a valid external format. This is only implemented
;; for lisps that don't sort out their own terminal encoding: CCL, ECL and
;; CMUCL. For the others, we just return NIL.
(defun valid-external-format-p (keyword)
  (declare (ignorable keyword))
  nil
  #+ccl
  (nth-value 0 (ignore-errors
                 (and (ccl:make-external-format :character-encoding keyword) t)))
  #+cmucl (among keyword (stream:list-all-external-formats))
  ;; I can't work out how to test on ECL, so I suppose we'll just have to
  ;; hope. Maybe we'll fail messily when trying to set the thing. Ho hum.
  #+(and ecl unicode) t)

;; Guess an external format to use, based on intl:*locale*. If it's not
;; specified, assume UTF-8.
(defun guess-external-format ()
  (let* ((dot-pos (position #\. intl:*locale*)))
    (if dot-pos
        (intern (string-upcase (subseq intl:*locale* (1+ dot-pos))) :keyword)
        :utf-8)))

;; Tell the terminal to use a sensible encoding, based on the information we can
;; glean from the various LC_* variables.
(defun set-terminal-encoding ()
  ;; The following lisps just sort this out on their own, without us needing to
  ;; get involved
  ;;   CLISP, SCL, ACL, SBCL
  ;;
  ;; GCL doesn't do external formats, so we may as well just ignore
  ;; it. Similarly with ECL when unicode support isn't compiled in.
  ;;
  ;; When doing it ourselves, we can't just copy clisp or sbcl, since they do
  ;; the work by calling out nl_langinfo and we don't have a portable FFI
  ;; guaranteed. So we may as well be slightly half-hearted about it: use
  ;; intl:*locale*; strip the stuff after the dot; intern it and then check the
  ;; result is a valid external format.
  (let ((external-fmt (guess-external-format)))
    (when (valid-external-format-p external-fmt)
      ;; On CCL, see the bug report http://trac.clozure.com/ccl/ticket/912, from
      ;; which we get the following incantation. (TODO: This works fine, but
      ;; it's a bit icky to be using an internal variable. What if the interface
      ;; changes?  How do you put this behind an "if fboundp" equivalent?)
      #+ccl
      (mapc (lambda (stream)
              (setf (ccl::stream-external-format stream)
                    (ccl:make-external-format :character-encoding external-fmt)))
            (list (two-way-stream-input-stream  *terminal-io*)
                  (two-way-stream-output-stream *terminal-io*)))
      ;; With a unicode-supporting ECL, we set the external format with the
      ;; function SI:STREAM-EXTERNAL-FORMAT-SET.
      #+(and ecl unicode)
      (when (fboundp 'si:stream-external-format-set)
        (mapc (lambda (stream)
                (funcall 'si:stream-external-format-set stream external-fmt))
              (list (two-way-stream-input-stream  *terminal-io*)
                    (two-way-stream-output-stream *terminal-io*))))
      ;; CMUCL: Use set-system-external-format, as long as it's defined (>=20a)
      #+cmucl (when (fboundp 'stream:set-system-external-format)
                (funcall 'stream:set-system-external-format external-fmt)))))
