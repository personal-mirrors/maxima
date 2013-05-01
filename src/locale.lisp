;; This file contains code that is shared by both Maxima (loaded up through our
;; entry in maxima.system) and the stand-alone .info file parser.
;;
;; Useful "entry points":
;;
;;   - locale-subdir
;;   - locale-subdir-external-format

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
          #+(and ecl unicode) '(ext:latin-1 ext:utf-8 ext:windows-1251)
          #+(or (and clisp (not unicode))
                (and ecl (not unicode))
                gcl) nil))

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
