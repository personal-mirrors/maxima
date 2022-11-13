;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module trpred)

(defvar wrap-an-is 'is-boole-check "How to verify booleans")

(defun wrap-an-is (exp)
  (cons '$any (list wrap-an-is exp)))

(defun tr-is/maybe (boole-check form)
  (let* ((wrap-an-is boole-check)
         (tr (translate-predicate form)))
    (destructuring-bind (mode . tr-form) tr
      (if (eq mode '$boolean)
          tr
          (cons '$any tr-form)))))

(def%tr $is (form)
  (tr-is/maybe 'is-boole-check (cadr form)))

(def%tr $maybe (form)
  (tr-is/maybe 'maybe-boole-check (cadr form)))

;;; these don't have an imperitive predicate semantics outside of
;;; being used in MNOT, MAND, MOR, MCOND, $IS.

(def%tr mnotequal (form)
  `($any . (simplify (list '(,(caar form)) ,@(tr-args (cdr form))))))

(def-same%tr mequal    mnotequal)
(def-same%tr $equal    mnotequal)
(def-same%tr $notequal mnotequal)
(def-same%tr mgreaterp mnotequal)
(def-same%tr mgeqp     mnotequal)
(def-same%tr mlessp    mnotequal)
(def-same%tr mleqp     mnotequal)

;;; It looks like it was copied from MRG;COMPAR > with 
;;; TRP- substituted for MEVALP. What a crockish way to dispatch,
;;; and in a system with a limited address space too!
;;; NOTE: See code for IS-BOOLE-CHECK, also duplication of MRG;COMPAR.

;;; Note: This TRANSLATE-PREDICATE and TRANSLATE should be combinded
;;; to a single function which takes a second argument of the
;;; TARGET (mode). Targeting is a pretty basic concept in compilation
;;; so its surprising this was done. In order to make this change all
;;; special-forms need to do targeting.

(defun translate-predicate (form)
  (cond ((atom form) (trp-with-boolean-convert form))
	((eq 'mnot (caar form)) (trp-mnot form))
	((eq 'mand (caar form)) (trp-mand form))
	((eq 'mor (caar form)) (trp-mor form))
	((eq 'mnotequal (caar form)) (trp-mnotequal form))
	((eq 'mequal (caar form)) (trp-mequal form))
	((eq '$equal (caar form)) (trp-$equal form))
	((eq '$notequal (caar form)) (trp-$notequal form))
	((eq 'mgreaterp (caar form)) (trp-mgreaterp form))
	((eq 'mgeqp (caar form)) (trp-mgeqp form))
	((eq 'mlessp (caar form)) (trp-mlessp form))
	((eq 'mleqp (caar form)) (trp-mleqp form))
	((eq 'mprogn (caar form))
	 ;; it was a pain not to have this case working, so I just
	 ;; patched it in. Lets try not to lazily patch in every
	 ;; special form in macsyma!
	 (let ((exprs (cdr form)))
	   (destructuring-bind (mode . last)
	       (translate-predicate (car (last exprs)))
	     (cons mode
		   `(progn
		      ,@(tr-args (butlast exprs))
		      ,last)))))
	(t (trp-with-boolean-convert form))))

(defun trp-with-boolean-convert (form)
  (let ((tr (translate form)))
    (destructuring-bind (mode . exp) tr
      (if (eq mode '$boolean)
          tr
          (wrap-an-is exp)))))

(defun trp-mnot (form) 
  (setq form (cdr (translate-predicate (cadr form))))
  (cond ((not form)
         (cons '$boolean t))
        ((eq t form)
         (cons '$boolean nil))
        ((and (not (atom form)) (eq (car form) 'not))
         (cons '$any (cadr form)))
        (t
         (cons '$any (list 'not form)))))

(defun trp-mand (form) 
  (setq form (mapcar (lambda (x) (cdr (translate-predicate x))) (cdr form)))
  (do ((l form (cdr l)) (nl))
      ((null l) (cons '$any (cons 'and (nreverse nl))))
    (cond ((car l) (setq nl (cons (car l) nl)))
          (t (return (cons '$any (cons 'and (nreverse (cons nil nl)))))))))

(defun trp-mor (form) 
  (setq form (mapcar (lambda (x) (cdr (translate-predicate x))) (cdr form)))
  (do ((l form (cdr l)) (nl))
      ((null l)
       (cond (nl
              (cond ((null (cdr nl))
                     (cons '$any (car nl)))
                    (t
                     (cons '$any (cons 'or (nreverse nl))))))
             (t
              (cons '$boolean nil))))
    (cond ((car l) (setq nl (cons (car l) nl))))))

(defvar *number-types* '($float $number $fixnum ))

(defun trp-mgreaterp (form) 
  (let (mode arg1 arg2)
    (setq arg1 (translate (cadr form)) arg2 (translate (caddr form))
	  mode (*union-mode (car arg1) (car arg2)))
    (cond ((or (eq '$fixnum mode) (eq '$float mode)
	       (and (member (car arg1) *number-types* :test #'eq)
		    (member (car arg2) *number-types* :test #'eq)))
	   `($boolean . (> ,(dconv arg1 mode) ,(dconv arg2 mode))))
	  ((eq '$number mode)
	   `($boolean . (> ,(cdr arg1) ,(cdr arg2))))
	  ('else
	   (wrap-an-is `(mgrp ,(dconvx arg1) ,(dconvx arg2)))))))
 
(defun trp-mlessp (form) 
  (let (mode arg1 arg2)
    (setq arg1 (translate (cadr form)) arg2 (translate (caddr form))
	  mode (*union-mode (car arg1) (car arg2)))
    (cond ((or (eq '$fixnum mode) (eq '$float mode)
	       (and (member (car arg1) *number-types* :test #'eq)
		    (member (car arg2) *number-types* :test #'eq)))
	   `($boolean . (< ,(dconv arg1 mode) ,(dconv arg2 mode))))
	  ((eq '$number mode)
	   `($boolean . (< ,(cdr arg1) ,(cdr arg2))))
	  ('else
	   (wrap-an-is `(mlsp ,(dconvx arg1) ,(dconvx arg2)))))))

(defun trp-mequal (form) 
  (destructuring-let (((mode1 . arg1) (translate (cadr form)))
                      ((mode2 . arg2) (translate (caddr form))))
    (cons '$boolean
          (if (and (covers '$number mode1) (covers '$number mode2))
              `(eql ,arg1 ,arg2)
              `(like ,arg1 ,arg2)))))

(defun trp-$equal (form) 
  (let (mode arg1 arg2) 
    (setq arg1 (translate (cadr form)) arg2 (translate (caddr form))
	  mode (*union-mode (car arg1) (car arg2)))
    (cond ((or (eq '$fixnum mode) (eq '$float mode))
	   `($boolean . (= ,(dconv arg1 mode) ,(dconv arg2 mode))))
	  ((eq '$number mode)
	   `($any . (meqp ,(cdr arg1) ,(cdr arg2))))
	  ('else
	   (wrap-an-is `(meqp ,(dconvx arg1) ,(dconvx arg2)))))))

;; Logical not for predicates.  Do the expected thing, except return
(defun trp-not (val)
  (case val
    ((t) nil)
    ((nil) t)
    (otherwise val)))

(defun trp-$notequal (form)
  (cons '$any (list 'trp-not (cdr (trp-$equal form)))))

(defun trp-mnotequal (form)
  (cons '$any (list 'trp-not (cdr (trp-mequal form)))))

(defun trp-mgeqp (form)
  (cons '$any (list 'trp-not (cdr (trp-mlessp form)))))

(defun trp-mleqp (form)
  (cons '$any (list 'trp-not (cdr (trp-mgreaterp form)))))

;;; sigh, i have to copy a lot of the $assume function too.

(def%tr $assume (form)
  (let ((x (cdr form)))
    (do ((nl))
	((null x)
	 `($any . (simplify (list '(mlist) ,@(nreverse nl)))))
      (cond ((atom (car x))
	     (setq nl (cons `(assume ,(dtranslate (car x))) nl)))
	    ((eq 'mand (caaar x))
	     (mapc #'(lambda (l) (setq nl (cons `(assume ,(dtranslate l)) nl)))
		   (cdar x)))
	    ((eq 'mnot (caaar x))
	     (setq nl (cons `(assume ,(dtranslate (pred-reverse (cadar x)))) nl)))
	    ((eq 'mor (caaar x))
	     (merror (intl:gettext "assume: argument cannot be an 'or' expression; found ~M") (car x)))
	    ((eq (caaar x) 'mequal)
	     (merror (intl:gettext "assume: argument cannot be an '=' expression; found ~M~%assume: maybe you want 'equal'.") (car x)))
	    ((eq (caaar x) 'mnotequal)
	     (merror (intl:gettext "assume: argument cannot be a '#' expression; found ~M~%assume: maybe you want 'not equal'.") (car x)))
	    ('else
	     (setq nl (cons `(assume ,(dtranslate (car x))) nl))))
      (setq x (cdr x)))))
