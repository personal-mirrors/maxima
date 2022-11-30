;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

;;; This file contains global vars (defvars/defmvars) that are used in
;;; multiple files.  We gather them all here so that they are
;;; consistently defined across the build and to make the dependencies
;;; easier to track.

(in-package "MAXIMA")

(defvar infinities '($inf $minf $infinity)
  "The types of infinities recognized by Maxima.
   INFINITY is complex infinity")

(defvar real-infinities '($inf $minf)
  "The real infinities, `inf' is positive infinity, `minf' negative infinity")

(defvar infinitesimals '($zeroa $zerob)
  "The infinitesimals recognized by Maxima. ZEROA zero from above,
   ZEROB zero from below")

;; Declare user-visible special variables.
;; Most of these come from lmdcls.lisp

(defvar *reset-var* t)

(defvar *variable-initial-values* (make-hash-table)
  "Hash table containing all Maxima defmvar variables and their initial
values")

(defmacro defmvar (var &rest val-and-doc)
  "If *reset-var* is true then loading or eval'ing will reset value, otherwise like defvar"
  (cond ((> (length val-and-doc) 2)
	 (setq val-and-doc (list (car val-and-doc) (second val-and-doc)))))
  `(progn
    (unless (gethash ',var *variable-initial-values*)
      (setf (gethash ',var *variable-initial-values*)
	    ,(first val-and-doc)))
    (defvar ,var ,@val-and-doc)))

(defmvar $% '$% "The last out-line computed, corresponds to lisp *"
	 no-reset)

(defmvar $%edispflag nil)

(defmvar $%emode t)

(defmvar $%enumer nil)

(defmvar $%e_to_numlog nil)

;; From trigi.lisp
(defmvar $%iargs t)
(defmvar $%piargs t)
(defmvar $triginverses t)
(defmvar $trigexpand nil)
(defmvar $trigexpandplus t)
(defmvar $trigexpandtimes t)
(defmvar $trigsign t)
(defmvar $exponentialize nil)
(defmvar $logarc nil)
(defmvar $halfangles nil)

;; From suprv1.lisp
(defmvar $disptime nil)
(defmvar $strdisp t)
(defmvar $grind nil)
(defmvar $backtrace '$backtrace)
(defmvar $debugmode nil)
(defmvar $poislim 5)
(defmvar $loadprint nil)
(defmvar $nolabels nil)
(defmvar $aliases '((mlist simp)))

(defmvar $%rnum 0)

(defmvar $%rnum_list '((mlist))
  "Upon exit from ALGSYS this is bound to a list of the %RNUMS
which where introduced into the expression. Useful for mapping over
and using as an argument to SUBST.")

(defvar $activecontexts '((mlist))
  "A list of the currently activated contexts")

(defmvar $algebraic nil)

(defmvar $exptdispflag t
  "When true, Maxima displays expressions with negative exponents using quotients.")
