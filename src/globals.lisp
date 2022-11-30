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

;; From mdot.lisp
(defmvar $dotconstrules t
  "Causes a non-commutative product of a constant and
another term to be simplified to a commutative product.  Turning on this
flag effectively turns on DOT0SIMP, DOT0NSCSIMP, and DOT1SIMP as well.")

(defmvar $dot0simp t
  "Causes a non-commutative product of zero and a scalar term to
be simplified to a commutative product.")

(defmvar $dot0nscsimp t
  "Causes a non-commutative product of zero and a nonscalar term
to be simplified to a commutative product.")

(defmvar $dot1simp t
  "Causes a non-commutative product of one and another term to be
simplified to  a commutative product.")

(defmvar $dotscrules nil
  "Causes a non-commutative product of a scalar and another term to
be simplified to a commutative product.  Scalars and constants are carried
to the front of the expression.")

(defmvar $dotdistrib nil
  "Causes every non-commutative product to be expanded each time it
is simplified, i.e.  A . (B + C) will simplify to A . B + A . C.")

(defmvar $dotexptsimp t "Causes A . A to be simplified to A ^^ 2.")

(defmvar $dotassoc t
  "Causes a non-commutative product to be considered associative, so
that A . (B . C) is simplified to A . B . C.  If this flag is off, dot is
taken to be right associative, i.e.  A . B . C is simplified to A . (B . C).")

(defmvar $doallmxops t
  "Causes all operations relating to matrices (and lists) to be
carried out.  For example, the product of two matrices will actually be
computed rather than simply being returned.  Turning on this switch
effectively turns on the following three.")

(defmvar $domxmxops t "Causes matrix-matrix operations to be carried out.")

(defmvar $doscmxops nil "Causes scalar-matrix operations to be carried out.")

(defmvar $domxnctimes nil
  "Causes non-commutative products of matrices to be carried out.")

(defmvar $scalarmatrixp t
  "Causes a square matrix of dimension one to be converted to a
scalar, i.e. its only element.")

(defmvar $dotident 1 "The value to be returned by X^^0.")

(defmvar $assumescalar t
  "This governs whether unknown expressions 'exp' are assumed to behave
like scalars for combinations of the form 'exp op matrix' where op is one of
{+, *, ^, .}.  It has three settings:

FALSE -- such expressions behave like non-scalars.
TRUE  -- such expressions behave like scalars only for the commutative
	 operators but not for non-commutative multiplication.
ALL   -- such expressions will behave like scalars for all operators
	 listed above.

Note:  This switch is primarily for the benefit of old code.  If possible,
you should declare your variables to be SCALAR or NONSCALAR so that there
is no need to rely on the setting of this switch.")

(defmvar $%rnum 0)

(defmvar $%rnum_list '((mlist))
  "Upon exit from ALGSYS this is bound to a list of the %RNUMS
which where introduced into the expression. Useful for mapping over
and using as an argument to SUBST.")

;; From mat.lisp
(defmvar $globalsolve nil)
(defmvar $sparse nil)
(defmvar $backsubst t)

;; Probably should be defvar and not defmvar.
(defmvar *rank* nil)
(defmvar *inv* nil)

;; From float.lisp
(defmvar $float2bf t
  "If TRUE, no MAXIMA-ERROR message is printed when a floating point number is
converted to a bigfloat number.")


(defmvar $bftorat nil
  "Controls the conversion of bigfloat numbers to rational numbers.  If
FALSE, RATEPSILON will be used to control the conversion (this results in
relatively small rational numbers).  If TRUE, the rational number generated
will accurately represent the bigfloat.")

(defmvar $bftrunc t
  "If TRUE, printing of bigfloat numbers will truncate trailing zeroes.
  Otherwise, all trailing zeroes are printed.")

(defmvar $fpprintprec 0
  "Controls the number of significant digits printed for floats.  If
  0, then full precision is used."
  fixnum)

(defmvar $maxfpprintprec (ceiling (log (expt 2 (float-digits 1d0)) 10d0))
  "The maximum number of significant digits printed for floats.")

(defmvar $fpprec $maxfpprintprec
  "Number of decimal digits of precision to use when creating new bigfloats.
One extra decimal digit in actual representation for rounding purposes.")



;; From solve.lisp
(defmvar $breakup t
  "Causes solutions to cubic and quartic equations to be expressed in
	 terms of common subexpressions.")

(defmvar $multiplicities '$not_set_yet
  "Set to a list of the multiplicities of the individual solutions
	 returned by SOLVE, REALROOTS, or ALLROOTS.")

(defmvar $linsolvewarn t
  "Needs to be documented.")

(defmvar $programmode t
  "Causes SOLVE to return its answers explicitly as elements
	 in a list rather than printing E-labels.")

(defmvar $solvedecomposes t
  "Causes `solve' to use `polydecomp' in attempting to solve polynomials.")

(defmvar $solveexplicit nil
  "Causes `solve' to return implicit solutions i.e. of the form F(x)=0.")

(defmvar $solvefactors t
  "If T, then SOLVE will try to factor the expression.  The FALSE
	 setting may be desired in zl-SOME cases where factoring is not
	 necessary.")

(defmvar $solvenullwarn t
  "Causes the user will be warned if SOLVE is called with either a
	 null equation list or a null variable list.  For example,
	 SOLVE([],[]); would print two warning messages and return [].")

(defmvar $solvetrigwarn t
  "Causes SOLVE to print a warning message when it is uses
	 inverse trigonometric functions to solve an equation,
	 thereby losing solutions.")

(defmvar $solveradcan nil
  "SOLVE will use RADCAN which will make SOLVE slower but will allow
	 certain problems containing exponentials and logs to be solved.")


;;
(defvar $activecontexts '((mlist))
  "A list of the currently activated contexts")

(defmvar $algebraic nil)

(defmvar $exptdispflag t
  "When true, Maxima displays expressions with negative exponents using quotients.")
