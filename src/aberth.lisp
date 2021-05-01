;;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

(in-package :maxima)

;;; Aberth method for solving roots of a polynomial
;;; https://en.wikipedia.org/wiki/Aberth_method

(defun aberth-roots-err (expr)
  (merror (intl:gettext "aberth_roots: expected a polynomial; found ~M") expr))

(defun synthetic-div (p z)
  "Compute the value of the polynomial at the point Z.  The polynomial
  is represented as an array of coefficients in descending powers."
  ;; Synthetic division: https://en.wikipedia.org/wiki/Synthetic_division
  (let ((degree (1- (length p)))
	(val (aref p 0)))
    (loop for k from 1 to degree
	  do
	     (setf val (bigfloat:+ (bigfloat:* val z)
				   (aref p k))))
    val))

(defun rouche-bound (p)
  "Rouche upper bound for the polynomial P whose cofficients are
  arranged in a vector in descending powers."
  ;; See
  ;; https://en.wikipedia.org/wiki/Geometrical_properties_of_polynomial_roots#From_Rouch%C3%A9_theorem
  (let ((degree (1- (length p)))
	;; H is the polynomial with coefficents that are the absolute
	;; value of coefficients of p.
	(h (map 'vector #'bigfloat:abs p)))
    ;; Construct finel form of h:
    ;;
    ;;   h(x) = -|a[0]|*x^n + |a[1]|*x^(n-1) + ... + |a[n]|
    ;;
    (setf (aref h 0) (bigfloat:- (aref h 0)))

    ;; We want to find the positive root of h using Newton's algorithm.  To do that, 
    ;; define g(x) = h(x)/x^n with g1(x) = g'(x).
    ;;
    ;; Then
    ;;
    ;;   g(x)  = -|a[0]| + |a[1]|/x + ... |a[n-1]|/x^(n-1) + |a[n]|/x^n
    ;;
    ;;   g1(x) = -|a[1]|/x^2 - ... - (n-1)*|a[n-1]|/x^n - n*|a[n]|/x^(n+1)
    ;;         = x^(-2)*(-|a[1]| - ... - (n-1)*|a[n-1]|/x^(n-1) - n*|a[n]|/x^(n-1))
    ;;
    ;; Note that g(x) is a polynomial in x = 1/y, with the
    ;; coefficients in h in reverse order.  Likewise x^2*g1(x) is a
    ;; polynomial 1/y too.  Construct these polynomials.
    (let ((g (reverse h))
	  (g1 (make-array degree)))
      (loop for k from 1 to degree
	    do
	       (setf (aref g1 (- degree k))
		     (bigfloat:- (bigfloat:* k
					     (aref g (- degree k))))))
      #+nil
      (progn
	(format t "h = ~A~%" h)
	(format t "g = ~A~%" g)
	(format t "g1 = ~A~%" g1))
      ;; Newton's algorithm for a root of g(x).  We take advantage of the fact that g(x) is a polynomial in 1/x so we can use synthetic-div to compute the value.  Likewise g1(x) is a polyn
      (let ((bnd (bigfloat:expt
		  (bigfloat:- (bigfloat:/ (aref h degree)
					  (aref h 0)))
		  (/ degree)))
	    (eps 0.001))
	(loop for delta = (bigfloat:/ (synthetic-div g (bigfloat:/ bnd))
				      (bigfloat:/
				       (synthetic-div g1 (bigfloat:/ bnd))
				       bnd
				       bnd))
	      while (bigfloat:> (bigfloat:abs delta) eps)
	      do
		 (setf bnd (bigfloat:- bnd delta)))
	bnd))))

(defun compute-bounds (p)
  "Compute an estimate of the lower and upper bounds of magnitude of
  the roots of a polynomial.  The coefficients of the polynomial are
  stored in the vector P in descending powers.  This returns two
  values: the lower bound and the upper bound"
  ;; Let the polynomial p(x) be
  ;;
  ;;  p(x) = p[0]*x^n + p[1]*x^(n-1) +  ... + a[1]*x + a[0].
  ;;
  ;; Consider the polynomial
  ;;
  ;;  x^n*p(1/x) = x^n*(p[0]/x^n + p[1]/x^(n-1) + ... + a[1]/x + a[0]
  ;;             = p[0] + p[1]*x + ... + a[1]*x^(n-1) + a[0]*x^n
  ;;
  ;; If we found an upper bound, U, for x^n*p(1/x), then 1/U is a
  ;; lower bound for p(x) since all the roots of x^n*p(1/x) are
  ;; reciprocals of the roots of p(x).  The polynomial x^n*p(1/x) has
  ;; the same form as p(x), except the coefficients are in reverse
  ;; order.
  (values (bigfloat:/ (rouche-bound (reverse p)))
	  (rouche-bound p)))
    
(defun compute-offsets (p p1 roots degree)
  "Compute the offsets according to the Aberth algorithm where P is
  the vector of the coefficients of the polynomial, P1 is the vector
  of coefficients of the derivative, ROOTS is the vector of the
  estimated roots, and DEGREE is the degree of the polynomial.  All of
  the coefficients are arranged in descending powers.  

  We also return in the second value the value of the polynomial
  evaluated at each root."
  ;; As given in https://en.wikipedia.org/wiki/Aberth_method, the
  ;; offset w is computed by
  ;;
  ;;   w[k] = pr(k) / (1 - pr(k) * sum(k))
  ;;
  ;; where, if p(x) is the polynomial and p1(x) is the derivative, then
  ;;
  ;;   pr(k) = p(z[k])/p1(z[k])
  ;;
  ;; and, if z[k] is the k'th root, then
  ;;
  ;;   sum(k) = sum 1/(z[k]-z[j]) for j /= k.
  (let ((w (make-array degree))
	(pz (make-array degree)))
    (loop for k from 0 below degree
	  do
	     (setf (aref pz k) (synthetic-div p (aref roots k)))
	     (let ((pz/p1z (bigfloat:/ (aref pz k)
				       (synthetic-div p1 (aref roots k))))
		   (s 0))
	       ;; Compute sum 1/(zk-zj)
	       (loop for j from 0 below degree
		     unless (= k j)
		       do
			  (setf s (bigfloat:+ s (bigfloat:/ (bigfloat:- (aref roots k)
									  (aref roots j))))))
	       (setf (aref w k)
		     (bigfloat:/ pz/p1z
				 (bigfloat:- 1 (bigfloat:* pz/p1z s))))))
    (values w pz)))

(defvar *aberth-initialize-randomly*
  nil
  "If non-NIL, the initial root estimates are determined randomly.
  Otherwise a deterministic scheme is used.")

(defun initialize-roots (degree bnd-lo bnd-hi)
  "Compute initial guess for roots"
  (let ((roots (make-array degree)))
    (cond (*aberth-initialize-randomly*
	   ;; Randomly select n distinct complex numbers whose
	   ;; absolute values are within the bounds.
	   (let ((range (bigfloat:- bnd-hi bnd-lo)))
	     (loop for k from 0 below degree
		   do
		      ;; root = (lo + random(hi-lo))*exp(%i*random(2*%pi))
		      (setf (aref roots k)
			    (bigfloat:*
			     (bigfloat:+ bnd-lo (bigfloat:random range))
			     (bigfloat:cis (random (* 2 pi))))))))
	  (t
	   ;; Make a spiral starting from the lower bound and going to
	   ;; the upper bound.
	   (let ((mag-step (bigfloat:/ (bigfloat:- bnd-hi bnd-lo) degree))
		 (arg-step (/ (* 2 pi) degree))
		 (mag-val bnd-lo)
		 ;; First point is at 94 degrees.  This is arbitrary,
		 ;; but what allroots does.
		 (arg-val (* 94 (/ (* 2 pi) 180))))
	     (loop for k from 0 below degree
		   do
		      (setf (aref roots k)
			    (bigfloat:* mag-val
					(bigfloat:cis arg-val)))
		      (bigfloat:incf mag-val mag-step)
		      (bigfloat:incf arg-val arg-step)))))
    roots))

(defun aberth-converges-p (roots offsets pz)
  (declare (ignorable pz))
  ;; Converges if |w| <= eps * |root| for all roots.
  ;;
  ;; Consider maybe changing the criteria so that convergence happens
  ;; if the value of the polynomial is as close to zero as we can get
  ;; considering round-off.  allroots does this.
  (loop for k from 0 below (length roots)
	always (bigfloat:<= (bigfloat:abs (aref offsets k))
			    (bigfloat:* (bigfloat:epsilon (aref roots k))
					(bigfloat:abs (aref roots k))))))

  
(defun aberth-roots (expr float-fun)
  "Compute the roots of a polynomial in EXPR using Aberth's algorithm.
  The variable of the polynomial is automatically determined."
  ;; The setup part here is basically stolen from allroots to figure
  ;; out the variable of the polynomial and the corresponding
  ;; coefficients.
  (let (degree *nn* var res $partswitch
	       ($keepfloat t)
	       $demoivre
	       ($listconstvars t)
	       ($algebraic t) complex $ratfac den expr1)
     (setq expr1 (setq expr (meqhk expr)))
     (setq var (delete '$%i (cdr ($listofvars expr)) :test #'eq))
     (or var (setq var (list (gensym))))
     (cond ((not (= (length var) 1))
	    (merror (intl:gettext "expected a polynomial in one variable; found variables ~M")
		    `((mlist) ,@var)))
	   ((setq var (car var))))
     (setq expr ($rat expr '$%i var)
	   res (reverse (car (cdddar expr))))
     (do ((i (- (length res)
		(length (caddar expr)))
	     (1- i)))
	 ((= i 0))
       (setq res (cdr res)))
     (setq den (cddr expr)
	   expr (cadr expr))
     ;; Check denominator is a complex number
    (cond ((numberp den)
	   (setq den (list den 0)))
	  ((eq (car den) (cadr res))
	   (setq den (cddr den))
	   (cond ((numberp (car den))
		  (cond ((null (cddr den))
			 (setq den (list 0 (car den))))
			((numberp (caddr den))
			 (setq den (list (caddr den) (car den))))
			(t
			 (aberth-roots-err expr1))))
		 (t
		  (aberth-roots-err expr1))))
	   (t
	    (aberth-roots-err expr1)))
     ;; If the name variable has disappeared, this is caught here
     (setq *nn* 0)
     (cond ((numberp expr)
	    (setq expr (list expr 0)))
	   ((eq (car expr) (car res))
	    (setq *nn* 1))
	   ((eq (car expr) (cadr res))
	    (setq expr (cddr expr))
	    (cond ((numberp (car expr))
		   (cond ((null (cddr expr))
			  (setq expr (list 0 (car expr))))
			 ((numberp (caddr expr))
			  (setq expr (list (caddr expr) (car expr))))
			 (t
			  (aberth-roots-err expr1))))
		  (t
		   (aberth-roots-err expr1))))
	   (t
	    (aberth-roots-err expr1)))
     (setq degree (cadr expr) *nn* (1+ degree))
     (let ((p  (make-array (1+ degree) :initial-element (bigfloat:bigfloat 0))))
       (or (catch 'notpoly
	     (errset (do ((expr (cdr expr) (cddr expr))
			  (l)
			  (%i (cadr res))
			  (pr-sl 0)
			  (pi-sl 0))
			 ((null expr))
		       (setq l (- degree (car expr))
			     res (cadr expr))
		       (cond ((numberp res)
			      (setf pr-sl (funcall float-fun res)))
			     (t
			      (or (eq (car res) %i)
				  (throw 'notpoly nil))
			      (setq res (cddr res))
			      (setf pi-sl (funcall float-fun (car res)))
			      (setq res (caddr res))
			      (and res (setf pr-sl (funcall float-fun res)))
			      (setq complex t)))
		       (setf (aref p l) (bigfloat:bigfloat pr-sl pi-sl)))))
	   ;; This should catch expressions like sin(x)-x
	   (aberth-roots-err expr1))
       ;; Setup is done and we've determined polynomial and the coefficients.
       ;;
       ;; p is an array of the coefficients in descending order.
       (format t "p = ~A~%" p)
       (let ((p1 (make-array degree :initial-element (bigfloat:bigfloat 0))))
	 ;; Compute derivative
	 (loop for k from 0 below degree do
	   (setf (aref p1 k) (bigfloat:* (aref p k)
					      (- degree k))))
	 #+nil
	 (format t "p1 = ~A~%" p1)

	 ;; Find upper and lower bounds for the roots of the polynomial.
	 (multiple-value-bind (bnd-lo bnd-hi)
	     (compute-bounds p)
	   (format t "bounds: ~A ~A~%" bnd-lo bnd-hi)
	   (let* ((roots (initialize-roots degree bnd-lo bnd-hi)))
	     #+nil
	     (format t "bounds ~A ~A~%" bnd-lo bnd-hi)

	     ;; Run Aberth's algorithm until it converges or until we
	     ;; tried enough times.
	     (loop for k from 0 below 100
		   do
		      (multiple-value-bind (w pz)
			  (compute-offsets p p1 roots degree)
			(when (aberth-converges-p roots w pz)
			  (return t))
			(map-into roots #'bigfloat:-
				  roots w)))
	     ;; Return the results in the form [x = r1, x = r2, ...].
	     (cons '(mlist)
		   (map 'list #'(lambda (r)
				  (simplify (list '(mequal) var (to r))))
			roots))))))))

(defmfun $aberth_roots (expr)
  (aberth-roots expr #'$float))

(defmfun $bf_aberth_roots (expr)
  (aberth-roots expr #'$bfloat))
