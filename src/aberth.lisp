;;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

(in-package :maxima)

;;; Aberth method for solving roots of a polynomial
;;; https://en.wikipedia.org/wiki/Aberth_method

(defmvar $aberth_debug_level 0
  "Debug level.  Higher values produce more debugging output from
  various parts of the algorithm")

;; What method to use for convergence.  Convergence can be determined
;; if the change in the the root value is small enough or if the
;; polnomial value is small enough.  If non-NIL, we use the change in
;; the root value.
(defmvar $aberth_use_roots_for_convergence nil
  "If non-NIL, use the computed root offsets to determine convergence
  instead of using the polynomial value and error bound.")

(defmvar $aberth_max_iterations 100
  "Maximum number of iterations allowed.  If this is exceeded, the
  algorithm did not appear to converge.")

(defvar *aberth-initialize-randomly*
  nil
  "If non-NIL, the initial root estimates are determined randomly.
  Otherwise a deterministic scheme is used.")

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

(defun polyev (p s q)
  "Evaluate the polynomial P at the point S by the horner recurrence,
  placing the partial sums in Q and returning the value of the
  polynomial."
  (setf (aref q 0) (aref p 0))
  (let ((pv (aref p 0)))
    (loop for k from 1 below (length p)
	  do
	     (setf pv (bigfloat:+ (aref p k)
				  (bigfloat:* s pv)))
	     (setf (aref q k) pv))
    pv))

(defun errev-poly (q ms mp are mre)
  "Compute bounds in evaluating the polynomial by the horner recurrence.

  Q    - a vector of the partial sums
  ms   - modulus of the point
  mp   - modulus of the polynomial value
  are  - error bound on complex addition
  mre  - error bound on complex multiplication"

  (let ((e (bigfloat:/ (bigfloat:* (bigfloat:abs (aref q 0))
				   mre)
		       (bigfloat:+ are mre))))
    (loop for k from 0 below (length q)
	  do
	     (bigfloat:incf e
		 (bigfloat:+ (bigfloat:* e ms)
			     (bigfloat:abs (aref q k)))))
    (bigfloat:- (bigfloat:* e (bigfloat:+ are mre))
		(bigfloat:* mp mre))))
			    
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
	      while (bigfloat:> (bigfloat:abs delta) (bigfloat:* bnd eps))
	      do
		 (setf bnd (bigfloat:- bnd delta)))
	bnd))))

(defun scale-poly (p)
  "Compute a scaling of the roots of the polynomial and return the
  scaling factor and the modified polynomial."
  ;; Compute a scaling factor for the roots of the polynomial.  We
  ;; know that |p[n]/p[0]| is the product of all the roots.  So set
  ;; compute s:
  ;;
  ;;   s = |p[n]/p[0]|^(1/n)
  ;;
  ;; and the scaling factor f = -truncate(log2(s)).  We use a power of
  ;; two so as not to introduce any additional rounding error.
  ;;
  ;; See https://en.wikipedia.org/wiki/Polynomial_transformation#Scaling_the_roots.
  (let* ((degree (1- (length p)))
	 (f (- (bigfloat:truncate
		(bigfloat:/ (bigfloat:log 
			     (bigfloat:/ (bigfloat:abs (aref p degree))
					 (bigfloat:abs (aref p 0)))
			     2)
			    degree)))))
    (loop for k from 1 to degree
	  for c = (bigfloat:/ (aref p k) (aref p 0))
	  do
	     (setf (aref p k)
		   (bigfloat:/
		    (bigfloat:complex
		     (bigfloat:scale-float (bigfloat:realpart (aref p k)) (* k f))
		     (bigfloat:scale-float (bigfloat:imagpart (aref p k)) (* k f)))
		    (aref p 0))))
    (setf (aref p 0) (bigfloat:float 1 (bigfloat:realpart (aref p 0))))
    f))
    
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
  (let* ((w (make-array degree))
	 (pz (make-array degree))
	 (err (make-array degree))
	 (q (make-array (1+ degree)))
	 (are (bigfloat:epsilon (aref p 0)))
	 (mre (bigfloat:* 2
			  (bigfloat:sqrt 2)
			  are)))
    (loop for k from 0 below degree
	  do
	     (setf (aref pz k) (polyev p (aref roots k) q))
	     (setf (aref err k) (errev-poly p
					    (bigfloat:abs (aref roots k))
					    (bigfloat:abs (aref pz k))
					    are
					    mre))
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
    (values w pz err)))

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

(defun aberth-converges-p (roots w pz err)
  "Determine if the algorithm has converged based on the value of the
  polynomial at each root and the corresponding error bound on the
  value of the polynomial.  PZ is a vector of the polynomial values,
  and ERR is a vector of the corresponding bound on the value."
  ;; Convergence occurs if every polynomial value less than 20 times
  ;; the error bound.  This comes from cpoly.
  (let ((degree (length pz))
	(eps (bigfloat:epsilon (aref pz 0))))
    (if $aberth_use_roots_for_convergence
	(loop for k from 0 below degree
	      always (bigfloat:<= (bigfloat:abs (aref w k))
				  (bigfloat:* eps
					      (bigfloat:abs (aref roots k)))))
	(loop for k from 0 below degree
	      always (bigfloat:<= (bigfloat:abs (aref pz k))
				  (bigfloat:* 1 (aref err k)))))))

  
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
     (let ((p (make-array (1+ degree) :initial-element 0d0)))
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
		       (setf (aref p l) (bigfloat:complex pr-sl pi-sl)))))
	   ;; This should catch expressions like sin(x)-x
	   (aberth-roots-err expr1))
       ;; Setup is done and we've determined polynomial and the coefficients.
       ;;
       ;; p is an array of the coefficients in descending order.
       (when (>= $aberth_debug_level 10)
	 (format t "p = ~A~%" p))

       (let ((leading-coef (aref p 0))
	     (scale (scale-poly p))
	     (p1 (make-array degree :initial-element 0d0)))
	 (when (>= $aberth_debug_level 10)
	   (format t "scaled p = ~A~%" p))

	 ;; Compute derivative
	 (loop for k from 0 below degree do
	   (setf (aref p1 k) (bigfloat:* (aref p k)
					      (- degree k))))
	 (when (>= $aberth_debug_level 10)
	   (format t "p1 = ~A~%" p1))

	 ;; Find upper and lower bounds for the roots of the polynomial.
	 (multiple-value-bind (bnd-lo bnd-hi)
	     (compute-bounds p)
	   (when (>= $aberth_debug_level 6)
	     (format t "scale: ~A~%" scale)
	     (format t "bounds: ~A ~A~%" bnd-lo bnd-hi))
	   (let* ((roots (initialize-roots degree bnd-lo bnd-hi))
		  (conv
		    ;; Run Aberth's algorithm until it converges or until we
		    ;; tried enough times.
		    (loop for k from 0 below $aberth_max_iterations
			  do
			     (multiple-value-bind (w pz err)
				 (compute-offsets p p1 roots degree)
			       (when (>= $aberth_debug_level 2)
				 (format t "~2D: r   ~A~%" k roots)
				 (format t "     w   ~A~%" w)
				 (format t "     pz  ~A~%" pz)
				 (format t "     err ~A~%" err))

			       (when (aberth-converges-p roots w pz err)
				 (return k))
			       (map-into roots #'bigfloat:-
					 roots w)))))
	     (when (or (not conv)
		       (>= $aberth_debug_level 1))
	       (format t "~:[Failed to converge~;Converged~] after ~A iterations.~%"
		       conv (or conv $aberth_max_iterations)))
	     ;; Undo the scaling
	     (map-into roots
		       (lambda (r)
			 (bigfloat:complex
			  (bigfloat:scale-float (bigfloat:realpart r) (- scale))
			  (bigfloat:scale-float (bigfloat:imagpart r) (- scale))))
		       roots)
	     (let ((res
		     ;; If polyfactor is true, return a list of the
		     ;; form (x - root) Otherwise, return a list of
		     ;; the form x = root.
		     (loop for k from 0 below degree
			   if $polyfactor
			     collect (if (bigfloat:zerop (bigfloat:imagpart (aref roots k)))
					 (add var (mul -1 (to (bigfloat:realpart (aref roots k)))))
					 (add var (mul -1
						       (to (aref roots k)))))
			   else
			     ;; Return the results in the form [x = r1, x = r2, ...].
			     collect (simplify (list '(mequal) var (to (aref roots k)))))))
	       ;; If polyfactor, multiply all the terms together and
	       ;; then multiply by the leading coefficient of the
	       ;; polynomial.  Otherwise, just make it a maxima list.
	       (if $polyfactor
		   (mul (to leading-coef)
			(reduce #'mul res))
		   (cons '(mlist) res)))))))))

(defmfun $aberth_roots (expr)
  (aberth-roots expr #'$float))

(defmfun $bf_aberth_roots (expr)
  (aberth-roots expr #'bigfloat:bigfloat))
