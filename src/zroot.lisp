;;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;
;;;;
;;;; Copyright 2021, Raymond Toy
;;;;
;;; Written by Raymond Toy and contributed to Maxima.  This code is
;;; placed under same license as Maxima itself.

(in-package :maxima)

(defun conv (p root)
  "Multiply the polynomial P by (x-root).  The coefficients of the
  polynomial are arranged in descending powers."
  ;; Let P = sum(a[k]*x^(n-k), k, 0, n).  Then P*(x-r) is
  ;;
  ;;   sum(a[k]*x^(n-k+1),k,0,n) - sum(r*a[k]*x^(n-k),k,0,n)
  ;;   = a[0]*x^(n+1) + sum((a[k]-r*a[k-1])*x^(n-k), k, 1, n)
  ;;       - r*a[n]
  ;;
  (declare (vector p))
  (let ((degree (1- (length p)))
	(prod (make-array (1+ (length p)))))
    ;; Compute the first and last terms separately.
    (setf (aref prod 0) (aref p 0))
    (setf (aref prod (1+ degree))
	  (- (* root (aref p degree))))

    ;; Compute all the remaining terms:  (a[k] - r*a[k-1])
    (loop for k of-type fixnum from 1 to degree
	  do
	     (setf (aref prod k)
		   (- (aref p k)
		      (* root (aref p (1- k))))))
    prod))

(defun evalg (z multiplicities)
  (declare (vector z multiplicities))
  (let ((m (length z))
	(s #(1)))
    (dotimes (i m)
      (loop for k from 1 to (aref multiplicities i)
	    do
	       (setf s (conv s (aref z i)))))
    s))

(defun evalj (z multiplicities)
  (declare (vector z multiplicities))
  (let* ((m (length z))
	 (n (reduce #'+ multiplicities))
	 (u (evalg z (map 'vector #'1- multiplicities)))
	 (jacobian (make-array (list n m) :initial-element 0)))
    (flet ((mult (u factor)
	     (map 'vector
		  #'(lambda (c)
		      (* factor c))
		  u)))
      (dotimes (j m)
	(let ((s (mult u (- (aref multiplicities j)))))
	  (loop for k from 0 below m
		unless (= k j)
		  do
		     (setf s (conv s (aref z k))))
	  (dotimes (k (length s))
	    (setf (aref jacobian k j) (aref s k)))))
      jacobian)))

(defun solve-sys (w a g j)
  ;; Compute the least-squares solution to the linear system
  ;;
  ;; [W*J]*delta = W*[G - a]
  (let* ((n (array-dimension j 0))
	 (m (array-dimension j 1))
	 (vars (coerce (loop for k from 0 below m
			     collect (gensym (format nil "Z~D_" k)))
		       'vector))
	 eqns)
    (dotimes (r n)
      (let ((eqn 0))
	(dotimes (c m)
	  #+nil
	  (format t "~A ~A~%" (aref vars c)
		  (* (aref w r)
		     (aref j r c)))
	  (setf eqn (add eqn (mul (mul (to (* (aref w r)
					      (aref j r c))))
				  (aref vars c)))))
	(setf eqn (sub eqn
		       (* (aref w r)
			  (- (aref g r)
			     (aref a r)))))
	#+nil
	(mformat t "eqn: ~M" eqn)
	(push eqn eqns)))
    (destructuring-bind ((marker &rest soln) lse info)
	(rest (minpack_lsquares-impl (list* '(mlist) eqns)
				     (list* '(mlist) (coerce vars 'list))
				     (list* '(mlist) (loop for k from 1 to m
							   collect 0))
				     :tolerance 1e-15))
      (declare (ignore marker)
	       (ignorable lse))
      (progn
	(format t "soln ~A~%" soln)
	(format t "lse ~A~%" lse)
	(format t "info ~A~%" info))
      (unless (zerop info)
	#+nil
	(format t "soln ~A~%" (coerce soln 'vector))
	(coerce soln 'vector)))))

    
    
(defun pejroot (a z multiplicities tau)
  (declare (vector a))
  (let ((w (map 'vector
		#'(lambda (c)
		    (min 1 (/ (abs c))))
		a)))
    (flet ((norm (v)
	     (declare (vector v))
	     (let ((sum 0))
	       (dotimes (k (length v))
		 (incf sum (expt (abs (aref v k)) 2)))
	       sum)))
      (loop with old-delta = 0
	    for k from 0
	    do
	       (let* ((g (evalg z multiplicities))
		      (j (evalj z multiplicities))
		      (z-delta (solve-sys w a (subseq g 1) j)))
		 (unless z-delta
		   (error "solving system failed"))
		 (let ((delta (norm z-delta)))
		   (map-into z #'-
			     z
			     z-delta)
		   (format t "old-delta delta ~A ~A~%" old-delta delta)
		   (format t "roots ~A~%" z)
		   (when (>= k 1)
		     (when (>= delta old-delta)
		       (format t "delta old ~A ~A~%" delta old-delta)
		       (error "Failed"))
		     (when (< (/ (* delta delta)
				 (- old-delta delta))
			      tau)
		       (return-from pejroot z)))
		   (setf old-delta delta)))))))
