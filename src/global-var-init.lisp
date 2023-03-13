;;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

;;;; This file contains initialization of global vars defined in
;;;; globals.lisp.  This is for initializating globals from that file
;;;; that can't be initialized directly in that file because the
;;;; necessary functions aren't available at that time.

(in-package "MAXIMA")

;; Initialize *bigprimes* here instead of globals.lisp because we
;; need the NEXT-PRIME function.
(setf *bigprimes*
	(loop with p = (ash most-positive-fixnum -1)
	      repeat 20
	      do (setq p (next-prime (1- p) -1))
	      collect p))

;; Initialize *alpha and $pointbound.  Since both of these are
;; defmvars, we need to update the *variable-initial-values* hash
;; table appropriately too so they get reset correctly.
(setf *alpha (car *bigprimes*))
(setf (gethash '*alpha *variable-initial-values*)
	(car *bigprimes*))

(setf $pointbound *alpha)
(setf (gethash '$pointbound *variable-initial-values*)
      *alpha)
