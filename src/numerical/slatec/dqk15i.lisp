;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(let ((xgk (make-array 8 :element-type 'double-float))
      (wgk (make-array 8 :element-type 'double-float))
      (wg (make-array 8 :element-type 'double-float)))
  (declare (type (simple-array double-float (8)) wg wgk xgk))
  (f2cl-lib:fset (f2cl-lib:fref xgk (1) ((1 8))) 0.9914553711208126)
  (f2cl-lib:fset (f2cl-lib:fref xgk (2) ((1 8))) 0.9491079123427585)
  (f2cl-lib:fset (f2cl-lib:fref xgk (3) ((1 8))) 0.8648644233597691)
  (f2cl-lib:fset (f2cl-lib:fref xgk (4) ((1 8))) 0.7415311855993943)
  (f2cl-lib:fset (f2cl-lib:fref xgk (5) ((1 8))) 0.5860872354676911)
  (f2cl-lib:fset (f2cl-lib:fref xgk (6) ((1 8))) 0.4058451513773972)
  (f2cl-lib:fset (f2cl-lib:fref xgk (7) ((1 8))) 0.2077849550078985)
  (f2cl-lib:fset (f2cl-lib:fref xgk (8) ((1 8))) 0.0)
  (f2cl-lib:fset (f2cl-lib:fref wgk (1) ((1 8))) 0.02293532201052922)
  (f2cl-lib:fset (f2cl-lib:fref wgk (2) ((1 8))) 0.06309209262997854)
  (f2cl-lib:fset (f2cl-lib:fref wgk (3) ((1 8))) 0.1047900103222502)
  (f2cl-lib:fset (f2cl-lib:fref wgk (4) ((1 8))) 0.1406532597155259)
  (f2cl-lib:fset (f2cl-lib:fref wgk (5) ((1 8))) 0.1690047266392679)
  (f2cl-lib:fset (f2cl-lib:fref wgk (6) ((1 8))) 0.1903505780647854)
  (f2cl-lib:fset (f2cl-lib:fref wgk (7) ((1 8))) 0.2044329400752989)
  (f2cl-lib:fset (f2cl-lib:fref wgk (8) ((1 8))) 0.2094821410847278)
  (f2cl-lib:fset (f2cl-lib:fref wg (1) ((1 8))) 0.0)
  (f2cl-lib:fset (f2cl-lib:fref wg (2) ((1 8))) 0.1294849661688697)
  (f2cl-lib:fset (f2cl-lib:fref wg (3) ((1 8))) 0.0)
  (f2cl-lib:fset (f2cl-lib:fref wg (4) ((1 8))) 0.2797053914892767)
  (f2cl-lib:fset (f2cl-lib:fref wg (5) ((1 8))) 0.0)
  (f2cl-lib:fset (f2cl-lib:fref wg (6) ((1 8))) 0.3818300505051189)
  (f2cl-lib:fset (f2cl-lib:fref wg (7) ((1 8))) 0.0)
  (f2cl-lib:fset (f2cl-lib:fref wg (8) ((1 8))) 0.4179591836734694)
  (defun dqk15i (f boun inf a b result abserr resabs resasc)
    (declare (type f2cl-lib:integer4 inf)
     (type double-float resasc resabs abserr result b a boun)
     (type (function (double-float) (values double-float &rest t)) f))
    (prog ((fv1 (make-array 7 :element-type 'double-float))
           (fv2 (make-array 7 :element-type 'double-float)) (j 0) (absc 0.0)
           (absc1 0.0) (absc2 0.0) (centr 0.0) (dinf 0.0) (epmach 0.0) (fc 0.0)
           (fsum 0.0) (fval1 0.0) (fval2 0.0) (hlgth 0.0) (resg 0.0) (resk 0.0)
           (reskh 0.0) (tabsc1 0.0) (tabsc2 0.0) (uflow 0.0) (abs$ 0.0f0))
      (declare (type single-float abs$)
       (type (simple-array double-float (7)) fv2 fv1)
       (type double-float uflow tabsc2 tabsc1 reskh resk resg hlgth fval2 fval1
        fsum fc epmach dinf centr absc2 absc1 absc)
       (type f2cl-lib:integer4 j))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf dinf
              (coerce
               (the f2cl-lib:integer4
                    (min (the f2cl-lib:integer4 1)
                         (the f2cl-lib:integer4 inf)))
               'double-float))
      (setf centr (* 0.5 (+ a b)))
      (setf hlgth (* 0.5 (- b a)))
      (setf tabsc1 (+ boun (/ (* dinf (- 1.0 centr)) centr)))
      (setf fval1
              (multiple-value-bind
                  (ret-val var-0)
                  (funcall f tabsc1)
                (declare (ignore))
                (when var-0 (setf tabsc1 var-0))
                ret-val))
      (if (= inf 2) (setf fval1 (+ fval1 (funcall f (- tabsc1)))))
      (setf fc (/ (/ fval1 centr) centr))
      (setf resg (* (f2cl-lib:fref wg (8) ((1 8))) fc))
      (setf resk (* (f2cl-lib:fref wgk (8) ((1 8))) fc))
      (setf resabs (coerce (abs resk) 'double-float))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 7) nil)
        (tagbody
          (setf absc (* hlgth (f2cl-lib:fref xgk (j) ((1 8)))))
          (setf absc1 (- centr absc))
          (setf absc2 (+ centr absc))
          (setf tabsc1 (+ boun (/ (* dinf (- 1.0 absc1)) absc1)))
          (setf tabsc2 (+ boun (/ (* dinf (- 1.0 absc2)) absc2)))
          (setf fval1
                  (multiple-value-bind
                      (ret-val var-0)
                      (funcall f tabsc1)
                    (declare (ignore))
                    (when var-0 (setf tabsc1 var-0))
                    ret-val))
          (setf fval2
                  (multiple-value-bind
                      (ret-val var-0)
                      (funcall f tabsc2)
                    (declare (ignore))
                    (when var-0 (setf tabsc2 var-0))
                    ret-val))
          (if (= inf 2) (setf fval1 (+ fval1 (funcall f (- tabsc1)))))
          (if (= inf 2) (setf fval2 (+ fval2 (funcall f (- tabsc2)))))
          (setf fval1 (/ (/ fval1 absc1) absc1))
          (setf fval2 (/ (/ fval2 absc2) absc2))
          (f2cl-lib:fset (f2cl-lib:fref fv1 (j) ((1 7))) fval1)
          (f2cl-lib:fset (f2cl-lib:fref fv2 (j) ((1 7))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resg (+ resg (* (f2cl-lib:fref wg (j) ((1 8))) fsum)))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (j) ((1 8))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (j) ((1 8)))
                        (+ (abs fval1) (abs fval2)))))
         label10))
      (setf reskh (* resk 0.5))
      (setf resasc (* (f2cl-lib:fref wgk (8) ((1 8))) (abs (- fc reskh))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 7) nil)
        (tagbody
          (setf resasc
                  (+ resasc
                     (* (f2cl-lib:fref wgk (j) ((1 8)))
                        (+ (abs (- (f2cl-lib:fref fv1 (j) ((1 7))) reskh))
                           (abs (- (f2cl-lib:fref fv2 (j) ((1 7))) reskh))))))
         label20))
      (setf result (* resk hlgth))
      (setf resasc (* resasc hlgth))
      (setf resabs (* resabs hlgth))
      (setf abserr (coerce (abs (* (- resk resg) hlgth)) 'double-float))
      (if (and (/= resasc 0.0) (/= abserr 0.0))
          (setf abserr
                  (* resasc (min 1.0 (expt (/ (* 200.0 abserr) resasc) 1.5)))))
      (if (> resabs (/ uflow (* 50.0 epmach)))
          (setf abserr (max (* epmach 50.0 resabs) abserr)))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil result abserr resabs resasc)))))

