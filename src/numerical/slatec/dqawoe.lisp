;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun dqawoe
       (f a b omega integr epsabs epsrel limit icall maxp1 result abserr neval
        ier last$ alist blist rlist elist iord nnlog momcom chebmo)
  (declare (type (array f2cl-lib:integer4 (*)) nnlog iord)
   (type (array double-float (*)) chebmo elist rlist blist alist)
   (type f2cl-lib:integer4 momcom last$ ier neval maxp1 icall limit integr)
   (type double-float abserr result epsrel epsabs omega b a))
  (prog ((rlist2 (make-array 52 :element-type 'double-float))
         (res3la (make-array 3 :element-type 'double-float)) (extrap nil)
         (noext nil) (extall nil) (id 0) (ierro 0) (iroff1 0) (iroff2 0)
         (iroff3 0) (jupbnd 0) (k 0) (ksgn 0) (ktmin 0) (maxerr 0) (nev 0)
         (nres 0) (nrmax 0) (nrmom 0) (numrl2 0) (abseps 0.0) (area 0.0)
         (area1 0.0) (area12 0.0) (area2 0.0) (a1 0.0) (a2 0.0) (b1 0.0)
         (b2 0.0) (correc 0.0) (defab1 0.0) (defab2 0.0) (defabs 0.0)
         (domega 0.0) (dres 0.0) (epmach 0.0) (erlarg 0.0) (erlast 0.0)
         (errbnd 0.0) (errmax 0.0) (error1 0.0) (erro12 0.0) (error2 0.0)
         (errsum 0.0) (ertest 0.0) (oflow 0.0) (resabs 0.0) (reseps 0.0)
         (small 0.0) (uflow 0.0) (width 0.0) (abs$ 0.0f0))
    (declare (type single-float abs$)
     (type (simple-array double-float (52)) rlist2)
     (type (simple-array double-float (3)) res3la)
     (type double-float width uflow small reseps resabs oflow ertest errsum
      error2 erro12 error1 errmax errbnd erlast erlarg epmach dres domega
      defabs defab2 defab1 correc b2 b1 a2 a1 area2 area12 area1 area abseps)
     (type f2cl-lib:integer4 numrl2 nrmom nrmax nres nev maxerr ktmin ksgn k
      jupbnd iroff3 iroff2 iroff1 ierro id)
     (type f2cl-lib:logical extall noext extrap))
    (setf epmach (f2cl-lib:d1mach 4))
    (setf ier 0)
    (setf neval 0)
    (setf last$ 0)
    (setf result 0.0)
    (setf abserr 0.0)
    (f2cl-lib:fset (f2cl-lib:fref alist (1) ((1 *))) a)
    (f2cl-lib:fset (f2cl-lib:fref blist (1) ((1 *))) b)
    (f2cl-lib:fset (f2cl-lib:fref rlist (1) ((1 *))) 0.0)
    (f2cl-lib:fset (f2cl-lib:fref elist (1) ((1 *))) 0.0)
    (f2cl-lib:fset (f2cl-lib:fref iord (1) ((1 *))) 0)
    (f2cl-lib:fset (f2cl-lib:fref nnlog (1) ((1 *))) 0)
    (if
     (or (and (/= integr 1) (/= integr 2))
         (and (<= epsabs 0.0) (< epsrel (max (* 50.0 epmach) 5.0e-29)))
         (< icall 1)
         (< maxp1 1))
     (setf ier 6))
    (if (= ier 6) (go label999))
    (setf domega (coerce (abs omega) 'double-float))
    (setf nrmom 0)
    (if (> icall 1) (go label5))
    (setf momcom 0)
   label5
    (multiple-value-bind
        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
         var-11 var-12 var-13 var-14)
        (dqc25f f a b domega integr nrmom maxp1 0 result abserr neval defabs
         resabs momcom chebmo)
      (declare (ignore var-0 var-1 var-2 var-5 var-6 var-7 var-14))
      (setf domega var-3)
      (setf integr var-4)
      (setf result var-8)
      (setf abserr var-9)
      (setf neval var-10)
      (setf defabs var-11)
      (setf resabs var-12)
      (setf momcom var-13))
    (setf dres (coerce (abs result) 'double-float))
    (setf errbnd (max epsabs (* epsrel dres)))
    (f2cl-lib:fset (f2cl-lib:fref rlist (1) ((1 *))) result)
    (f2cl-lib:fset (f2cl-lib:fref elist (1) ((1 *))) abserr)
    (f2cl-lib:fset (f2cl-lib:fref iord (1) ((1 *))) 1)
    (if (and (<= abserr (* 100.0 epmach defabs)) (> abserr errbnd))
        (setf ier 2))
    (if (= limit 1) (setf ier 1))
    (if (or (/= ier 0) (<= abserr errbnd)) (go label200))
    (setf uflow (f2cl-lib:d1mach 1))
    (setf oflow (f2cl-lib:d1mach 2))
    (setf errmax abserr)
    (setf maxerr 1)
    (setf area result)
    (setf errsum abserr)
    (setf abserr oflow)
    (setf nrmax 1)
    (setf extrap f2cl-lib:%false%)
    (setf noext f2cl-lib:%false%)
    (setf ierro 0)
    (setf iroff1 0)
    (setf iroff2 0)
    (setf iroff3 0)
    (setf ktmin 0)
    (setf small (* (abs (- b a)) 0.75))
    (setf nres 0)
    (setf numrl2 0)
    (setf extall f2cl-lib:%false%)
    (if (> (* 0.5 (abs (- b a)) domega) 2.0) (go label10))
    (setf numrl2 1)
    (setf extall f2cl-lib:%true%)
    (f2cl-lib:fset (f2cl-lib:fref rlist2 (1) ((1 52))) result)
   label10
    (if (<= (* 0.25 (abs (- b a)) domega) 2.0) (setf extall f2cl-lib:%true%))
    (setf ksgn -1)
    (if (>= dres (* (- 1.0 (* 50.0 epmach)) defabs)) (setf ksgn 1))
    (f2cl-lib:fdo (last$ 2 (f2cl-lib:int-add last$ 1))
                  ((> last$ limit) nil)
      (tagbody
        (setf nrmom
                (f2cl-lib:int-add (f2cl-lib:fref nnlog (maxerr) ((1 *))) 1))
        (setf a1 (f2cl-lib:fref alist (maxerr) ((1 *))))
        (setf b1
                (* 0.5
                   (+ (f2cl-lib:fref alist (maxerr) ((1 *)))
                      (f2cl-lib:fref blist (maxerr) ((1 *))))))
        (setf a2 b1)
        (setf b2 (f2cl-lib:fref blist (maxerr) ((1 *))))
        (setf erlast errmax)
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12 var-13 var-14)
            (dqc25f f a1 b1 domega integr nrmom maxp1 0 area1 error1 nev resabs
             defab1 momcom chebmo)
          (declare (ignore var-0 var-1 var-2 var-5 var-6 var-7 var-14))
          (setf domega var-3)
          (setf integr var-4)
          (setf area1 var-8)
          (setf error1 var-9)
          (setf nev var-10)
          (setf resabs var-11)
          (setf defab1 var-12)
          (setf momcom var-13))
        (setf neval (f2cl-lib:int-add neval nev))
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12 var-13 var-14)
            (dqc25f f a2 b2 domega integr nrmom maxp1 1 area2 error2 nev resabs
             defab2 momcom chebmo)
          (declare (ignore var-0 var-1 var-2 var-5 var-6 var-7 var-14))
          (setf domega var-3)
          (setf integr var-4)
          (setf area2 var-8)
          (setf error2 var-9)
          (setf nev var-10)
          (setf resabs var-11)
          (setf defab2 var-12)
          (setf momcom var-13))
        (setf neval (f2cl-lib:int-add neval nev))
        (setf area12 (+ area1 area2))
        (setf erro12 (+ error1 error2))
        (setf errsum (- (+ errsum erro12) errmax))
        (setf area (- (+ area area12) (f2cl-lib:fref rlist (maxerr) ((1 *)))))
        (if (or (= defab1 error1) (= defab2 error2)) (go label25))
        (if
         (or
          (> (abs (- (f2cl-lib:fref rlist (maxerr) ((1 *))) area12))
             (* 1.0e-5 (abs area12)))
          (< erro12 (* 0.99 errmax)))
         (go label20))
        (if extrap (setf iroff2 (f2cl-lib:int-add iroff2 1)))
        (if (not extrap) (setf iroff1 (f2cl-lib:int-add iroff1 1)))
       label20
        (if (and (> last$ 10) (> erro12 errmax))
            (setf iroff3 (f2cl-lib:int-add iroff3 1)))
       label25
        (f2cl-lib:fset (f2cl-lib:fref rlist (maxerr) ((1 *))) area1)
        (f2cl-lib:fset (f2cl-lib:fref rlist (last$) ((1 *))) area2)
        (f2cl-lib:fset (f2cl-lib:fref nnlog (maxerr) ((1 *))) nrmom)
        (f2cl-lib:fset (f2cl-lib:fref nnlog (last$) ((1 *))) nrmom)
        (setf errbnd (max epsabs (* epsrel (abs area))))
        (if (or (>= (f2cl-lib:int-add iroff1 iroff2) 10) (>= iroff3 20))
            (setf ier 2))
        (if (>= iroff2 5) (setf ierro 3))
        (if (= last$ limit) (setf ier 1))
        (if
         (<= (max (abs a1) (abs b2))
             (* (+ 1.0 (* 100.0 epmach)) (+ (abs a2) (* 1000.0 uflow))))
         (setf ier 4))
        (if (> error2 error1) (go label30))
        (f2cl-lib:fset (f2cl-lib:fref alist (last$) ((1 *))) a2)
        (f2cl-lib:fset (f2cl-lib:fref blist (maxerr) ((1 *))) b1)
        (f2cl-lib:fset (f2cl-lib:fref blist (last$) ((1 *))) b2)
        (f2cl-lib:fset (f2cl-lib:fref elist (maxerr) ((1 *))) error1)
        (f2cl-lib:fset (f2cl-lib:fref elist (last$) ((1 *))) error2)
        (go label40)
       label30
        (f2cl-lib:fset (f2cl-lib:fref alist (maxerr) ((1 *))) a2)
        (f2cl-lib:fset (f2cl-lib:fref alist (last$) ((1 *))) a1)
        (f2cl-lib:fset (f2cl-lib:fref blist (last$) ((1 *))) b1)
        (f2cl-lib:fset (f2cl-lib:fref rlist (maxerr) ((1 *))) area2)
        (f2cl-lib:fset (f2cl-lib:fref rlist (last$) ((1 *))) area1)
        (f2cl-lib:fset (f2cl-lib:fref elist (maxerr) ((1 *))) error2)
        (f2cl-lib:fset (f2cl-lib:fref elist (last$) ((1 *))) error1)
       label40
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
            (dqpsrt limit last$ maxerr errmax elist iord nrmax)
          (declare (ignore var-0 var-1 var-4 var-5))
          (setf maxerr var-2)
          (setf errmax var-3)
          (setf nrmax var-6))
        (if (<= errsum errbnd) (go label170))
        (if (/= ier 0) (go label150))
        (if (and (= last$ 2) extall) (go label120))
        (if noext (go label140))
        (if (not extall) (go label50))
        (setf erlarg (- erlarg erlast))
        (if (> (abs (- b1 a1)) small) (setf erlarg (+ erlarg erro12)))
        (if extrap (go label70))
       label50
        (setf width
                (coerce
                 (abs
                  (- (f2cl-lib:fref blist (maxerr) ((1 *)))
                     (f2cl-lib:fref alist (maxerr) ((1 *)))))
                 'double-float))
        (if (> width small) (go label140))
        (if extall (go label60))
        (setf small (* small 0.5))
        (if (> (* 0.25 width domega) 2.0) (go label140))
        (setf extall f2cl-lib:%true%)
        (go label130)
       label60
        (setf extrap f2cl-lib:%true%)
        (setf nrmax 2)
       label70
        (if (or (= ierro 3) (<= erlarg ertest)) (go label90))
        (setf jupbnd last$)
        (if (> last$ (+ (the f2cl-lib:integer4 (truncate limit 2)) 2))
            (setf jupbnd (f2cl-lib:int-sub (f2cl-lib:int-add limit 3) last$)))
        (setf id nrmax)
        (f2cl-lib:fdo (k id (f2cl-lib:int-add k 1))
                      ((> k jupbnd) nil)
          (tagbody
            (setf maxerr (f2cl-lib:fref iord (nrmax) ((1 *))))
            (setf errmax (f2cl-lib:fref elist (maxerr) ((1 *))))
            (if
             (>
              (abs
               (- (f2cl-lib:fref blist (maxerr) ((1 *)))
                  (f2cl-lib:fref alist (maxerr) ((1 *)))))
              small)
             (go label140))
            (setf nrmax (f2cl-lib:int-add nrmax 1))
           label80))
       label90
        (setf numrl2 (f2cl-lib:int-add numrl2 1))
        (f2cl-lib:fset (f2cl-lib:fref rlist2 (numrl2) ((1 52))) area)
        (if (< numrl2 3) (go label110))
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5)
            (dqelg numrl2 rlist2 reseps abseps res3la nres)
          (declare (ignore var-1 var-4))
          (setf numrl2 var-0)
          (setf reseps var-2)
          (setf abseps var-3)
          (setf nres var-5))
        (setf ktmin (f2cl-lib:int-add ktmin 1))
        (if (and (> ktmin 5) (< abserr (* 0.001 errsum))) (setf ier 5))
        (if (>= abseps abserr) (go label100))
        (setf ktmin 0)
        (setf abserr abseps)
        (setf result reseps)
        (setf correc erlarg)
        (setf ertest (max epsabs (* epsrel (abs reseps))))
        (if (<= abserr ertest) (go label150))
       label100
        (if (= numrl2 1) (setf noext f2cl-lib:%true%))
        (if (= ier 5) (go label150))
       label110
        (setf maxerr (f2cl-lib:fref iord (1) ((1 *))))
        (setf errmax (f2cl-lib:fref elist (maxerr) ((1 *))))
        (setf nrmax 1)
        (setf extrap f2cl-lib:%false%)
        (setf small (* small 0.5))
        (setf erlarg errsum)
        (go label140)
       label120
        (setf small (* small 0.5))
        (setf numrl2 (f2cl-lib:int-add numrl2 1))
        (f2cl-lib:fset (f2cl-lib:fref rlist2 (numrl2) ((1 52))) area)
       label130
        (setf ertest errbnd)
        (setf erlarg errsum)
       label140))
   label150
    (if (or (= abserr oflow) (= nres 0)) (go label170))
    (if (= (f2cl-lib:int-add ier ierro) 0) (go label165))
    (if (= ierro 3) (setf abserr (+ abserr correc)))
    (if (= ier 0) (setf ier 3))
    (if (and (/= result 0.0) (/= area 0.0)) (go label160))
    (if (> abserr errsum) (go label170))
    (if (= area 0.0) (go label190))
    (go label165)
   label160
    (if (> (/ abserr (abs result)) (/ errsum (abs area))) (go label170))
   label165
    (if
     (and (= ksgn -1)
          (<= (max (abs result) (abs area)) (* defabs 0.010000000000000002)))
     (go label190))
    (if
     (or (> 0.010000000000000002 (/ result area))
         (> (/ result area) 100.0)
         (>= errsum (abs area)))
     (setf ier 6))
    (go label190)
   label170
    (setf result 0.0)
    (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                  ((> k last$) nil)
      (tagbody
        (setf result (+ result (f2cl-lib:fref rlist (k) ((1 *)))))
       label180))
    (setf abserr errsum)
   label190
    (if (> ier 2) (setf ier (f2cl-lib:int-sub ier 1)))
   label200
    (if (and (= integr 2) (< omega 0.0)) (setf result (- result)))
   label999
    (go end_label)
   end_label
    (return
     (values nil
             nil
             nil
             nil
             integr
             nil
             nil
             nil
             nil
             nil
             result
             abserr
             neval
             ier
             last$
             nil
             nil
             nil
             nil
             nil
             nil
             momcom
             nil))))

