;; lexical_symbols.lisp -- lexical symbols for Maxima
;; copyright 2012, 2019 by Robert Dodier
;; I release this work under terms of the GNU GPL
;;
;; Constructs which have local variables, namely
;; block, function definitions (named and unnamed), and for loops,
;; are redefined so that the local variables are lexical symbols.
;; That is, two local variables which have the same name in
;; different constructs are distinct.
;;
;; Local variables are made lexical, via gensym substitution,
;; when the construct (block, function, or for loop) is parsed.
;;
;; This file, by itself, only implements lexical local variables.
;;
;; This file defines a new feature, global,
;; which makes symbols dynamic instead of lexical.
;; One can say declare(foo, global) and then featurep(foo, global) => true,
;; or, equivalently, (KINDP '$FOO '$GLOBAL) => T.
;; Any symbols which have been declared global by the time
;; the construct is parsed are excluded from gensym substitution.

(in-package :maxima)

;; VERIFY THAT TRIGRAT, TRIGSIMP, FACSUM, FACTORFACSUM, AND COLLECTTERMS WORK AS EXPECTED WITH LEXICAL SYMBOLS
;; THEN REMOVE THESE LINES
;; ($auto_mexpr '$trigrat "trigrat.lisp")
;; ($auto_mexpr '$trigsimp "trgsmp.mac")
;; ($auto_mexpr '$facsum "facexp.mac")
;; ($auto_mexpr '$factorfacsum "facexp.mac")
;; ($auto_mexpr '$collectterms "facexp.mac")

;; At present, this is just a convenience for testing.
;; Not sure whether this should be made permanent.

(defun $unlexicalize (e)
  (if (atom e)
    (if (symbolp e)
      (or (get e 'reversealias) e)
      e)
    (cons (cons ($unlexicalize (caar e)) (cdar e)) (mapcar '$unlexicalize (cdr e)))))

;; Lexicalize MPROG (i.e., block([a, b, c, ...], ...))

(defun maybe-subst-lexical-symbols-into-mprog (e)
  (if (or (null (cdr e)) (not ($listp (cadr e))))
    e
    (let*
      ((mprog-op (car e))
       (mprog-args (cdr e))
       (vars+var-inits (cdr (car mprog-args)))
       (exprs (cdr mprog-args)))
      (subst-lexical-symbols-into-mprog mprog-op vars+var-inits exprs))))

(defun make-lexical-gensym (s)
  (let*
    ((s1 (gensym (symbol-name s)))
     (%s ($nounify s))
     (%s1 ($nounify s1)))
    (setf (get s1 'reversealias) (or (get s 'reversealias) s))
    (setf (get %s1 'reversealias) (or (get %s 'reversealias) %s))
    s1))

(defun subst-lexical-symbols-into-mprog (mprog-op vars+var-inits exprs)
  (let*
   ((vars+var-inits-lexical (remove-if #'(lambda (x) (kindp (if (symbolp x) x (second x)) '$global)) vars+var-inits))
    (vars+var-inits-global (remove-if-not #'(lambda (x) (kindp (if (symbolp x) x (second x)) '$global)) vars+var-inits))
    (vars-only-lexical (remove-if-not #'symbolp vars+var-inits-lexical))
    (var-inits-lexical (remove-if #'symbolp vars+var-inits-lexical))
    (var-inits-vars-lexical (mapcar #'second var-inits-lexical))
    (vars-all-lexical (append vars-only-lexical var-inits-vars-lexical))
    (vars-only-gensyms (mapcar #'make-lexical-gensym vars-only-lexical))
    (var-inits-gensyms (mapcar #'make-lexical-gensym var-inits-vars-lexical))
    (gensyms-all (append vars-only-gensyms var-inits-gensyms))
    (subst-eqns (apply #'append (mapcar #'(lambda (x y)
                                            (list `((mequal) ,x ,y)
                                                  `((mequal) ,($nounify x) ,($nounify y))))
                                        vars-all-lexical gensyms-all)))
    (gensym-mprogn (let (($simp nil)) (declare (special $simp)) ($substitute `((mlist) ,@ subst-eqns) `((mprogn) ,@ exprs))))
    (gensym-inits (mapcar #'(lambda (e y) (list (first e) y (third e))) var-inits-lexical var-inits-gensyms))
    (gensym-mprog `(,mprog-op ((mlist) ,@ (append vars-only-gensyms gensym-inits vars+var-inits-global)) ,@ (cdr gensym-mprogn))))
   gensym-mprog))

(defvar left-paren-symbol '|$(|)
(defvar right-paren-symbol '|$)|)

(def-nud (mprog) (op)
  (if (eq (first-c) left-paren-symbol)
    (progn
      (pop-c) ;; eat the opening parenthesis
      (let
        ((right (prsmatch right-paren-symbol '$any))
         (header (mheader 'mprog)))
        (cons '$any (maybe-subst-lexical-symbols-into-mprog (cons header right)))))
    `($any . ,op)))

;; Lexicalize MDEFINE (i.e. f(a, b, c, ...) := ..., also f[a, b, c, ...] := ..., and f[a, b, c, ...](x, y, z, ...) := ...)

(defun parse-infix-with-lexicalization (op left)
  (let ((e (parse-infix op left)))
    (cons (first e) (subst-lexical-symbols-into-mdefine-or-lambda (rest e)))))

(defun subst-lexical-symbols-into-mdefine-or-lambda (e)
  (let*
    ((args (remove-if #'(lambda (x) (kindp x '$global)) (extract-arguments-symbols e)))
     (args-gensyms (mapcar
                     #'(lambda (s)
                         (let ((s1 (gensym (symbol-name s))))
                           (setf (get s1 'reversealias) (or (get s 'reversealias) s)) s1)) args))
     (subst-eqns (mapcar #'(lambda (x y) `((mequal) ,x ,y)) args args-gensyms))
     (substituted-definition (let (($simp nil)) (declare (special $simp)) ($substitute `((mlist) ,@ subst-eqns) e)))
     (function-header (first (second e)))
     (function-header-new (cons function-header (rest (second substituted-definition)))))
    (append (list (first e) function-header-new) (cddr substituted-definition))))

(defun extract-arguments-symbols (e)
  (let*
    ((e-lhs (second e))
     (is-mqapply (eq (caar e-lhs) 'mqapply))
     (e-lhs-op (when (consp e-lhs) ($op e-lhs)))
     (e-lhs-args (when (consp e-lhs) ($args e-lhs))))
    (if is-mqapply
      (rest ($listofvars ($append ($args e-lhs-op) e-lhs-args)))
      (rest ($listofvars e-lhs-args)))))

(def-led-equiv |$:=| parse-infix-with-lexicalization)
(def-lbp       |$:=| 180.)
(def-rbp       |$:=|  20.)
(def-pos       |$:=| $any)
(def-rpos      |$:=| $any)
(def-lpos      |$:=| $any)
(def-mheader   |$:=| (mdefine))

(def-led-equiv |$::=| parse-infix-with-lexicalization)
(def-lbp       |$::=| 180.)
(def-rbp       |$::=|  20.)
(def-pos       |$::=| $any)
(def-rpos      |$::=| $any)
(def-lpos      |$::=| $any)
(def-mheader   |$::=| (mdefmacro))

;; Lexicalize LAMBDA (i.e., lambda([a, b, c, ...], ...))

(def-nud (lambda) (op)
  (if (eq (first-c) '|$(|)
    (progn
      (pop-c) ;; eat the opening parenthesis
      (let
        ((right (prsmatch right-paren-symbol '$any))
         (header (mheader 'lambda)))
        (cons '$any (subst-lexical-symbols-into-mdefine-or-lambda (cons header right)))))
    `($any . ,op)))

;; Lexicalize MDO and MDOIN (i.e., for x: ... do ..., and for x in ... do ...)

(defun parse-$do-simple (lex &aux (left (make-mdo)))
  (setf (car left) (mheader 'mdo))
  (do ((op lex (pop-c))  (active-bitmask 0))
      (nil)
    (if (eq op '|$:|) (setq op '$from))
    (setq active-bitmask (collision-check '$do active-bitmask op))
    (let ((data (parse (rpos op) (rbp op))))
      (case op
	($do		(setf (mdo-body left) data) (return (cons '$any left)))
	($for		(setf (mdo-for  left) data))
	($from		(setf (mdo-from left) data))
	($in		(setf (mdo-op   left) 'mdoin)
			(setf (mdo-from left) data))
	($step		(setf (mdo-step left) data))
	($next		(setf (mdo-next left) data))
	($thru		(setf (mdo-thru left) data))
	(($unless $while)
			(if (eq op '$while)
			    (setq data (list (mheader '$not) data)))
			(setf (mdo-unless left)
			   (if (null (mdo-unless left))
			       data
			       (list (mheader '$or) data (mdo-unless left)))))
	(t (parse-bug-err '$do))))))

(defun parse-$do-with-lexicalization (&rest a)
  (let*
    ((do-expr (apply 'parse-$do-simple a))
     (var (third do-expr))
     (var-subst (gensym (symbol-name var)))
     (next (sixth do-expr))
     (unless (eighth do-expr))
     (body (ninth do-expr)))
    (setf (get var-subst 'reversealias) (or (get var 'reversealias) var))
    (setf (third do-expr) var-subst)
    (let (($simp nil))
      (declare (special $simp))
      (setf (sixth do-expr) (maxima-substitute var-subst var next))
      (setf (eighth do-expr) (maxima-substitute var-subst var unless))
      (setf (ninth do-expr) (maxima-substitute var-subst var body)))
    do-expr))

(def-mheader $do (mdo))

(def-lbp $for    25.)
(def-lbp $from   25.)
(def-lbp $step   25.)
(def-lbp $next   25.)
(def-lbp $thru   25.)
(def-lbp $unless 25.)
(def-lbp $while  25.)
(def-lbp $do	 25.)

(def-nud-equiv $for    parse-$do-with-lexicalization)
(def-nud-equiv $from   parse-$do-simple)
(def-nud-equiv $step   parse-$do-simple)
(def-nud-equiv $next   parse-$do-simple)
(def-nud-equiv $thru   parse-$do-simple)
(def-nud-equiv $unless parse-$do-simple)
(def-nud-equiv $while  parse-$do-simple)
(def-nud-equiv $do     parse-$do-simple)

(def-rbp $do      25.)
(def-rbp $for    200.)
(def-rbp $from    95.)
(def-rbp $in      95.)
(def-rbp $step    95.)
(def-rbp $next    45.)
(def-rbp $thru    95.)
(def-rbp $unless  45.)
(def-rbp $while	  45.)

(def-rpos $do     $any)
(def-rpos $for    $any)
(def-rpos $from   $any)
(def-rpos $step   $expr)
(def-rpos $next   $any)
(def-rpos $thru   $expr)
(def-rpos $unless $clause)
(def-rpos $while  $clause)

(def-collisions $do
  ($do	   . ())
  ($for    . ($for))
  ($from   . ($in $from))
  ($in     . ($in $from $step $next))
  ($step   . ($in       $step $next))
  ($next   . ($in	$step $next))
  ($thru   . ($in $thru)) ;$IN didn't used to get checked for
  ($unless . ())
  ($while  . ()))

