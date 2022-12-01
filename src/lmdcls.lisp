;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 8 -*- ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defvar *macro-file* nil)

#+gcl
(progn 
  (system::clines "object MAKE_UNSPECIAL(object x) {if (type_of(x)==t_symbol) x->s.s_stype=0;return Cnil;}")
  (system::defentry make-unspecial (system::object) (system::object "MAKE_UNSPECIAL")))

#+(or scl cmu)
(defun make-unspecial (symbol)
  (ext:clear-info variable c::kind symbol)
  symbol)


(defmacro declare-top (&rest decl-specs)
  `(eval-when
    ,(cond (*macro-file*  #+gcl '(compile eval load)
			  #-gcl '(:compile-toplevel :load-toplevel :execute) )
	   (t #+gcl '(eval compile) #-gcl '(:compile-toplevel :execute)))
    ,@(loop for v in decl-specs
	     unless (member (car v) '(special unspecial)) nconc nil
	     else
	     when (eql (car v) 'unspecial)
	     collect `(progn
		       ,@(loop for w in (cdr v)
				collect #-(or gcl scl cmu ecl)
                                       `(remprop ',w
						 #-excl 'special
						 #+excl 'excl::.globally-special.)
				#+(or gcl scl cmu ecl)
			        `(make-unspecial ',w)))
	     else collect `(proclaim ',v))))

;;; This list should contain all specials required by runtime or more than one maxima file,
;;; except for some specials declared in the macro files, eg displm

(declaim (special
	  $%% 
	  $arrays $berlefact
	  $beta_args_sum_to_integer $boxchar
	  $compgrind
	  $current_let_rule_package
	  $default_let_rule_package
	  $dispflag
	  $display_format_internal $domain $domxexpt
	  $domxplus $domxtimes $dontfactor
	  $doscmxplus
	  $erfflag $errexp $error $error_size $error_syms $expon
	  $expop $exptisolate
	  $facexpand $factorflag $features $file_search
	  $float $floatformat $floatfrac $floatint
	  $floatoptions $floatprec $floatwidth $fortfloat $fortindent
	  $fortspaces $functions $gammalim $gcd
	  $homog_hack
	  $inchar $infeval $intfaclim
	  $isolate_wrt_times $keepfloat $labels $letrat $letvarsimp
	  $let_rule_packages $liflag $linechar $linenum $linel
	  $linsolve_params $listarith $listconstvars
	  $lmxchar $logconcoeffp $logexpand $lognegint
	  $logsimp $m1pbranch $macroexpansion $macros $maperror $mapprint
	  $maxapplydepth $maxapplyheight $maxnegex $maxposex
	  $maxtayorder $mode_checkp $mode_check_errorp $mode_check_warnp
	  $mx0simp $myoptions $nalgfac $negdistrib
	  $negsumdispflag $norepeat $noundisp $numer
	  $numer_pbranch $optimprefix $optionset $outchar
	  $parsewindow $pfeformat $piece $pointbound
	  $powerdisp $props
	  $radexpand $ratalgdenom $ratdenomdivide $ratepsilon $ratexpand
          $ratfac $ratprint $ratsimpexpons $ratvars $ratvarswitch
          $ratweights
	  $ratwtlvl $realonly $refcheck $resultant $rmxchar $rootsconmode
	  $rules $savedef $savefactors $setcheck
	  $setcheckbreak $setval $showtime $simp
	  $sqrtdispflag $sublis_apply_lambda
	  $subnumsimp $sumexpand $sumsplitfact
	  $taylor_logexpand
	  $taylor_truncate_polynomials $timer $timer_devalue
	  $trace $trace_break_arg $trace_max_indent
	  $trace_safety $translate $transrun
	  $tr_array_as_ref $tr_bound_function_applyp
	  $tr_file_tty_messagesp $tr_float_can_branch_complex
	  $tr_function_call_default $tr_numer
	  $tr_optimize_max_loop
	  $tr_state_vars
	  $tr_true_name_of_file_being_translated $tr_warn_bad_function_calls
	  $tr_warn_fexpr $tr_warn_meval $tr_warn_mode $tr_warn_undeclared
	  $tr_warn_undefined_variable
	  $use_fast_arrays $values $vect_cross
	  %e-val %p%i %pi-val %pi//2 %pi//4 %pi2 *$any-modes*
	  *alpha *const* *fnewvarsw *gcdl* *in *in-compile*
	  *in-translate-file* *inv* *irreds *min* *mx*
	  *n *opers-list *out *ratweights *tr-warn-break* *transl-backtrace*
	  *transl-debug* *warned-fexprs*
	  *warned-mode-vars* *warned-un-declared-vars* *zexptsimp? |-1//2|
	  -sqrt3//2 |1//2| adn* aexprp algfac* algnotexact
	  alpha *alphabet* aryp assigns *atp* atvars
	  bfhalf bfmhalf bigfloat%e bigfloat%pi bigfloatone bigfloatzero
	  bindlist *mdebug*
	  defined_variables defintdebug derivflag derivlist
	  derivsimp displayp dn* dosimp dsksetp dummy-variable-operators
	  errorsw evarrp evp expandflag expandp
	  exptrlsw factlist featurel fmaplvl
	  fourth%pi fr-factor gauss
	  generate-atan2 genpairs genvar half%pi half%pi3
	  implicit-real in-p inratsimp inside-mprog
	  integerl *islinp* limit-answers limitp linel
	  *linelabel* local loclist low* maplp mdop
	  meta-prop-l meta-prop-p mfexprp minpoly* mlocp mm* modulus *mopl*
	  mplc* mprogp mproplist mspeclist msump munbindp
	  need-prog? negprods negsums nn* noevalargs nonintegerl
	  *nounl* *nounsflag* opers opers-list outargs1 outargs2
	  preserve-direction prods radcanp
	  realonlyratnum *refchkl* return-mode returns rulefcnl
	  rulesw scanmapp sign-imag-errp simplimplus-problems
	  *small-primes* sqrt3//2 substp
	  sums tellratlist timesinp tr-abort tr-progret tr-unique
	  transl-file translate-time-evalables transp
	  tstack varlist wflag
	  $cflength *trunclist $taylordepth
	  $maxtaydiff $verbose $psexpand ps-bmt-disrep
	  silent-taylor-flag $define_variable $infolists
	  $factor_max_degree $factor_max_degree_print_warning))

(declaim (declaration unspecial))
