@c -*- Mode: texinfo -*-
@c -----------------------------------------------------------------------------
@page
@node Constants, Lists, Strings, Data Types and Structures
@section Constants
@c -----------------------------------------------------------------------------

@menu
* Functions and Variables for Constants::
@end menu

@c -----------------------------------------------------------------------------
@node Functions and Variables for Constants,  , Constants, Constants
@subsection Functions and Variables for Constants
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
@anchor{%e}
m4_setcat(Constants)
@c @defvr {Constant} %e
m4_defvr({Constant}, %e)
@ifinfo
@vrindex e
@vrindex Euler's number
@vrindex Base of natural logarithm
@end ifinfo

@code{%e} represents the base of the natural logarithm, also known as Euler's
number.  The numeric value of @code{%e} is the double-precision floating-point
value 2.718281828459045d0.

@c @opencatbox
@c @category{Constants}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{%i}
@c @defvr {Constant} %i
m4_defvr({Constant}, %i)
@ifinfo
@vrindex i
@vrindex Imaginary unit
@end ifinfo

@code{%i} represents the imaginary unit, @math{sqrt(- 1)}.

@opencatbox
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{false}
@c @defvr {Constant} false
m4_defvr({Constant}, false)

@code{false} represents the Boolean constant of the same name.
Maxima implements @code{false} by the value @code{NIL} in Lisp.

@c @opencatbox
@c @category{Constants}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{%gamma}
@c @defvr {Constant} %gamma
m4_defvr({Constant}, %gamma)
@ifinfo
@vrindex Euler-Mascheroni constant
@end ifinfo

The Euler-Mascheroni constant, 0.5772156649015329 ....
@c DOUBTLESS THERE IS MORE TO SAY HERE.

@c @opencatbox
@c @category{Constants}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{ind}
@c @defvr {Constant} ind
m4_defvr({Constant}, ind)
@ifinfo
@vrindex Indeterminate
@end ifinfo

@code{ind} represents a bounded, indefinite result.

See also @mrefdot{limit}

Example:

@c ===beg===
@c limit (sin(1/x), x, 0);
@c ===end===
@example
(%i1) limit (sin(1/x), x, 0);
(%o1)                          ind
@end example

@c @opencatbox
@c @category{Constants}
@c @closecatbox
@c @end defvr
m4_end_defvr()
@c -----------------------------------------------------------------------------
@anchor{inf}
@c @defvr {Constant} inf
m4_defvr({Constant}, inf)
@ifinfo
@vrindex Real infinity
@end ifinfo

@code{inf} represents real positive infinity.

@c @opencatbox
@c @category{Constants}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{infinity}
@c @defvr {Constant}  infinity
m4_defvr({Constant},  infinity)
@ifinfo
@vrindex Complex infinity
@end ifinfo

@code{infinity} represents complex infinity.

@c @opencatbox
@c @category{Constants}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{minf}
@c @defvr {Constant} minf
m4_defvr({Constant}, minf)
@ifinfo
@vrindex Minus infinity
@vrindex Negative infinity
@end ifinfo

@code{minf} represents real minus (i.e., negative) infinity.

@c @opencatbox
@c @category{Constants}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{%phi}
@c @defvr {Constant} %phi
m4_defvr({Constant}, %phi)
@ifinfo
@vrindex phi
@vrindex Golden mean
@end ifinfo

@code{%phi} represents the so-called @i{golden mean}, @math{(1 + sqrt(5))/2}.
The numeric value of @code{%phi} is the double-precision floating-point value
1.618033988749895d0.

@mref{fibtophi} expresses Fibonacci numbers @code{fib(n)} in terms of
@code{%phi}.

By default, Maxima does not know the algebraic properties of @code{%phi}.
After evaluating @code{tellrat(%phi^2 - %phi - 1)} and @code{algebraic: true},
@mref{ratsimp} can simplify some expressions containing @code{%phi}.

Examples:

@code{fibtophi} expresses Fibonacci numbers @code{fib(n)} in terms of @code{%phi}.

@c ===beg===
@c fibtophi (fib (n));
@c fib (n-1) + fib (n) - fib (n+1);
@c fibtophi (%);
@c ratsimp (%);
@c ===end===
@example
(%i1) fibtophi (fib (n));
                           n             n
                       %phi  - (1 - %phi)
(%o1)                  -------------------
                           2 %phi - 1
(%i2) fib (n-1) + fib (n) - fib (n+1);
(%o2)          - fib(n + 1) + fib(n) + fib(n - 1)
(%i3) fibtophi (%);
            n + 1             n + 1       n             n
        %phi      - (1 - %phi)        %phi  - (1 - %phi)
(%o3) - --------------------------- + -------------------
                2 %phi - 1                2 %phi - 1
                                          n - 1             n - 1
                                      %phi      - (1 - %phi)
                                    + ---------------------------
                                              2 %phi - 1
(%i4) ratsimp (%);
(%o4)                           0
@end example

By default, Maxima does not know the algebraic properties of @code{%phi}.
After evaluating @code{tellrat (%phi^2 - %phi - 1)} and @code{algebraic: true},
@code{ratsimp} can simplify some expressions containing @code{%phi}.

@c ===beg===
@c e : expand ((%phi^2 - %phi - 1) * (A + 1));
@c ratsimp (e);
@c tellrat (%phi^2 - %phi - 1);
@c algebraic : true;
@c ratsimp (e);
@c ===end===
@example
(%i1) e : expand ((%phi^2 - %phi - 1) * (A + 1));
                 2                      2
(%o1)        %phi  A - %phi A - A + %phi  - %phi - 1
(%i2) ratsimp (e);
                  2                     2
(%o2)        (%phi  - %phi - 1) A + %phi  - %phi - 1
(%i3) tellrat (%phi^2 - %phi - 1);
                            2
(%o3)                  [%phi  - %phi - 1]
(%i4) algebraic : true;
(%o4)                         true
(%i5) ratsimp (e);
(%o5)                           0
@end example

@c @opencatbox
@c @category{Constants}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{%pi}
@c @defvr {Constant} %pi
m4_defvr({Constant}, %pi)
@ifinfo
@vrindex pi
@end ifinfo

@code{%pi} represents the ratio of the perimeter of a circle to its diameter.
The numeric value of @code{%pi} is the double-precision floating-point value
3.141592653589793d0.

@c @opencatbox
@c @category{Constants}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{true}
@c @defvr {Constant} true
m4_defvr({Constant}, true)

@code{true} represents the Boolean constant of the same name.
Maxima implements @code{true} by the value @code{T} in Lisp.

@c @opencatbox
@c @category{Constants}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{und}
@c @defvr {Constant} und
m4_defvr({Constant}, und)
@ifinfo
@vrindex Undefined
@end ifinfo

@code{und} represents an undefined result.

See also @mrefdot{limit}

Example:

@c ===beg===
@c limit (x*sin(x), x, inf);
@c ===end===
@example
(%i1) limit (x*sin(x), x, inf);
(%o1)                          und
@end example

@c @opencatbox
@c @category{Constants}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{zeroa}
@c @defvr {Constant} zeroa
m4_defvr({Constant}, zeroa)

@code{zeroa} represents an infinitesimal above zero.  @code{zeroa} can be used
in expressions.  @code{limit} simplifies expressions which contain
infinitesimals.

See also @mref{zerob} and @mrefdot{limit}

Example:

@code{limit} simplifies expressions which contain infinitesimals:

@c ===beg===
@c limit(zeroa);
@c limit(zeroa+x);
@c ===end===
@example
(%i1) limit(zeroa);
(%o1)                           0
(%i2) limit(x+zeroa);
(%o2)                           x
@end example

@c @opencatbox
@c @category{Constants}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{zerob}
@c @defvr {Constant} zerob
m4_defvr({Constant}, zerob)

@code{zerob} represents an infinitesimal below zero.  @code{zerob} can be used
in expressions.  @code{limit} simplifies expressions which contain
infinitesimals.

See also @mref{zeroa} and @mrefdot{limit}

@c @opencatbox
@c @category{Constants}
@c @closecatbox
@c @end defvr
m4_end_defvr()

