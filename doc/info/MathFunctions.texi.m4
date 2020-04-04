@c -*- Mode: texinfo -*-
@menu
* Functions for Numbers::
* Functions for Complex Numbers::
* Combinatorial Functions::
* Root Exponential and Logarithmic Functions::
* Trigonometric Functions::
* Random Numbers::
@end menu

@c -----------------------------------------------------------------------------
@node Functions for Numbers, Functions for Complex Numbers, Mathematical Functions, Mathematical Functions
@section Functions for Numbers
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
m4_setcat(Mathematical functions)
@anchor{abs}
@c @deffn {Function} abs (@var{z})
m4_deffn({Function}, abs, <<<(@var{z})>>>)

The @code{abs} function represents the mathematical absolute value function and
works for both numerical and symbolic values. If the argument, @var{z}, is a
real or complex number, @code{abs} returns the absolute value of @var{z}. If
possible, symbolic expressions using the absolute value function are
also simplified.

Maxima can differentiate, integrate and calculate limits for expressions
containing @code{abs}. The @code{abs_integrate} package further extends
Maxima's ability to calculate integrals involving the abs function. See
(%i12) in the examples below.

When applied to a list or matrix, @code{abs} automatically distributes over
the terms. Similarly, it distributes over both sides of an
equation. To alter this behaviour, see the variable @mrefdot{distribute_over}

See also @mrefdot{cabs}

Examples:

Calculation of @code{abs} for real and complex numbers, including numerical
constants and various infinities. The first example shows how @code{abs}
distributes over the elements of a list.

@c ===beg===
@c abs([-4, 0, 1, 1+%i]);
@c abs((1+%i)*(1-%i));
@c abs(%e+%i);
@c abs([inf, infinity, minf]);
@c ===end===
@example
(%i1) abs([-4, 0, 1, 1+%i]);
(%o1)                  [4, 0, 1, sqrt(2)]

(%i2) abs((1+%i)*(1-%i));
(%o2)                           2
(%i3) abs(%e+%i);
                                2
(%o3)                    sqrt(%e  + 1)
(%i4) abs([inf, infinity, minf]);
(%o4)                   [inf, inf, inf]
@end example

Simplification of expressions containing @code{abs}:

@c ===beg===
@c abs(x^2);
@c abs(x^3);
@c abs(abs(x));
@c abs(conjugate(x));
@c ===end===
@example
(%i5) abs(x^2);
                                2
(%o5)                          x
(%i6) abs(x^3);
                             2
(%o6)                       x  abs(x)

(%i7) abs(abs(x));
(%o7)                       abs(x)
(%i8) abs(conjugate(x));
(%o8)                       abs(x)
@end example

Integrating and differentiating with the @code{abs} function. Note that more
integrals involving the @code{abs} function can be performed, if the
@code{abs_integrate} package is loaded. The last example shows the Laplace
transform of @code{abs}: see @mrefdot{laplace}

@c ===beg===
@c diff(x*abs(x),x),expand;
@c integrate(abs(x),x);
@c integrate(x*abs(x),x);
@c load("abs_integrate")$
@c integrate(x*abs(x),x);
@c integrate(abs(x),x,-2,%pi);
@c laplace(abs(x),x,s);
@c ===end===
@example
(%i9) diff(x*abs(x),x),expand;
(%o9)                       2 abs(x)

(%i10) integrate(abs(x),x);
                             x abs(x)
(%o10)                       --------
                                2

(%i11) integrate(x*abs(x),x);
                           /
                           [
(%o11)                     I x abs(x) dx
                           ]
                           /

(%i12) load("abs_integrate")$
(%i13) integrate(x*abs(x),x);
                      2           3
                     x  abs(x)   x  signum(x)
(%o13)               --------- - ------------
                         2            6

(%i14) integrate(abs(x),x,-2,%pi);
                               2
                            %pi
(%o14)                      ---- + 2
                             2

(%i15) laplace(abs(x),x,s);
                               1
(%o15)                         --
                                2
                               s
@end example

@c @opencatbox
@c @category{Mathematical functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{ceiling}
@c @deffn {Function} ceiling (@var{x})
m4_deffn({Function}, ceiling, <<<(@var{x})>>>)

When @var{x} is a real number, return the least integer that 
is greater than or equal to @var{x}.

If @var{x} is a constant expression (@code{10 * %pi}, for example), 
@code{ceiling} evaluates @var{x} using big floating point numbers, and 
applies @code{ceiling} to the resulting big float.  Because @code{ceiling} uses
floating point evaluation, it's possible, although unlikely, that @code{ceiling}
could return an erroneous value for constant inputs.  To guard against errors,
the floating point evaluation is done using three values for @mrefdot{fpprec}

For non-constant inputs, @code{ceiling} tries to return a simplified value.
Here are examples of the simplifications that @code{ceiling} knows about:

@c ===beg===
@c ceiling (ceiling (x));
@c ceiling (floor (x));
@c declare (n, integer)$
@c [ceiling (n), ceiling (abs (n)), ceiling (max (n, 6))];
@c assume (x > 0, x < 1)$
@c ceiling (x);
@c tex (ceiling (a));
@c ===end===
@example
@group
(%i1) ceiling (ceiling (x));
(%o1)                      ceiling(x)
@end group
@group
(%i2) ceiling (floor (x));
(%o2)                       floor(x)
@end group
(%i3) declare (n, integer)$
@group
(%i4) [ceiling (n), ceiling (abs (n)), ceiling (max (n, 6))];
(%o4)                [n, abs(n), max(6, n)]
@end group
(%i5) assume (x > 0, x < 1)$
@group
(%i6) ceiling (x);
(%o6)                           1
@end group
@group
(%i7) tex (ceiling (a));
$$\left \lceil a \right \rceil$$
(%o7)                         false
@end group
@end example

The @code{ceiling} function distributes over lists, matrices and equations.
See @mrefdot{distribute_over}

Finally, for all inputs that are manifestly complex, @code{ceiling} returns 
a noun form.

If the range of a function is a subset of the integers, it can be declared to
be @code{integervalued}.  Both the @code{ceiling} and @mref{floor} functions
can use this information; for example:

@c ===beg===
@c declare (f, integervalued)$
@c floor (f(x));
@c ceiling (f(x) - 1);
@c ===end===
@example
(%i1) declare (f, integervalued)$
@group
(%i2) floor (f(x));
(%o2)                         f(x)
@end group
@group
(%i3) ceiling (f(x) - 1);
(%o3)                       f(x) - 1
@end group
@end example

Example use:

@c ===beg===
@c unitfrac(r) := block([uf : [], q],
@c     if not(ratnump(r)) then 
@c        error("unitfrac: argument must be a rational number"),
@c     while r # 0 do (
@c         uf : cons(q : 1/ceiling(1/r), uf),
@c         r : r - q),
@c     reverse(uf));
@c unitfrac (9/10);
@c apply ("+", %);
@c unitfrac (-9/10);
@c apply ("+", %);
@c unitfrac (36/37);
@c apply ("+", %);
@c ===end===
@example
(%i1) unitfrac(r) := block([uf : [], q],
    if not(ratnump(r)) then
       error("unitfrac: argument must be a rational number"),
    while r # 0 do (
        uf : cons(q : 1/ceiling(1/r), uf),
        r : r - q),
    reverse(uf));
(%o1) unitfrac(r) := block([uf : [], q], 
if not ratnump(r) then error("unitfrac: argument must be a rational number"
                                     1
), while r # 0 do (uf : cons(q : ----------, uf), r : r - q), 
                                         1
                                 ceiling(-)
                                         r
reverse(uf))
@group
(%i2) unitfrac (9/10);
                            1  1  1
(%o2)                      [-, -, --]
                            2  3  15
@end group
@group
(%i3) apply ("+", %);
                               9
(%o3)                          --
                               10
@end group
@group
(%i4) unitfrac (-9/10);
                                  1
(%o4)                       [- 1, --]
                                  10
@end group
@group
(%i5) apply ("+", %);
                                9
(%o5)                         - --
                                10
@end group
@group
(%i6) unitfrac (36/37);
                        1  1  1  1    1
(%o6)                  [-, -, -, --, ----]
                        2  3  8  69  6808
@end group
@group
(%i7) apply ("+", %);
                               36
(%o7)                          --
                               37
@end group
@end example

@c @opencatbox
@c @category{Mathematical functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{entier}
@c @deffn {Function} entier (@var{x})
m4_deffn({Function}, entier, <<<(@var{x})>>>)

Returns the largest integer less than or equal to @var{x} where @var{x} is
numeric.  @mref{fix} (as in @code{fixnum}) is a synonym for this, so
@code{fix(@var{x})} is precisely the same.

@c @opencatbox
@c @category{Mathematical functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{floor}
@c @deffn {Function} floor (@var{x})
m4_deffn({Function}, floor, <<<(@var{x})>>>)

When @var{x} is a real number, return the largest integer that is less than or
equal to @var{x}.

If @var{x} is a constant expression (@code{10 * %pi}, for example), @code{floor}
evaluates @var{x} using big floating point numbers, and applies @code{floor} to
the resulting big float. Because @code{floor} uses floating point evaluation,
it's possible, although unlikely, that @code{floor} could return an erroneous
value for constant inputs.  To guard against errors, the floating point
evaluation is done using three values for @mrefdot{fpprec}

For non-constant inputs, @code{floor} tries to return a simplified value.  Here
are examples of the simplifications that @code{floor} knows about:

@c ===beg===
@c floor (ceiling (x));
@c floor (floor (x));
@c declare (n, integer)$
@c [floor (n), floor (abs (n)), floor (min (n, 6))];
@c assume (x > 0, x < 1)$
@c floor (x);
@c tex (floor (a));
@c ===end===
@example
@group
(%i1) floor (ceiling (x));
(%o1)                      ceiling(x)
@end group
@group
(%i2) floor (floor (x));
(%o2)                       floor(x)
@end group
(%i3) declare (n, integer)$
@group
(%i4) [floor (n), floor (abs (n)), floor (min (n, 6))];
(%o4)                [n, abs(n), min(6, n)]
@end group
(%i5) assume (x > 0, x < 1)$
@group
(%i6) floor (x);
(%o6)                           0
@end group
@group
(%i7) tex (floor (a));
$$\left \lfloor a \right \rfloor$$
(%o7)                         false
@end group
@end example

The @code{floor} function distributes over lists, matrices and equations.
See @mrefdot{distribute_over}

Finally, for all inputs that are manifestly complex, @code{floor} returns 
a noun form.

If the range of a function is a subset of the integers, it can be declared to
be @code{integervalued}.  Both the @mref{ceiling} and @code{floor} functions
can use this information; for example:

@c ===beg===
@c declare (f, integervalued)$
@c floor (f(x));
@c ceiling (f(x) - 1);
@c ===end===
@example
(%i1) declare (f, integervalued)$
@group
(%i2) floor (f(x));
(%o2)                         f(x)
@end group
@group
(%i3) ceiling (f(x) - 1);
(%o3)                       f(x) - 1
@end group
@end example

@c @opencatbox
@c @category{Mathematical functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{fix}
@c @deffn {Function} fix (@var{x})
m4_deffn({Function}, fix, <<<(@var{x})>>>)

A synonym for @code{entier (@var{x})}.

@c @opencatbox
@c @category{Mathematical functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Mathematical functions, Lists, Sets)
@anchor{lmax}
@c @deffn {Function} lmax (@var{L})
m4_deffn({Function}, lmax, <<<(@var{L})>>>)

When @var{L} is a list or a set, return @code{apply ('max, args (@var{L}))}.
When @var{L} is not a list or a set, signal an error.
See also @mref{lmin} and @mrefdot{max}

@c @opencatbox
@c @category{Mathematical functions}
@c @category{Lists}
@c @category{Sets}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{lmin}
@c @deffn {Function} lmin (@var{L})
m4_deffn({Function}, lmin, <<<(@var{L})>>>)

When @var{L} is a list or a set, return @code{apply ('min, args (@var{L}))}.
When @var{L} is not a list or a set, signal an error.
See also @mref{lmax} and @mrefdot{min}

@c @opencatbox
@c @category{Mathematical functions}
@c @category{Lists}
@c @category{Sets}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Mathematical functions)
@anchor{max}
@c @deffn {Function} max (@var{x_1}, @dots{}, @var{x_n})
m4_deffn({Function}, max, <<<(@var{x_1}, @dots{}, @var{x_n})>>>)

Return a simplified value for the maximum of the expressions @var{x_1} through
@var{x_n}.  When @code{get (trylevel, maxmin)}, is 2 or greater, @code{max}
uses the simplification @code{max (e, -e) --> |e|}.  When
@code{get (trylevel, maxmin)} is 3 or greater, @var{max} tries to eliminate
expressions that are between two other arguments; for example,
@code{max (x, 2*x, 3*x) --> max (x, 3*x)}.  To set the value of @code{trylevel}
to 2, use @code{put (trylevel, 2, maxmin)}.

See also @mref{min} and @mrefdot{lmax}

@c @opencatbox
@c @category{Mathematical functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{min}
@c @deffn {Function} min (@var{x_1}, @dots{}, @var{x_n})
m4_deffn({Function}, min, <<<(@var{x_1}, @dots{}, @var{x_n})>>>)

Return a simplified value for the minimum of the expressions @code{x_1} through
@code{x_n}.  When @code{get (trylevel, maxmin)}, is 2 or greater, @code{min}
uses the simplification @code{min (e, -e) --> -|e|}.  When
@code{get (trylevel, maxmin)} is 3 or greater, @code{min} tries to eliminate
expressions that are between two other arguments; for example,
@code{min (x, 2*x, 3*x) --> min (x, 3*x)}.  To set the value of @code{trylevel}
to 2, use @code{put (trylevel, 2, maxmin)}.

See also @mref{max} and @mrefdot{lmin}

@c @opencatbox
@c @category{Mathematical functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{round}
@c @deffn {Function} round (@var{x})
m4_deffn({Function}, round, <<<(@var{x})>>>)

When @var{x} is a real number, returns the closest integer to @var{x}.
Multiples of 1/2 are rounded to the nearest even integer.  Evaluation of
@var{x} is similar to @mref{floor} and @mrefdot{ceiling}

The @code{round} function distributes over lists, matrices and equations.
See @mrefdot{distribute_over}

@c @opencatbox
@c @category{Mathematical functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{signum}
@c @deffn {Function} signum (@var{x})
m4_deffn({Function}, signum, <<<(@var{x})>>>)

For either real or complex numbers @var{x}, the signum function returns
0 if @var{x} is zero; for a nonzero numeric input @var{x}, the signum function
returns @code{x/abs(x)}.

For non-numeric inputs, Maxima attempts to determine the sign of the input.
When the sign is negative, zero, or positive, @code{signum} returns -1,0, 1,
respectively.  For all other values for the sign, @code{signum} a simplified but
equivalent form.  The simplifications include reflection (@code{signum(-x)}
gives @code{-signum(x)}) and multiplicative identity (@code{signum(x*y)} gives
@code{signum(x) * signum(y)}).

The @code{signum} function distributes over a list, a matrix, or an
equation.  See @mref{sign} and @mrefdot{distribute_over}

@c @opencatbox
@c @category{Mathematical functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{truncate}
@c @deffn {Function} truncate (@var{x})
m4_deffn({Function}, truncate, <<<(@var{x})>>>)

When @var{x} is a real number, return the closest integer to @var{x} not
greater in absolute value than @var{x}.  Evaluation of @var{x} is similar
to @mref{floor} and @mrefdot{ceiling}

The @code{truncate} function distributes over lists, matrices and equations.
See @mrefdot{distribute_over}

@c @opencatbox
@c @category{Mathematical functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@node Functions for Complex Numbers, Combinatorial Functions, Functions for Numbers, Mathematical Functions
@section Functions for Complex Numbers
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
m4_setcat(Complex variables)
@anchor{cabs}
@c @deffn {Function} cabs (@var{expr})
m4_deffn({Function}, cabs, <<<(@var{expr})>>>)

Calculates the absolute value of an expression representing a complex
number.  Unlike the function @mrefcomma{abs} the @code{cabs} function always
decomposes its argument into a real and an imaginary part.  If @code{x} and
@code{y} represent real variables or expressions, the @code{cabs} function
calculates the absolute value of @code{x + %i*y} as

@c ===beg===
@c cabs (1);
@c cabs (1 + %i);
@c cabs (exp (%i));
@c cabs (exp (%pi * %i));
@c cabs (exp (3/2 * %pi * %i));
@c cabs (17 * exp (2 * %i));
@c ===end===
@example
@group
(%i1) cabs (1);
(%o1)                           1
@end group
@group
(%i2) cabs (1 + %i);
(%o2)                        sqrt(2)
@end group
@group
(%i3) cabs (exp (%i));
(%o3)                           1
@end group
@group
(%i4) cabs (exp (%pi * %i));
(%o4)                           1
@end group
@group
(%i5) cabs (exp (3/2 * %pi * %i));
(%o5)                           1
@end group
@group
(%i6) cabs (17 * exp (2 * %i));
(%o6)                          17
@end group
@end example

If @code{cabs} returns a noun form this most commonly is caused by
some properties of the variables involved not being known:

@c ===beg===
@c cabs (a+%i*b);
@c declare(a,real,b,real);
@c cabs (a+%i*b);
@c assume(a>0,b>0);
@c cabs (a+%i*b);
@c ===end===
@example
@group
(%i1) cabs (a+%i*b);
                                2    2
(%o1)                     sqrt(b  + a )
@end group
@group
(%i2) declare(a,real,b,real);
(%o2)                         done
@end group
@group
(%i3) cabs (a+%i*b);
                                2    2
(%o3)                     sqrt(b  + a )
@end group
@group
(%i4) assume(a>0,b>0);
(%o4)                    [a > 0, b > 0]
@end group
@group
(%i5) cabs (a+%i*b);
                                2    2
(%o5)                     sqrt(b  + a )
@end group
@end example

The @code{cabs} function can use known properties like symmetry properties of
complex functions to help it calculate the absolute value of an expression.  If
such identities exist, they can be advertised to @code{cabs} using function
properties.  The symmetries that @code{cabs} understands are: mirror symmetry,
conjugate function and complex characteristic.

@code{cabs} is a verb function and is not suitable for symbolic
calculations.  For such calculations (including integration,
differentiation and taking limits of expressions containing absolute
values), use @mrefdot{abs}

The result of @code{cabs} can include the absolute value function,
@mrefcomma{abs} and the arc tangent, @mrefdot{atan2}

When applied to a list or matrix, @code{cabs} automatically distributes over
the terms.  Similarly, it distributes over both sides of an equation.

For further ways to compute with complex numbers, see the functions
@mrefcomma{rectform} @mrefcomma{realpart} @mrefcomma{imagpart}@w{}
@mrefcomma{carg} @mref{conjugate} and @mrefdot{polarform}

Examples:

Examples with @mref{sqrt} and @mrefdot{sin}

@c ===beg===
@c cabs(sqrt(1+%i*x));
@c cabs(sin(x+%i*y));
@c ===end===
@example
(%i1) cabs(sqrt(1+%i*x));
                             2     1/4
(%o1)                      (x  + 1)
(%i2) cabs(sin(x+%i*y));
                    2        2         2        2
(%o2)       sqrt(cos (x) sinh (y) + sin (x) cosh (y))
@end example

The error function, @mrefcomma{erf} has mirror symmetry, which is used here in
the calculation of the absolute value with a complex argument:

@c ===beg===
@c cabs(erf(x+%i*y));
@c ===end===
@example
(%i3) cabs(erf(x+%i*y));
                                          2
           (erf(%i y + x) - erf(%i y - x))
(%o3) sqrt(--------------------------------
                          4
                                                               2
                                (erf(%i y + x) + erf(%i y - x))
                              - --------------------------------)
                                               4
@end example

Maxima knows complex identities for the Bessel functions, which allow
it to compute the absolute value for complex arguments.  Here is an
example for @mrefdot{bessel_j}

@c ===beg===
@c cabs(bessel_j(1,%i));
@c ===end===
@example
(%i4) cabs(bessel_j(1,%i));
(%o4)                 abs(bessel_j(1, %i))
@end example

@c @opencatbox
@c @category{Complex variables}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{carg}
@c @deffn {Function} carg (@var{z})
m4_deffn({Function}, carg, <<<(@var{z})>>>)

Returns the complex argument of @var{z}.  The complex argument is an angle
@code{theta} in @code{(-%pi, %pi]} such that @code{r exp (theta %i) = @var{z}}
where @code{r} is the magnitude of @var{z}.

@code{carg} is a computational function, not a simplifying function.
@c PROBABLY NEED TO EXPLAIN IMPLICATIONS OF ABOVE STATEMENT

See also @mref{abs} (complex magnitude), @mrefcomma{polarform}@w{}
@mrefcomma{rectform} @mrefcomma{realpart} and @mrefdot{imagpart}

Examples:

@c ===beg===
@c carg (1);
@c carg (1 + %i);
@c carg (exp (%i));
@c carg (exp (%pi * %i));
@c carg (exp (3/2 * %pi * %i));
@c carg (17 * exp (2 * %i));
@c ===end===
@example
@group
(%i1) carg (1);
(%o1)                           0
@end group
@group
(%i2) carg (1 + %i);
                               %pi
(%o2)                          ---
                                4
@end group
@group
(%i3) carg (exp (%i));
                               sin(1)
(%o3)                     atan(------)
                               cos(1)
@end group
@group
(%i4) carg (exp (%pi * %i));
(%o4)                          %pi
@end group
@group
(%i5) carg (exp (3/2 * %pi * %i));
                                %pi
(%o5)                         - ---
                                 2
@end group
@group
(%i6) carg (17 * exp (2 * %i));
                            sin(2)
(%o6)                  atan(------) + %pi
                            cos(2)
@end group
@end example

If @code{carg} returns a noun form this most communly is caused by
some properties of the variables involved not being known:

@c ===beg===
@c carg (a+%i*b);
@c declare(a,real,b,real);
@c carg (a+%i*b);
@c assume(a>0,b>0);
@c carg (a+%i*b);
@c ===end===
@example
@group
(%i1) carg (a+%i*b);
(%o1)                      atan2(b, a)
@end group
@group
(%i2) declare(a,real,b,real);
(%o2)                         done
@end group
@group
(%i3) carg (a+%i*b);
(%o3)                      atan2(b, a)
@end group
@group
(%i4) assume(a>0,b>0);
(%o4)                    [a > 0, b > 0]
@end group
@group
(%i5) carg (a+%i*b);
                                  b
(%o5)                        atan(-)
                                  a
@end group
@end example

@c @opencatbox
@c @category{Complex variables}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{conjugate}
@c @deffn {Function} conjugate (@var{x})
m4_deffn({Function}, conjugate, <<<(@var{x})>>>)

Returns the complex conjugate of @var{x}.

@c ===beg===
@c declare ([aa, bb], real, cc, complex, ii, imaginary);
@c conjugate (aa + bb*%i);
@c conjugate (cc);
@c conjugate (ii);
@c conjugate (xx + yy);
@c ===end===
@example
@group
(%i1) declare ([aa, bb], real, cc, complex, ii, imaginary);
(%o1)                         done
@end group
@group
(%i2) conjugate (aa + bb*%i);
(%o2)                      aa - %i bb
@end group
@group
(%i3) conjugate (cc);
(%o3)                     conjugate(cc)
@end group
@group
(%i4) conjugate (ii);
(%o4)                         - ii
@end group
@group
(%i5) conjugate (xx + yy);
(%o5)                        yy + xx
@end group
@end example

@c @opencatbox
@c @category{Complex variables}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{imagpart}
@c @deffn {Function} imagpart (@var{expr})
m4_deffn({Function}, imagpart, <<<(@var{expr})>>>)

Returns the imaginary part of the expression @var{expr}.

@code{imagpart} is a computational function, not a simplifying function.
@c PROBABLY NEED TO EXPLAIN IMPLICATIONS OF ABOVE STATEMENT
@c SEE ALSO SF BUG REPORT # 902290

See also @mrefcomma{abs} @mrefcomma{carg} @mrefcomma{polarform}@w{}
@mrefcomma{rectform} and @mrefdot{realpart}

Example:

@c ===beg===
@c imagpart (a+b*%i);
@c imagpart (1+sqrt(2)*%i);
@c imagpart (1);
@c imagpart (sqrt(2)*%i);
@c ===end===
@example
@group
(%i1) imagpart (a+b*%i);
(%o1)                           b
@end group
@group
(%i2) imagpart (1+sqrt(2)*%i);
(%o2)                        sqrt(2)
@end group
@group
(%i3) imagpart (1);
(%o3)                           0
@end group
@group
(%i4) imagpart (sqrt(2)*%i);
(%o4)                        sqrt(2)
@end group
@end example

@c @opencatbox
@c @category{Complex variables}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c NEEDS EXAMPLES

@c -----------------------------------------------------------------------------
m4_setcat(Complex variables, Exponential and logarithm functions)
@anchor{polarform}
@c @deffn {Function} polarform (@var{expr})
m4_deffn({Function}, polarform, <<<(@var{expr})>>>)

Returns an expression @code{r %e^(%i theta)} equivalent to @var{expr},
such that @code{r} and @code{theta} are purely real.

Example:

@c ===beg===
@c polarform(a+b*%i);
@c polarform(1+%i);
@c polarform(1+2*%i);
@c ===end===
@example
@group
(%i1) polarform(a+b*%i);
                       2    2    %i atan2(b, a)
(%o1)            sqrt(b  + a ) %e
@end group
@group
(%i2) polarform(1+%i);
                                  %i %pi
                                  ------
                                    4
(%o2)                   sqrt(2) %e
@end group
@group
(%i3) polarform(1+2*%i);
                                %i atan(2)
(%o3)                 sqrt(5) %e
@end group
@end example

@c @opencatbox
@c @category{Complex variables}
@c @category{Exponential and logarithm functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Complex variables)
@anchor{realpart}
@c @deffn {Function} realpart (@var{expr})
m4_deffn({Function}, realpart, <<<(@var{expr})>>>)

Returns the real part of @var{expr}.  @code{realpart} and @mref{imagpart} will
work on expressions involving trigonometric and hyperbolic functions,
as well as square root, logarithm, and exponentiation.

Example:

@c ===beg===
@c realpart (a+b*%i);
@c realpart (1+sqrt(2)*%i);
@c realpart (sqrt(2)*%i);
@c realpart (1);
@c ===end===
@example
@group
(%i1) realpart (a+b*%i);
(%o1)                           a
@end group
@group
(%i2) realpart (1+sqrt(2)*%i);
(%o2)                           1
@end group
@group
(%i3) realpart (sqrt(2)*%i);
(%o3)                           0
@end group
@group
(%i4) realpart (1);
(%o4)                           1
@end group
@end example

@c @opencatbox
@c @category{Complex variables}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{rectform}
@c @deffn {Function} rectform (@var{expr})
m4_deffn({Function}, rectform, <<<(@var{expr})>>>)

Returns an expression @code{a + b %i} equivalent to @var{expr},
such that @var{a} and @var{b} are purely real.

Example:

@c ===beg===
@c rectform(sqrt(2)*%e^(%i*%pi/4));
@c rectform(sqrt(b^2+a^2)*%e^(%i*atan2(b, a)));
@c rectform(sqrt(5)*%e^(%i*atan(2)));
@c ===end===
@example
@group
(%i1) rectform(sqrt(2)*%e^(%i*%pi/4));
(%o1)                        %i + 1
@end group
@group
(%i2) rectform(sqrt(b^2+a^2)*%e^(%i*atan2(b, a)));
(%o2)                       %i b + a
@end group
@group
(%i3) rectform(sqrt(5)*%e^(%i*atan(2)));
(%o3)                       2 %i + 1
@end group
@end example

@c @opencatbox
@c @category{Complex variables}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@node Combinatorial Functions, Root Exponential and Logarithmic Functions, Functions for Complex Numbers, Mathematical Functions
@section Combinatorial Functions
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
m4_setcat(Gamma and factorial functions, Operators)
@anchor{!!}
@c @deffn {Operator} !!
m4_deffn({Operator}, !!, <<<>>>)
@ifinfo
@fnindex Double factorial
@end ifinfo

The double factorial operator.

For an integer, float, or rational number @code{n}, @code{n!!} evaluates to the
product @code{n (n-2) (n-4) (n-6) ... (n - 2 (k-1))} where @code{k} is equal to
@code{entier (n/2)}, that is, the largest integer less than or equal to
@code{n/2}.  Note that this definition does not coincide with other published
definitions for arguments which are not integers.
@c REPORTED TO BUG TRACKER AS BUG # 1093138 !!!

For an even (or odd) integer @code{n}, @code{n!!} evaluates to the product of
all the consecutive even (or odd) integers from 2 (or 1) through @code{n}
inclusive.

For an argument @code{n} which is not an integer, float, or rational, @code{n!!}
yields a noun form @code{genfact (n, n/2, 2)}.
@c n!! IS NEITHER SIMPLIFIED NOR EVALUATED IN THIS CASE 
@c -- MENTION THAT? OR TOO MUCH DETAIL ???

@c @opencatbox
@c @category{Gamma and factorial functions}
@c @category{Operators}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{binomial}
@c @deffn {Function} binomial (@var{x}, @var{y})
m4_deffn({Function}, binomial, <<<(@var{x}, @var{y})>>>)

The binomial coefficient @code{@var{x}!/(@var{y}! (@var{x} - @var{y})!)}.
If @var{x} and @var{y} are integers, then the numerical value of the binomial
coefficient is computed.  If @var{y}, or @var{x - y}, is an integer, the
binomial coefficient is expressed as a polynomial.

Examples:

@c ===beg===
@c binomial (11, 7);
@c 11! / 7! / (11 - 7)!;
@c binomial (x, 7);
@c binomial (x + 7, x);
@c binomial (11, y);
@c ===end===
@example
@group
(%i1) binomial (11, 7);
(%o1)                          330
@end group
@group
(%i2) 11! / 7! / (11 - 7)!;
(%o2)                          330
@end group
@group
(%i3) binomial (x, 7);
        (x - 6) (x - 5) (x - 4) (x - 3) (x - 2) (x - 1) x
(%o3)   -------------------------------------------------
                              5040
@end group
@group
(%i4) binomial (x + 7, x);
      (x + 1) (x + 2) (x + 3) (x + 4) (x + 5) (x + 6) (x + 7)
(%o4) -------------------------------------------------------
                               5040
@end group
@group
(%i5) binomial (11, y);
(%o5)                    binomial(11, y)
@end group
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Gamma and factorial functions)
@anchor{factcomb}
@deffn {Function} factcomb (@var{expr})

Tries to combine the coefficients of factorials in @var{expr}
with the factorials themselves by converting, for example, @code{(n + 1)*n!}
into @code{(n + 1)!}.

@mref{sumsplitfact} if set to @code{false} will cause @mref{minfactorial} to be
applied after a @code{factcomb}.

Example:

@c ===beg===
@c sumsplitfact;
@c (n + 1)*(n + 1)*n!;
@c factcomb (%);
@c sumsplitfact: not sumsplitfact;
@c (n + 1)*(n + 1)*n!;
@c factcomb (%);
@c ===end===
@example
@group
(%i1) sumsplitfact;
(%o1)                         true
@end group
@group
(%i2) (n + 1)*(n + 1)*n!;
                                  2
(%o2)                      (n + 1)  n!
@end group
@group
(%i3) factcomb (%);
(%o3)                  (n + 2)! - (n + 1)!
@end group
@group
(%i4) sumsplitfact: not sumsplitfact;
(%o4)                         false
@end group
@group
(%i5) (n + 1)*(n + 1)*n!;
                                  2
(%o5)                      (n + 1)  n!
@end group
@group
(%i6) factcomb (%);
(%o6)                 n (n + 1)! + (n + 1)!
@end group
@end example

@c @opencatbox
@c @category{Gamma and factorial functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Gamma and factorial functions, Operators)
@anchor{!}
@anchor{factorial}
@c @deffn  {Function} factorial
m4_deffn({Function}, factorial, <<<(x)>>>)
@c @deffnx {Operator} !
m4_deffnx({Operator}, !, <<<>>>)

Represents the factorial function.  Maxima treats @code{factorial (@var{x})}
the same as @code{@var{x}!}.

For any complex number @code{x}, except for negative integers, @code{x!} is 
defined as @code{gamma(x+1)}.

For an integer @code{x}, @code{x!} simplifies to the product of the integers 
from 1 to @code{x} inclusive.  @code{0!} simplifies to 1.  For a real or complex 
number in float or bigfloat precision @code{x}, @code{x!} simplifies to the 
value of @code{gamma (x+1)}.  For @code{x} equal to @code{n/2} where @code{n} is 
an odd integer, @code{x!} simplifies to a rational factor times 
@code{sqrt (%pi)} (since @code{gamma (1/2)} is equal to @code{sqrt (%pi)}).

The option variables @mref{factlim} and @mref{gammalim} control the numerical
evaluation of factorials for integer and rational arguments.  The functions 
@mref{minfactorial} and @mref{factcomb} simplifies expressions containing
factorials.

The functions @mrefcomma{gamma} @mrefcomma{bffac} and @mref{cbffac} are
varieties of the gamma function.  @code{bffac} and @code{cbffac} are called
internally by @code{gamma} to evaluate the gamma function for real and complex
numbers in bigfloat precision.

@mref{makegamma} substitutes @code{gamma} for factorials and related functions.

Maxima knows the derivative of the factorial function and the limits for 
specific values like negative integers.

The option variable @mref{factorial_expand} controls the simplification of
expressions like @code{(n+x)!}, where @code{n} is an integer.

See also @mrefdot{binomial}

The factorial of an integer is simplified to an exact number unless the operand 
is greater than @code{factlim}.  The factorial for real and complex numbers is 
evaluated in float or bigfloat precision.

@c ===beg===
@c factlim : 10;
@c [0!, (7/2)!, 8!, 20!];
@c [4,77!, (1.0+%i)!];
@c [2.86b0!, (1.0b0+%i)!];
@c ===end===
@example
@group
(%i1) factlim : 10;
(%o1)                          10
@end group
@group
(%i2) [0!, (7/2)!, 8!, 20!];
                     105 sqrt(%pi)
(%o2)            [1, -------------, 40320, 20!]
                          16
@end group
@group
(%i3) [4,77!, (1.0+%i)!];
(%o3) [4, 77!, 0.3430658398165453 %i + 0.6529654964201667]
@end group
@group
(%i4) [2.86b0!, (1.0b0+%i)!];
(%o4)  [5.046635586910012b0, 3.430658398165454b-1 %i + 6.529654964201667b-1]
@end group
@end example

The factorial of a known constant, or general expression is not simplified.
Even so it may be possible to simplify the factorial after evaluating the
operand.

@c ===beg===
@c [(%i + 1)!, %pi!, %e!, (cos(1) + sin(1))!];
@c ev (%, numer, %enumer);
@c ===end===
@example
@group
(%i1) [(%i + 1)!, %pi!, %e!, (cos(1) + sin(1))!];
(%o1)      [(%i + 1)!, %pi!, %e!, (sin(1) + cos(1))!]
@end group
@group
(%i2) ev (%, numer, %enumer);
(%o2) [0.3430658398165453 %i + 0.6529654964201667, 
         7.188082728976031, 4.260820476357003, 1.227580202486819]
@end group
@end example

@c REMOVING THIS EXAMPLE. IT IS NOT SPECIFIC FOR THE FACTORIAL FUNCTION:
@c The factorial of an unbound symbol is not simplified.

@c @c ===beg===
@c @c kill (foo);
@c @c foo!;
@c @c ===end===
@c @example
@c (%i1) kill (foo);
@c (%o1)                         done
@c (%i2) foo!;
@c (%o2)                         foo!
@c @end example

Factorials are simplified, not evaluated.
Thus @code{x!} may be replaced even in a quoted expression.

@c ===beg===
@c '([0!, (7/2)!, 4.77!, 8!, 20!]);
@c ===end===
@example
@group
(%i1) '([0!, (7/2)!, 4.77!, 8!, 20!]);
          105 sqrt(%pi)
(%o1) [1, -------------, 81.44668037931197, 40320, 
               16
                                             2432902008176640000]
@end group
@end example

Maxima knows the derivative of the factorial function.

@c ===beg===
@c diff(x!,x);
@c ===end===
@example
@group
(%i1) diff(x!,x);
(%o1)                    x! psi (x + 1)
                               0
@end group
@end example

The option variable @code{factorial_expand} controls expansion and 
simplification of expressions with the factorial function.

@c ===beg===
@c (n+1)!/n!,factorial_expand:true;
@c ===end===
@example
@group
(%i1) (n+1)!/n!,factorial_expand:true;
(%o1)                         n + 1
@end group
@end example

@c @opencatbox
@c @category{Gamma and factorial functions}
@c @category{Operators}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c NEEDS EXAMPLES

@c -----------------------------------------------------------------------------
m4_setcat(Gamma and factorial functions)
@anchor{factlim}
@c @defvr {Option variable} factlim
m4_defvr({Option variable}, factlim)
Default value: 100000

@code{factlim} specifies the highest factorial which is
automatically expanded.  If it is -1 then all integers are expanded.

@c @opencatbox
@c @category{Gamma and factorial functions}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{factorial_expand}
@c @defvr {Option variable} factorial_expand
m4_defvr({Option variable}, factorial_expand)
Default value: false

The option variable @code{factorial_expand} controls the simplification of 
expressions like @code{(n+1)!}, where @code{n} is an integer.
See @mref{factorial} for an example.

@c @opencatbox
@c @category{Gamma and factorial functions}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c IS THIS DEFINITION CORRECT ??

@c -----------------------------------------------------------------------------
m4_setcat(Gamma and factorial functions)
@anchor{genfact}
@c @deffn {Function} genfact (@var{x}, @var{y}, @var{z})
m4_deffn({Function}, genfact, <<<(@var{x}, @var{y}, @var{z})>>>)

Returns the generalized factorial, defined as
@code{x (x-z) (x - 2 z) ... (x - (y - 1) z)}.  Thus, when @var{x} is an integer,
@code{genfact (x, x, 1) = x!} and @code{genfact (x, x/2, 2) = x!!}.

@c @opencatbox
@c @category{Gamma and factorial functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{minfactorial}
@c @deffn {Function} minfactorial (@var{expr})
m4_deffn({Function}, minfactorial, <<<(@var{expr})>>>)

Examines @var{expr} for occurrences of two factorials
which differ by an integer.
@code{minfactorial} then turns one into a polynomial times the other.

@c I CAN'T TELL WHAT THIS IS SUPPOSED TO MEAN. !!!
@c minfactorial DOESN'T SEEM TO DO ANYTHING binomial DOESN'T DO BY ITSELF !!!
@c LOOKING AT THE minfactorial CODE DOESN'T HELP !!!
@c If exp involves binomial coefficients then they will be
@c converted into ratios of factorials.

@c ===beg===
@c n!/(n+2)!;
@c minfactorial (%);
@c ===end===
@example
(%i1) n!/(n+2)!;
                               n!
(%o1)                       --------
                            (n + 2)!
(%i2) minfactorial (%);
                                1
(%o2)                    ---------------
                         (n + 1) (n + 2)
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Gamma and factorial functions, Simplification flags and variables)
@anchor{sumsplitfact}
@c @defvr {Option variable} sumsplitfact
m4_defvr({Option variable}, sumsplitfact)
Default value: @code{true}

When @code{sumsplitfact} is @code{false},
@c "IS APPLIED" -- UNDER WHAT CIRCUMSTANCES EXACTLY ??
@mref{minfactorial} is applied after a @mrefdot{factcomb}

@c ===beg===
@c sumsplitfact;
@c n!/(n+2)!;   
@c factcomb(%); 
@c sumsplitfact: not sumsplitfact ;
@c n!/(n+2)!;
@c factcomb(%);
@c ===end=== 
@example
@group
(%i1) sumsplitfact;
(%o1)                         true
@end group
@group
(%i2) n!/(n+2)!;
                               n!
(%o2)                       --------
                            (n + 2)!
@end group
@group
(%i3) factcomb(%);
                               n!
(%o3)                       --------
                            (n + 2)!
@end group
@group
(%i4) sumsplitfact: not sumsplitfact ;
(%o4)                         false
@end group
@group
(%i5) n!/(n+2)!;
                               n!
(%o5)                       --------
                            (n + 2)!
@end group
@group
(%i6) factcomb(%);
                                1
(%o6)                    ---------------
                         (n + 1) (n + 2)
@end group
@end example

@c @opencatbox
@c @category{Gamma and factorial functions}
@c @category{Simplification flags and variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@node Root Exponential and Logarithmic Functions, Trigonometric Functions, Combinatorial Functions, Mathematical Functions
@section Root, Exponential and Logarithmic Functions
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
m4_setcat(Exponential and logarithm functions, Simplification flags and variables)
@anchor{%e_to_numlog}
@c @defvr {Option variable} %e_to_numlog
m4_defvr({Option variable}, %e_to_numlog)
Default value: @code{false}

When @code{true}, @code{r} some rational number, and @code{x} some expression,
@code{%e^(r*log(x))} will be simplified into @code{x^r} .  It should be noted
that the @code{radcan} command also does this transformation, and more
complicated transformations of this ilk as well.  The @code{logcontract}
command "contracts" expressions containing @code{log}.

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @category{Simplification flags and variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{%emode}
@c @defvr {Option variable} %emode
m4_defvr({Option variable}, %emode)
Default value: @code{true}

When @code{%emode} is @code{true}, @code{%e^(%pi %i x)} is simplified as
follows.

@code{%e^(%pi %i x)} simplifies to @code{cos (%pi x) + %i sin (%pi x)} if
@code{x} is a floating point number, an integer, or a multiple of 1/2, 1/3, 1/4,
or 1/6, and then further simplified.

For other numerical @code{x}, @code{%e^(%pi %i x)} simplifies to
@code{%e^(%pi %i y)} where @code{y} is @code{x - 2 k} for some integer @code{k}
such that @code{abs(y) < 1}.

When @code{%emode} is @code{false}, no special simplification of
@code{%e^(%pi %i x)} is carried out.

@c ===beg===
@c %emode;
@c %e^(%pi*%i*1);
@c %e^(%pi*%i*216/144);
@c %e^(%pi*%i*192/144);
@c %e^(%pi*%i*180/144);
@c %e^(%pi*%i*120/144);
@c %e^(%pi*%i*121/144);
@c ===end===
@example
@group
(%i1) %emode;
(%o1)                         true
@end group
@group
(%i2) %e^(%pi*%i*1);
(%o2)                          - 1
@end group
@group
(%i3) %e^(%pi*%i*216/144);
(%o3)                         - %i
@end group
@group
(%i4) %e^(%pi*%i*192/144);
                          sqrt(3) %i    1
(%o4)                  (- ----------) - -
                              2         2
@end group
@group
(%i5) %e^(%pi*%i*180/144);
                           %i          1
(%o5)                 (- -------) - -------
                         sqrt(2)    sqrt(2)
@end group
@group
(%i6) %e^(%pi*%i*120/144);
                          %i   sqrt(3)
(%o6)                     -- - -------
                          2       2
@end group
@group
(%i7) %e^(%pi*%i*121/144);
                            121 %i %pi
                            ----------
                               144
(%o7)                     %e
@end group
@end example

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @category{Simplification flags and variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
m4_setcat(Exponential and logarithm functions, Evaluation flags)
@anchor{%enumer}
@c @defvr {Option variable} %enumer
m4_defvr({Option variable}, %enumer)
Default value: @code{false}

When @code{%enumer} is @code{true}, @code{%e} is replaced by its numeric value
2.718@dots{}  whenever @code{numer} is @code{true}.

When @code{%enumer} is @code{false}, this substitution is carried out
only if the exponent in @code{%e^x} evaluates to a number.

See also @mref{ev} and @mrefdot{numer}

@c ===beg===
@c %enumer;
@c numer;
@c 2*%e;
@c %enumer: not %enumer;
@c 2*%e;
@c numer: not numer;
@c 2*%e;    
@c 2*%e^1;  
@c 2*%e^x;  
@c ===end===
@example
@group
(%i1) %enumer;
(%o1)                         false
@end group
@group
(%i2) numer;
(%o2)                         false
@end group
@group
(%i3) 2*%e;
(%o3)                         2 %e
@end group
@group
(%i4) %enumer: not %enumer;
(%o4)                         true
@end group
@group
(%i5) 2*%e;
(%o5)                         2 %e
@end group
@group
(%i6) numer: not numer;
(%o6)                         true
@end group
@group
(%i7) 2*%e;
(%o7)                   5.43656365691809
@end group
@group
(%i8) 2*%e^1;
(%o8)                   5.43656365691809
@end group
@group
(%i9) 2*%e^x;
                                         x
(%o9)                 2 2.718281828459045
@end group
@end example

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @category{Evaluation flags}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c PROBABLY MORE TO BE SAID HERE

@c -----------------------------------------------------------------------------
m4_setcat(Exponential and logarithm functions)
@anchor{exp}
@c @deffn {Function} exp (@var{x})
m4_deffn({Function}, exp, <<<(@var{x})>>>)

Represents the exponential function.  Instances of @code{exp (@var{x})} in input
are simplified to @code{%e^@var{x}}; @code{exp} does not appear in simplified
expressions.

@code{demoivre} if @code{true} causes @code{%e^(a + b %i)} to simplify to
@code{%e^(a (cos(b) + %i sin(b)))} if @code{b} is free of @code{%i}.
See @mrefdot{demoivre}

@code{%emode}, when @code{true}, causes @code{%e^(%pi %i x)} to be simplified.
See @mrefdot{%emode}

@code{%enumer}, when @code{true} causes @code{%e} to be replaced by
2.718@dots{} whenever @code{numer} is @code{true}.  See @mrefdot{%enumer}

@c ===beg===
@c demoivre;
@c %e^(a + b*%i);
@c demoivre: not demoivre;
@c %e^(a + b*%i);
@c ===end===
@example
@group
(%i1) demoivre;
(%o1)                         false
@end group
@group
(%i2) %e^(a + b*%i);
                             %i b + a
(%o2)                      %e
@end group
@group
(%i3) demoivre: not demoivre;
(%o3)                         true
@end group
@group
(%i4) %e^(a + b*%i);
                      a
(%o4)               %e  (%i sin(b) + cos(b))
@end group
@end example

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@c -----------------------------------------------------------------------------
m4_setcat(Exponential and logarithm functions)
@anchor{li}
@c @deffn {Function} li [@var{s}] (@var{z})
m4_deffn({Function}, li, <<<[@var{s}], (@var{z})>>>)
Represents the polylogarithm function of order @var{s} and argument @var{z},
defined by the infinite series

m4_mathjax(
<<<$${\rm Li}_s \left(z\right) = \sum_{k=1}^\infty {z^k \over k^s}$$>>>,
<<<@example
                                 inf
                                 ====   k
                                 \     z
                        Li (z) =  >    --
                          s      /      s
                                 ====  k
                                 k = 1
@end example
>>>)

@code{li [1]} is @code{- log (1 - z)}.  @code{li [2]} and @code{li [3]} are the
dilogarithm and trilogarithm functions, respectively.

When the order is 1, the polylogarithm simplifies to @code{- log (1 - z)}, which
in turn simplifies to a numerical value if @var{z} is a real or complex floating
point number or the @code{numer} evaluation flag is present.

When the order is 2 or 3,
the polylogarithm simplifies to a numerical value
if @var{z} is a real floating point number
or the @code{numer} evaluation flag is present.

Examples:

@c ===beg===
@c assume (x > 0);
@c integrate ((log (1 - t)) / t, t, 0, x);
@c li [2] (7);
@c li [2] (7), numer;
@c li [3] (7);
@c li [2] (7), numer;
@c L : makelist (i / 4.0, i, 0, 8);
@c map (lambda ([x], li [2] (x)), L);
@c map (lambda ([x], li [3] (x)), L);
@c ===end===
@example
@group
(%i1) assume (x > 0);
(%o1)                        [x > 0]
@end group
@group
(%i2) integrate ((log (1 - t)) / t, t, 0, x);
(%o2)                       - li (x)
                                2
@end group
@group
(%i3) li [2] (7);
(%o3)                        li (7)
                               2
@end group
@group
(%i4) li [2] (7), numer;
(%o4)       1.248273182099423 - 6.113257028817991 %i
@end group
@group
(%i5) li [3] (7);
(%o5)                        li (7)
                               3
@end group
@group
(%i6) li [3] (7), numer;
(%o6)       5.319257992145674 - 5.94792444808033 %i
@end group
@group
(%i7) L : makelist (i / 4.0, i, 0, 8);
(%o7)   [0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0]
@end group
@group
(%i8) map (lambda ([x], li [2] (x)), L);
(%o8) [0.0, 0.2676526390827326, 0.5822405264650125, 
0.978469392930306, 1.644934066848226, 
2.190177011441645 - 0.7010261415046585 %i, 
2.37439527027248 - 1.2738062049196 %i, 
2.448686765338205 - 1.758084848210787 %i, 
2.467401100272339 - 2.177586090303601 %i]
@end group
@group
(%i9) map (lambda ([x], li [3] (x)), L);
(%o9) [0.0, 0.2584613953442624, 0.537213192678042, 
0.8444258046482203, 1.2020569, 1.642866878950322
 - 0.07821473130035025 %i, 2.060877505514697
 - 0.2582419849982037 %i, 2.433418896388322
 - 0.4919260182322965 %i, 2.762071904015935
 - 0.7546938285978846 %i]
@end group
@end example

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Exponential and logarithm functions)
@anchor{log}
@c @deffn {Function} log (@var{x})
m4_deffn({Function}, log, <<<(@var{x})>>>)

Represents the natural (base @math{e}) logarithm of @var{x}.

Maxima does not have a built-in function for the base 10 logarithm or other 
bases. @code{log10(x) := log(x) / log(10)} is an useful definition.

Simplification and evaluation of logarithms is governed by several global flags:

@table @code
@item @code{logexpand}
causes @code{log(a^b)} to become @code{b*log(a)}. If it is 
set to @code{all}, @code{log(a*b)} will also simplify to @code{log(a)+log(b)}.
If it is set to @code{super}, then @code{log(a/b)} will also simplify to 
@code{log(a)-log(b)} for rational numbers @code{a/b}, @code{a#1}. 
(@code{log(1/b)}, for @code{b} integer, always simplifies.) If it is set to 
@code{false}, all of these simplifications will be turned off.

@item @code{logsimp}
if @code{false} then no simplification of @code{%e} to a power containing 
@code{log}'s is done.

@item @code{lognegint}
if @code{true} implements the rule @code{log(-n)} -> @code{log(n)+%i*%pi} for 
@code{n} a positive integer.

@item @code{%e_to_numlog}
when @code{true}, @code{r} some rational number, and @code{x} some expression,
the expression @code{%e^(r*log(x))} will be simplified into @code{x^r}.  It
should be noted that the @code{radcan} command also does this transformation,
and more complicated transformations of this as well. The @code{logcontract} 
command "contracts" expressions containing @code{log}.
@end table

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Exponential and logarithm functions,  Integral calculus, Global flags)
@anchor{logabs}
@c @defvr {Option variable} logabs
m4_defvr({Option variable}, logabs)
Default value: @code{false}

When doing indefinite integration where logs are generated, e.g.
@code{integrate(1/x,x)}, the answer is given in terms of @code{log(abs(...))}
if @code{logabs} is @code{true}, but in terms of @code{log(...)} if
@code{logabs} is @code{false}.  For definite integration, the @code{logabs:true}
setting is used, because here "evaluation" of the indefinite integral at the
endpoints is often needed.

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @category{Integral calculus}
@c @category{Global flags}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c NEEDS EXAMPLES

@c -----------------------------------------------------------------------------
m4_setcat(Exponential and logarithm functions, Simplification functions)
@c @deffn {Function} logarc (@var{expr})
m4_deffn({Function}, logarc, <<<(@var{expr})>>>)

The function @code{logarc(@var{expr})} carries out the replacement of
inverse circular and hyperbolic functions with equivalent logarithmic
functions for an expression @var{expr} without setting the global
variable @code{logarc}.

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @category{Simplification flags and variables}
@c @category{Simplification functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{logarc}
@c @defvr  {Option variable} logarc
m4_defvr({Option variable}, logarc)

When the global variable @code{logarc} is @code{true},
inverse circular and hyperbolic functions are replaced by
equivalent logarithmic functions.
The default value of @code{logarc} is @code{false}.

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @category{Simplification flags and variables}
@c @category{Simplification functions}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
m4_setcat(Exponential and logarithm functions, Simplification flags and variables)
@anchor{logconcoeffp}
@c @defvr {Option variable} logconcoeffp
m4_defvr({Option variable}, logconcoeffp)
Default value: @code{false}

Controls which coefficients are
contracted when using @code{logcontract}.  It may be set to the name of a
predicate function of one argument.  E.g. if you like to generate
SQRTs, you can do @code{logconcoeffp:'logconfun$
logconfun(m):=featurep(m,integer) or ratnump(m)$} .  Then
@code{logcontract(1/2*log(x));} will give @code{log(sqrt(x))}.

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @category{Simplification flags and variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
m4_setcat(Exponential and logarithm functions)
@anchor{logcontract}
@c @deffn {Function} logcontract (@var{expr})
m4_deffn({Function}, logcontract, <<<(@var{expr})>>>)

Recursively scans the expression @var{expr}, transforming
subexpressions of the form @code{a1*log(b1) + a2*log(b2) + c} into
@code{log(ratsimp(b1^a1 * b2^a2)) + c}

@c ===beg===
@c 2*(a*log(x) + 2*a*log(y))$
@c logcontract(%);
@c ===end===
@example
(%i1) 2*(a*log(x) + 2*a*log(y))$
@group
(%i2) logcontract(%);
                                 2  4
(%o2)                     a log(x  y )
@end group
@end example

The declaration @code{declare(n,integer)} causes
@code{logcontract(2*a*n*log(x))} to simplify to @code{a*log(x^(2*n))}.  The
coefficients that "contract" in this manner are those such as the 2 and the
@code{n} here which satisfy @code{featurep(coeff,integer)}.  The user can
control which coefficients are contracted by setting the option
@code{logconcoeffp} to the name of a predicate function of one argument.
E.g. if you like to generate SQRTs, you can do @code{logconcoeffp:'logconfun$
logconfun(m):=featurep(m,integer) or ratnump(m)$} .  Then
@code{logcontract(1/2*log(x));} will give @code{log(sqrt(x))}.

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Exponential and logarithm functions, Simplification flags and variables)
@anchor{logexpand}
@c @defvr {Option variable} logexpand
m4_defvr({Option variable}, logexpand)
Default value: @code{true}

If @code{true}, that is the default value, causes @code{log(a^b)} to become
@code{b*log(a)}.  If it is set to @code{all}, @code{log(a*b)} will also simplify
to @code{log(a)+log(b)}.  If it is set to @code{super}, then @code{log(a/b)}
will also simplify to @code{log(a)-log(b)} for rational numbers @code{a/b},
@code{a#1}.  (@code{log(1/b)}, for integer @code{b}, always simplifies.) If it
is set to @code{false}, all of these simplifications will be turned off.

When @code{logexpand} is set to @code{all} or @code{super},
the logarithm of a product expression simplifies to a summation of logarithms.

Examples:

When @code{logexpand} is @code{true},
@code{log(a^b)} simplifies to @code{b*log(a)}.

@c ===beg===
@c log(n^2), logexpand=true;
@c ===end===
@example
(%i1) log(n^2), logexpand=true;
(%o1)                       2 log(n)
@end example

When @code{logexpand} is @code{all},
@code{log(a*b)} simplifies to @code{log(a)+log(b)}.

@c ===beg===
@c log(10*x), logexpand=all;
@c ===end===
@example
(%i1) log(10*x), logexpand=all;
(%o1)                   log(x) + log(10)
@end example

When @code{logexpand} is @code{super},
@code{log(a/b)} simplifies to @code{log(a)-log(b)}
for rational numbers @code{a/b} with @code{a#1}.

@c ===beg===
@c log(a/(n + 1)), logexpand=super;
@c ===end===
@example
(%i1) log(a/(n + 1)), logexpand=super;
(%o1)                  log(a) - log(n + 1)
@end example

When @code{logexpand} is set to @code{all} or @code{super},
the logarithm of a product expression simplifies to a summation of logarithms.

@c ===beg===
@c my_product : product (X(i), i, 1, n);
@c log(my_product), logexpand=all;
@c log(my_product), logexpand=super;
@c ===end===
@example
(%i1) my_product : product (X(i), i, 1, n);
                             n
                           /===\
                            ! !
(%o1)                       ! !  X(i)
                            ! !
                           i = 1
(%i2) log(my_product), logexpand=all;
                          n
                         ====
                         \
(%o2)                     >    log(X(i))
                         /
                         ====
                         i = 1
(%i3) log(my_product), logexpand=super;
                          n
                         ====
                         \
(%o3)                     >    log(X(i))
                         /
                         ====
                         i = 1
@end example

When @code{logexpand} is @code{false},
these simplifications are disabled.

@c ===beg===
@c logexpand : false $
@c log(n^2);
@c log(10*x);
@c log(a/(n + 1));
@c log ('product (X(i), i, 1, n));
@c ===end===
@example
(%i1) logexpand : false $
(%i2) log(n^2);
                                  2
(%o2)                        log(n )
(%i3) log(10*x);
(%o3)                       log(10 x)
(%i4) log(a/(n + 1));
                                 a
(%o4)                      log(-----)
                               n + 1
(%i5) log ('product (X(i), i, 1, n));
                               n
                             /===\
                              ! !
(%o5)                    log( ! !  X(i))
                              ! !
                             i = 1
@end example

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @category{Simplification flags and variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{lognegint}
@c @defvr {Option variable} lognegint
m4_defvr({Option variable}, lognegint)
Default value: @code{false}

If @code{true} implements the rule
@code{log(-n)} -> @code{log(n)+%i*%pi} for @code{n} a positive integer.

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @category{Simplification flags and variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{logsimp}
@c @defvr {Option variable} logsimp
m4_defvr({Option variable}, logsimp)
Default value: @code{true}

If @code{false} then no simplification of @code{%e} to a
power containing @code{log}'s is done.

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @category{Simplification flags and variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
m4_setcat(Exponential and logarithm functions, Complex variables)
@anchor{plog}
@c @deffn {Function} plog (@var{x})
m4_deffn({Function}, plog, <<<(@var{x})>>>)

Represents the principal branch of the complex-valued natural
logarithm with @code{-%pi} < @code{carg(@var{x})} <= @code{+%pi} .

@c @opencatbox
@c @category{Exponential and logarithm functions}
@c @category{Complex variables}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Mathematical functions)
@anchor{sqrt}
@c @deffn {Function} sqrt (@var{x})
m4_deffn({Function}, sqrt, <<<(@var{x})>>>)

The square root of @var{x}.  It is represented internally by
@code{@var{x}^(1/2)}.  See also @mref{rootscontract} and @mrefdot{radexpand}

@c @opencatbox
@c @category{Mathematical functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@page
@node Trigonometric Functions, Random Numbers, Root Exponential and Logarithmic Functions, Mathematical Functions
@section Trigonometric Functions
@c -----------------------------------------------------------------------------

@menu
* Introduction to Trigonometric::
* Functions and Variables for Trigonometric::
@end menu

@c -----------------------------------------------------------------------------
@node Introduction to Trigonometric, Functions and Variables for Trigonometric, Trigonometric Functions, Trigonometric Functions
@subsection Introduction to Trigonometric
@c -----------------------------------------------------------------------------

Maxima has many trigonometric functions defined.  Not all trigonometric
identities are programmed, but it is possible for the user to add many
of them using the pattern matching capabilities of the system.  The
trigonometric functions defined in Maxima are: @code{acos},
@code{acosh}, @code{acot}, @code{acoth}, @code{acsc},
@code{acsch}, @code{asec}, @code{asech}, @code{asin},
@code{asinh}, @code{atan}, @code{atanh}, @code{cos},
@code{cosh}, @code{cot}, @code{coth}, @code{csc}, @code{csch},
@code{sec}, @code{sech}, @code{sin}, @code{sinh}, @code{tan},
and @code{tanh}.  There are a number of commands especially for
handling trigonometric functions, see @code{trigexpand},
@code{trigreduce}, and the switch @code{trigsign}.  Two share
packages extend the simplification rules built into Maxima,
@code{ntrig} and @code{atrig1}.  Do @code{describe(@var{command})}
for details.

@opencatbox
@category{Trigonometric functions}
@closecatbox

@c -----------------------------------------------------------------------------
@node Functions and Variables for Trigonometric,  , Introduction to Trigonometric, Trigonometric Functions
@subsection Functions and Variables for Trigonometric
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions, Simplification flags and variables)
@anchor{%piargs}
@c @defvr {Option variable} %piargs
m4_defvr({Option variable}, %piargs)
Default value: @code{true}

When @code{%piargs} is @code{true},
trigonometric functions are simplified to algebraic constants
when the argument is an integer multiple of
@iftex
@math{\pi}, @math{\pi/2}, @math{\pi/3}, @math{\pi/4}, or @math{\pi/6}.
@end iftex
@ifnottex
@math{%pi}, @math{%pi/2}, @math{%pi/3}, @math{%pi/4}, or @math{%pi/6}.
@end ifnottex

@iftex
Maxima knows some identities which can be applied when @math{\pi}, etc.,
@end iftex
@ifnottex
Maxima knows some identities which can be applied when @math{%pi}, etc.,
@end ifnottex
are multiplied by an integer variable (that is, a symbol declared to be
integer).

Examples:

@c ===beg===
@c %piargs : false$
@c [sin (%pi), sin (%pi/2), sin (%pi/3)];
@c [sin (%pi/4), sin (%pi/5), sin (%pi/6)];
@c %piargs : true$
@c [sin (%pi), sin (%pi/2), sin (%pi/3)];
@c [sin (%pi/4), sin (%pi/5), sin (%pi/6)];
@c [cos (%pi/3), cos (10*%pi/3), tan (10*%pi/3),
@c        cos (sqrt(2)*%pi/3)];
@c ===end===
@example
(%i1) %piargs : false$
@group
(%i2) [sin (%pi), sin (%pi/2), sin (%pi/3)];
                                %pi       %pi
(%o2)            [sin(%pi), sin(---), sin(---)]
                                 2         3
@end group
@group
(%i3) [sin (%pi/4), sin (%pi/5), sin (%pi/6)];
                      %pi       %pi       %pi
(%o3)            [sin(---), sin(---), sin(---)]
                       4         5         6
@end group
(%i4) %piargs : true$
@group
(%i5) [sin (%pi), sin (%pi/2), sin (%pi/3)];
                                sqrt(3)
(%o5)                    [0, 1, -------]
                                   2
@end group
@group
(%i6) [sin (%pi/4), sin (%pi/5), sin (%pi/6)];
                         1         %pi   1
(%o6)                [-------, sin(---), -]
                      sqrt(2)       5    2
@end group
@group
(%i7) [cos (%pi/3), cos (10*%pi/3), tan (10*%pi/3),
       cos (sqrt(2)*%pi/3)];
                1    1               sqrt(2) %pi
(%o7)          [-, - -, sqrt(3), cos(-----------)]
                2    2                    3
@end group
@end example

@iftex
Some identities are applied when @math{\pi} and @math{\pi/2} are multiplied by
an integer variable.
@end iftex
@ifnottex
Some identities are applied when @math{%pi} and @math{%pi/2} are multiplied by
an integer variable.
@end ifnottex

@c ===beg===
@c declare (n, integer, m, even)$
@c [sin (%pi * n), cos (%pi * m), sin (%pi/2 * m),
@c        cos (%pi/2 * m)];
@c ===end===
@example
(%i1) declare (n, integer, m, even)$
@group
(%i2) [sin (%pi * n), cos (%pi * m), sin (%pi/2 * m),
       cos (%pi/2 * m)];
                                      m/2
(%o2)                  [0, 1, 0, (- 1)   ]
@end group
@end example

@c @opencatbox
@c @category{Trigonometric functions}
@c @category{Simplification flags and variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions, Hyperbolic functions, Simplification flags and variables)
@anchor{%iargs}
@c @defvr {Option variable} %iargs
m4_defvr({Option variable}, %iargs)
Default value: @code{true}

When @code{%iargs} is @code{true},
trigonometric functions are simplified to hyperbolic functions
@iftex
when the argument is apparently a multiple of the imaginary unit @math{i}.
@end iftex
@ifnottex
when the argument is apparently a multiple of the imaginary unit @math{%i}.
@end ifnottex

Even when the argument is demonstrably real, the simplification is applied;
@iftex
Maxima considers only whether the argument is a literal multiple of @math{i}.
@end iftex
@ifnottex
Maxima considers only whether the argument is a literal multiple of @math{%i}.
@end ifnottex

Examples:

@c ===beg===
@c %iargs : false$
@c [sin (%i * x), cos (%i * x), tan (%i * x)];
@c %iargs : true$
@c [sin (%i * x), cos (%i * x), tan (%i * x)];
@c ===end===
@example
(%i1) %iargs : false$
@group
(%i2) [sin (%i * x), cos (%i * x), tan (%i * x)];
(%o2)           [sin(%i x), cos(%i x), tan(%i x)]
@end group
(%i3) %iargs : true$
@group
(%i4) [sin (%i * x), cos (%i * x), tan (%i * x)];
(%o4)           [%i sinh(x), cosh(x), %i tanh(x)]
@end group
@end example

Even when the argument is demonstrably real, the simplification is applied.

@c ===beg===
@c declare (x, imaginary)$
@c [featurep (x, imaginary), featurep (x, real)];
@c sin (%i * x);
@c ===end===
@example
(%i1) declare (x, imaginary)$
@group
(%i2) [featurep (x, imaginary), featurep (x, real)];
(%o2)                     [true, false]
@end group
@group
(%i3) sin (%i * x);
(%o3)                      %i sinh(x)
@end group
@end example

@c @opencatbox
@c @category{Trigonometric functions}
@c @category{Hyperbolic functions}
@c @category{Simplification flags and variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions)
@anchor{acos}
@c @deffn {Function} acos (@var{x})
m4_deffn({Function}, acos, <<<(@var{x})>>>)

-- Arc Cosine.

@c @opencatbox
@c @category{Trigonometric functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Hyperbolic functions)
@anchor{acosh}
@c @deffn {Function} acosh (@var{x})
m4_deffn({Function}, acosh, <<<(@var{x})>>>)

-- Hyperbolic Arc Cosine.

@c @opencatbox
@c @category{Hyperbolic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions)
@anchor{acot}
@c @deffn {Function} acot (@var{x})
m4_deffn({Function}, acot, <<<(@var{x})>>>)

-- Arc Cotangent.

@c @opencatbox
@c @category{Trigonometric functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Hyperbolic functions)
@anchor{acoth}
@c @deffn {Function} acoth (@var{x})
m4_deffn({Function}, acoth, <<<(@var{x})>>>)

-- Hyperbolic Arc Cotangent.

@c @opencatbox
@c @category{Hyperbolic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions)
@anchor{acsc}
@c @deffn {Function} acsc (@var{x})
m4_deffn({Function}, acsc, <<<(@var{x})>>>)

-- Arc Cosecant.

@c @opencatbox
@c @category{Trigonometric functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Hyperbolic functions)
@anchor{acsch}
@c @deffn {Function} acsch (@var{x})
m4_deffn({Function}, acsch, <<<(@var{x})>>>)

-- Hyperbolic Arc Cosecant.

@c @opencatbox
@c @category{Hyperbolic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions)
@anchor{asec}
@c @deffn {Function} asec (@var{x})
m4_deffn({Function}, asec, <<<(@var{x})>>>)

-- Arc Secant.

@c @opencatbox
@c @category{Trigonometric functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Hyperbolic functions)
@anchor{asech}
@c @deffn {Function} asech (@var{x})
m4_deffn({Function}, asech, <<<(@var{x})>>>)

-- Hyperbolic Arc Secant.

@c @opencatbox
@c @category{pHyperbolic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions)
@anchor{asin}
@c @deffn {Function} asin (@var{x})
m4_deffn({Function}, asin, <<<(@var{x})>>>)

-- Arc Sine.

@c @opencatbox
@c @category{Trigonometric functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Hyperbolic functions)
@anchor{asinh}
@c @deffn {Function} asinh (@var{x})
m4_deffn({Function}, asinh, <<<(@var{x})>>>)

-- Hyperbolic Arc Sine.

@c @opencatbox
@c @category{Hyperbolic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions)
@anchor{atan}
@c @deffn {Function} atan (@var{x})
m4_deffn({Function}, atan, <<<(@var{x})>>>)

-- Arc Tangent.

See also @mref{atan2}.

@c @opencatbox
@c @category{Trigonometric functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions)
@anchor{atan2}
@c @deffn {Function} atan2 (@var{y}, @var{x})
m4_deffn({Function}, atan2, <<<(@var{y}, @var{x})>>>)

-- yields the value of @code{atan(@var{y}/@var{x})} in the interval @code{-%pi}
to @code{%pi}.

See also @mref{atan}.

@c @opencatbox
@c @category{Trigonometric functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Hyperbolic functions)
@anchor{atanh}
@c @deffn {Function} atanh (@var{x})
m4_deffn({Function}, atanh, <<<(@var{x})>>>)

-- Hyperbolic Arc Tangent.

@c @opencatbox
@c @category{Hyperbolic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c IS THIS DESCRIPTION ACCURATE ??
@c LET'S BE EXPLICIT ABOUT EXACTLY WHAT ARE THE RULES IMPLEMENTED BY THIS PACKAGE

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions, Package atrig1)
@anchor{atrig1}
@c @defvr {Package} atrig1
m4_defvr({Package}, atrig1)

The @code{atrig1} package contains several additional simplification rules
for inverse trigonometric functions.  Together with rules
already known to Maxima, the following angles are fully implemented:
@code{0}, @code{%pi/6}, @code{%pi/4}, @code{%pi/3}, and @code{%pi/2}.
Corresponding angles in the other three quadrants are also available.
Do @code{load("atrig1");} to use them.

@c @opencatbox
@c @category{Trigonometric functions}
@c @category{Package atrig1}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions)
@anchor{cos}
@c @deffn {Function} cos (@var{x})
m4_deffn({Function}, cos, <<<(@var{x})>>>)

-- Cosine.

@c @opencatbox
@c @category{Trigonometric functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Hyperbolic functions)
@anchor{cosh}
@c @deffn {Function} cosh (@var{x})
m4_deffn({Function}, cosh, <<<(@var{x})>>>)

-- Hyperbolic Cosine.

@c @opencatbox
@c @category{Hyperbolic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions)
@anchor{cot}
@c @deffn {Function} cot (@var{x})
m4_deffn({Function}, cot, <<<(@var{x})>>>)

-- Cotangent.

@c @opencatbox
@c @category{Trigonometric functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Hyperbolic functions)
@anchor{coth}
@c @deffn {Function} coth (@var{x})
m4_deffn({Function}, coth, <<<(@var{x})>>>)

-- Hyperbolic Cotangent.

@c @opencatbox
@c @category{Hyperbolic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions)
@anchor{csc}
@c @deffn {Function} csc (@var{x})
m4_deffn({Function}, csc, <<<(@var{x})>>>)

-- Cosecant.

@c @opencatbox
@c @category{Trigonometric functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Hyperbolic functions)
@anchor{csch}
@c @deffn {Function} csch (@var{x})
m4_deffn({Function}, csch, <<<(@var{x})>>>)

-- Hyperbolic Cosecant.

@c @opencatbox
@c @category{Hyperbolic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions,  Simplification flags and variables)
@anchor{halfangles}
@c @defvr {Option variable} halfangles
m4_defvr({Option variable}, halfangles)
Default value: @code{false}

When @code{halfangles} is @code{true}, trigonometric functions of arguments 
@code{@var{expr}/2} are simplified to functions of @var{expr}.

For a real argument @var{x} in the interval @code{0 < x < 2*%pi} the sine of 
the half-angle simplifies to a simple formula:

@example
                         sqrt(1 - cos(x))
                         ----------------
                             sqrt(2)
@end example

A complicated factor is needed to make this formula correct for all complex 
arguments @var{z}:

@verbatim
           realpart(z)
     floor(-----------)
              2 %pi
(- 1)                   (1 - unit_step(- imagpart(z))

                            realpart(z)            realpart(z)
                      floor(-----------) - ceiling(-----------)
                               2 %pi                  2 %pi
                ((- 1)                                          + 1))
@end verbatim

Maxima knows this factor and similar factors for the functions @code{sin}, 
@code{cos}, @code{sinh}, and @code{cosh}.  For special values of the argument 
@math{z} these factors simplify accordingly.

Examples:

@c ===beg===
@c halfangles : false$
@c sin (x / 2);
@c halfangles : true$
@c sin (x / 2);
@c assume(x>0, x<2*%pi)$
@c sin(x / 2);
@c ===end===
@example
(%i1) halfangles : false$
@group
(%i2) sin (x / 2);
                                 x
(%o2)                        sin(-)
                                 2
@end group
(%i3) halfangles : true$
@group
(%i4) sin (x / 2);
                            x
                    floor(-----)
                          2 %pi
               (- 1)             sqrt(1 - cos(x))
(%o4)          ----------------------------------
                            sqrt(2)
@end group
(%i5) assume(x>0, x<2*%pi)$
@group
(%i6) sin(x / 2);
                        sqrt(1 - cos(x))
(%o6)                   ----------------
                            sqrt(2)
@end group
@end example

@c @opencatbox
@c @category{Trigonometric functions}
@c @category{Simplification flags and variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c IS THIS DESCRIPTION ACCURATE ??
@c LET'S BE EXPLICIT ABOUT EXACTLY WHAT ARE THE RULES IMPLEMENTED BY THIS PACKAGE

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions, Package ntrig)
@c @defvr {Package} ntrig
m4_defvr({Package}, ntrig)

The @code{ntrig} package contains a set of simplification rules that are
used to simplify trigonometric function whose arguments are of the form
@code{@var{f}(@var{n} %pi/10)} where @var{f} is any of the functions
@code{sin}, @code{cos}, @code{tan}, @code{csc}, @code{sec} and @code{cot}.
@c NEED TO LOAD THIS PACKAGE ??

@c @opencatbox
@c @category{Trigonometric functions}
@c @category{Package ntrig}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions)
@anchor{sec}
@c @deffn {Function} sec (@var{x})
m4_deffn({Function}, sec, <<<(@var{x})>>>)

-- Secant.

@c @opencatbox
@c @category{Trigonometric functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Hyperbolic functions)
@anchor{sech}
@c @deffn {Function} sech (@var{x})
m4_deffn({Function}, sech, <<<(@var{x})>>>)

-- Hyperbolic Secant.

@c @opencatbox
@c @category{Hyperbolic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions)
@anchor{sin}
@c @deffn {Function} sin (@var{x})
m4_deffn({Function}, sin, <<<(@var{x})>>>)

-- Sine.

@c @opencatbox
@c @category{Trigonometric functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Hyperbolic functions)
@anchor{sinh}
@c @deffn {Function} sinh (@var{x})
m4_deffn({Function}, sinh, <<<(@var{x})>>>)

-- Hyperbolic Sine.

@c @opencatbox
@c @category{Hyperbolic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions)
@anchor{tan}
@c @deffn {Function} tan (@var{x})
m4_deffn({Function}, tan, <<<(@var{x})>>>)

-- Tangent.

@c @opencatbox
@c @category{Trigonometric functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Hyperbolic functions)
@anchor{tanh}
@c @deffn {Function} tanh (@var{x})
m4_deffn({Function}, tanh, <<<(@var{x})>>>)

-- Hyperbolic Tangent.

@c @opencatbox
@c @category{Hyperbolic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c NEEDS CLARIFICATION AND EXAMPLES

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions, Simplification functions)
@anchor{trigexpand}
@c @deffn {Function} trigexpand (@var{expr})
m4_deffn({Function}, trigexpand, <<<(@var{expr})>>>)

Expands trigonometric and hyperbolic functions of
sums of angles and of multiple angles occurring in @var{expr}.  For best
results, @var{expr} should be expanded.  To enhance user control of
simplification, this function expands only one level at a time,
expanding sums of angles or multiple angles.  To obtain full expansion
into sines and cosines immediately, set the switch @code{trigexpand: true}.

@code{trigexpand} is governed by the following global flags:

@table @code
@item trigexpand
If @code{true} causes expansion of all
expressions containing sin's and cos's occurring subsequently.
@item halfangles
If @code{true} causes half-angles to be simplified
away.
@item trigexpandplus
Controls the "sum" rule for @code{trigexpand},
expansion of sums (e.g. @code{sin(x + y)}) will take place only if
@code{trigexpandplus} is @code{true}.
@item trigexpandtimes
Controls the "product" rule for @code{trigexpand},
expansion of products (e.g. @code{sin(2 x)}) will take place only if
@code{trigexpandtimes} is @code{true}.
@end table

Examples:

@c ===beg===
@c x+sin(3*x)/sin(x),trigexpand=true,expand;
@c trigexpand(sin(10*x+y));
@c ===end===
@example
@group
(%i1) x+sin(3*x)/sin(x),trigexpand=true,expand;
                         2            2
(%o1)              (- sin (x)) + 3 cos (x) + x
@end group
@group
(%i2) trigexpand(sin(10*x+y));
(%o2)          cos(10 x) sin(y) + sin(10 x) cos(y)
@end group
@end example

@c @opencatbox
@c @category{Trigonometric functions}
@c @category{Simplification functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{trigexpandplus}
@c @defvr {Option variable} trigexpandplus
m4_defvr({Option variable}, trigexpandplus)
Default value: @code{true}

@code{trigexpandplus} controls the "sum" rule for
@code{trigexpand}.  Thus, when the @code{trigexpand} command is used or the
@code{trigexpand} switch set to @code{true}, expansion of sums
(e.g. @code{sin(x+y))} will take place only if @code{trigexpandplus} is
@code{true}.

@c @opencatbox
@c @category{Trigonometric functions}
@c @category{Simplification flags and variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{trigexpandtimes}
@c @defvr {Option variable} trigexpandtimes
m4_defvr({Option variable}, trigexpandtimes)
Default value: @code{true}

@code{trigexpandtimes} controls the "product" rule for @code{trigexpand}.
Thus, when the @code{trigexpand} command is used or the @code{trigexpand}
switch set to @code{true}, expansion of products (e.g. @code{sin(2*x)})
will take place only if @code{trigexpandtimes} is @code{true}.

@c @opencatbox
@c @category{Trigonometric functions}
@c @category{Simplification flags and variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{triginverses}
@c @defvr {Option variable} triginverses
m4_defvr({Option variable}, triginverses)
Default value: @code{true}

@code{triginverses} controls the simplification of the
composition of trigonometric and hyperbolic functions with their inverse
functions.

If @code{all}, both e.g. @code{atan(tan(@var{x}))}
and @code{tan(atan(@var{x}))} simplify to @var{x}.

If @code{true}, the @code{@var{arcfun}(@var{fun}(@var{x}))}
simplification is turned off.

If @code{false}, both the
@code{@var{arcfun}(@var{fun}(@var{x}))} and
@code{@var{fun}(@var{arcfun}(@var{x}))}
simplifications are turned off.

@c @opencatbox
@c @category{Trigonometric functions}
@c @category{Simplification flags and variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions, Simplification functions)
@anchor{trigreduce}
@c @deffn  {Function} trigreduce @
m4_deffn( {Function}, trigreduce, <<<>>>) @
@fname{trigreduce} (@var{expr}, @var{x}) @
@fname{trigreduce} (@var{expr})

Combines products and powers of trigonometric
and hyperbolic sin's and cos's of @var{x} into those of multiples of @var{x}.
It also tries to eliminate these functions when they occur in
denominators.  If @var{x} is omitted then all variables in @var{expr} are used.

See also @mref{poissimp}.

@c ===beg===
@c trigreduce(-sin(x)^2+3*cos(x)^2+x);
@c ===end===
@example
@group
(%i1) trigreduce(-sin(x)^2+3*cos(x)^2+x);
               cos(2 x)      cos(2 x)   1        1
(%o1)          -------- + 3 (-------- + -) + x - -
                  2             2       2        2
@end group
@end example

@c 
@c     OBSOLETE
@c     The behavior was changed in order to avoid calling expand in the core
@c     simplifier (trigi.lisp rev 1.31)
@c     See http://www.math.utexas.edu/pipermail/maxima/2008/010919.html.
@c 
@c The trigonometric simplification routines will use declared
@c information in some simple cases.  Declarations about variables are
@c used as follows, e.g.
@c 
@c ---beg---
@c declare(j, integer, e, even, o, odd)$
@c sin(x + (e + 1/2)*%pi);
@c sin(x + (o + 1/2)*%pi);
@c ---end---
@c @example
@c (%i1) declare(j, integer, e, even, o, odd)$
@c (%i2) sin(x + (e + 1/2)*%pi);
@c (%o2)                        cos(x)
@c (%i3) sin(x + (o + 1/2)*%pi);
@c (%o3)                       - cos(x)
@c @end example

@c @opencatbox
@c @category{Trigonometric functions}
@c @category{Simplification functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{trigsign}
@c @defvr {Option variable} trigsign
m4_defvr({Option variable}, trigsign)
Default value: @code{true}

When @code{trigsign} is @code{true}, it permits simplification of negative
arguments to trigonometric functions.  E.g., @code{sin(-x)} will become
@code{-sin(x)} only if @code{trigsign} is @code{true}.

@c @opencatbox
@c @category{Trigonometric functions}
@c @category{Simplification flags and variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
m4_setcat(Trigonometric functions, Simplification functions)
@anchor{trigsimp}
@c @deffn {Function} trigsimp (@var{expr})
m4_deffn({Function}, trigsimp, <<<(@var{expr})>>>)

Employs the identities
m4_mathjax(
<<<\(\sin\left(x\right)^2 + \cos\left(x\right)^2 = 1\)>>>,
<<<@math{sin(x)^2 + cos(x)^2 = 1}>>>,
<<<$\sin\left(x\right)^2 + \cos\left(x\right)^2 = 1$>>>)
and
m4_mathjax(
<<<\(\cosh\left(x\right)^2 - \sinh\left(x\right)^2 = 1\)>>>,
<<<@math{cosh(x)^2 - sinh(x)^2 = 1}>>>,
<<<$\cosh\left(x\right)^2 - \sinh\left(x\right)^2 = 1$>>>)
to simplify expressions containing @code{tan},
@code{sec}, etc., to @code{sin}, @code{cos}, @code{sinh}, @code{cosh}.

@code{trigreduce}, @code{ratsimp}, and @code{radcan} may be
able to further simplify the result.

@code{demo ("trgsmp.dem")} displays some examples of @code{trigsimp}.
@c MERGE EXAMPLES INTO THIS ITEM

@c @opencatbox
@c @category{Trigonometric functions}
@c @category{Simplification functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c NEEDS CLARIFICATION

@c -----------------------------------------------------------------------------
@anchor{trigrat}
@c @deffn {Function} trigrat (@var{expr})
m4_deffn({Function}, trigrat, <<<(@var{expr})>>>)

Gives a canonical simplified quasilinear form of a trigonometrical expression;
@var{expr} is a rational fraction of several @code{sin}, @code{cos} or
@code{tan}, the arguments of them are linear forms in some variables (or
kernels) and @code{%pi/@var{n}} (@var{n} integer) with integer coefficients.
The result is a simplified fraction with numerator and denominator linear in
@code{sin} and @code{cos}.  Thus @code{trigrat} linearize always when it is
possible.

@c ===beg===
@c trigrat(sin(3*a)/sin(a+%pi/3));
@c ===end===
@example
@group
(%i1) trigrat(sin(3*a)/sin(a+%pi/3));
(%o1)            sqrt(3) sin(2 a) + cos(2 a) - 1
@end group
@end example

The following example is taken from
Davenport, Siret, and Tournier, @i{Calcul Formel}, Masson (or in English,
Addison-Wesley), section 1.5.5, Morley theorem.

@c ===beg===
@c c : %pi/3 - a - b$
@c bc : sin(a)*sin(3*c)/sin(a+b);
@c ba : bc, c=a, a=c;
@c ac2 : ba^2 + bc^2 - 2*bc*ba*cos(b);
@c trigrat (ac2);
@c ===end===
@example
(%i1) c : %pi/3 - a - b$
@group
(%i2) bc : sin(a)*sin(3*c)/sin(a+b);
                                           %pi
                 sin(a) sin(3 ((- b) - a + ---))
                                            3
(%o2)            -------------------------------
                           sin(b + a)
@end group
@group
(%i3) ba : bc, c=a, a=c;
                                         %pi
                    sin(3 a) sin(b + a - ---)
                                          3
(%o3)               -------------------------
                                  %pi
                          sin(a - ---)
                                   3
@end group
@group
(%i4) ac2 : ba^2 + bc^2 - 2*bc*ba*cos(b);
         2         2         %pi
      sin (3 a) sin (b + a - ---)
                              3
(%o4) ---------------------------
                2     %pi
             sin (a - ---)
                       3
                                         %pi
 - (2 sin(a) sin(3 a) sin(3 ((- b) - a + ---)) cos(b)
                                          3
             %pi            %pi
 sin(b + a - ---))/(sin(a - ---) sin(b + a))
              3              3
      2       2                %pi
   sin (a) sin (3 ((- b) - a + ---))
                                3
 + ---------------------------------
                 2
              sin (b + a)
@end group
@group
(%i5) trigrat (ac2);
(%o5) - (sqrt(3) sin(4 b + 4 a) - cos(4 b + 4 a)
 - 2 sqrt(3) sin(4 b + 2 a) + 2 cos(4 b + 2 a)
 - 2 sqrt(3) sin(2 b + 4 a) + 2 cos(2 b + 4 a)
 + 4 sqrt(3) sin(2 b + 2 a) - 8 cos(2 b + 2 a) - 4 cos(2 b - 2 a)
 + sqrt(3) sin(4 b) - cos(4 b) - 2 sqrt(3) sin(2 b) + 10 cos(2 b)
 + sqrt(3) sin(4 a) - cos(4 a) - 2 sqrt(3) sin(2 a) + 10 cos(2 a)
 - 9)/4
@end group
@end example

@c @opencatbox
@c @category{Trigonometric functions}
@c @category{Simplification functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@page
@node Random Numbers, , Trigonometric Functions, Mathematical Functions
@section Random Numbers
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
m4_setcat(Random numbers)
@anchor{make_random_state}
@c @deffn  {Function} make_random_state @
m4_deffn( {Function}, make_random_state, <<<>>>) @
@fname{make_random_state} (@var{n}) @
@fname{make_random_state} (@var{s}) @
@fname{make_random_state} (true) @
@fname{make_random_state} (false)

@c OMIT THIS FOR NOW. SEE COMMENT BELOW.
@c @defunx make_random_state (@var{a})

A random state object represents the state of the random number generator.
The state comprises 627 32-bit words.

@code{make_random_state (@var{n})} returns a new random state object
created from an integer seed value equal to @var{n} modulo 2^32.
@var{n} may be negative.

@c OMIT THIS FOR NOW. NOT SURE HOW THIS IS SUPPOSED TO WORK.
@c @code{make_random_state (@var{a})} returns a new random state object
@c created from an array @var{a}, which must be a Lisp array of 32 unsigned bytes.

@code{make_random_state (@var{s})} returns a copy of the random state @var{s}.

@code{make_random_state (true)} returns a new random state object,
using the current computer clock time as the seed.

@code{make_random_state (false)} returns a copy of the current state
of the random number generator.

@c @opencatbox
@c @category{Random numbers}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{set_random_state}
@c @deffn {Function} set_random_state (@var{s})
m4_deffn({Function}, set_random_state, <<<(@var{s})>>>)

Copies @var{s} to the random number generator state.

@code{set_random_state} always returns @code{done}.

@c @opencatbox
@c @category{Random numbers}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Random numbers, Numerical methods)
@anchor{random}
@c @deffn {Function} random (@var{x})
m4_deffn({Function}, random, <<<(@var{x})>>>)

Returns a pseudorandom number.  If @var{x} is an integer,
@code{random (@var{x})} returns an integer from 0 through @code{@var{x} - 1}
inclusive.  If @var{x} is a floating point number, @code{random (@var{x})}
returns a nonnegative floating point number less than @var{x}.  @code{random}
complains with an error if @var{x} is neither an integer nor a float, or if
@var{x} is not positive.

The functions @code{make_random_state} and @code{set_random_state}
maintain the state of the random number generator.

The Maxima random number generator is an implementation of the Mersenne twister
MT 19937.

Examples:

@c ===beg===
@c s1: make_random_state (654321)$
@c set_random_state (s1);
@c random (1000);
@c random (9573684);
@c random (2^75);
@c s2: make_random_state (false)$
@c random (1.0);
@c random (10.0);
@c random (100.0);
@c set_random_state (s2);
@c random (1.0);
@c random (10.0);
@c random (100.0);
@c ===end===
@example
(%i1) s1: make_random_state (654321)$
@group
(%i2) set_random_state (s1);
(%o2)                         done
@end group
@group
(%i3) random (1000);
(%o3)                          768
@end group
@group
(%i4) random (9573684);
(%o4)                        7657880
@end group
@group
(%i5) random (2^75);
(%o5)                11804491615036831636390
@end group
(%i6) s2: make_random_state (false)$
@group
(%i7) random (1.0);
(%o7)                  0.2310127244107132
@end group
@group
(%i8) random (10.0);
(%o8)                   4.394553645870825
@end group
@group
(%i9) random (100.0);
(%o9)                   32.28666704056853
@end group
@group
(%i10) set_random_state (s2);
(%o10)                        done
@end group
@group
(%i11) random (1.0);
(%o11)                 0.2310127244107132
@end group
@group
(%i12) random (10.0);
(%o12)                  4.394553645870825
@end group
@group
(%i13) random (100.0);
(%o13)                  32.28666704056853
@end group
@end example

@c @opencatbox
@c @category{Random numbers}
@c @category{Numerical methods}
@c @closecatbox
@c @end deffn
m4_end_deffn()

