@c -*- Mode: texinfo -*-
@menu
* Functions and Variables for Number Theory::  
@end menu

@c -----------------------------------------------------------------------------
@node Functions and Variables for Number Theory,  , Number Theory, Number Theory
@section Functions and Variables for Number Theory
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{bern}
@c @deffn {Function} bern (@var{n})
m4_deffn({Function}, bern, <<<(@var{n})>>>)

Returns the @var{n}'th Bernoulli number for integer @var{n}.
@c WELL, ACTUALLY bern SIMPLIFIES, LIKE FACTORIAL -- DO WE WANT TO GET INTO THAT ???
@c OR JUST PRETEND IT'S "RETURNED" ???
Bernoulli numbers equal to zero are suppressed if @code{zerobern} is
@code{false}.

See also @mrefdot{burn}

@example
(%i1) zerobern: true$
(%i2) map (bern, [0, 1, 2, 3, 4, 5, 6, 7, 8]);
                      1  1       1      1        1
(%o2)           [1, - -, -, 0, - --, 0, --, 0, - --]
                      2  6       30     42       30
(%i3) zerobern: false$
(%i4) map (bern, [0, 1, 2, 3, 4, 5, 6, 7, 8]);
                      1  1    1   1     1   5     691   7
(%o4)           [1, - -, -, - --, --, - --, --, - ----, -]
                      2  6    30  42    30  66    2730  6
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{bernpoly}
@c @deffn {Function} bernpoly (@var{x}, @var{n})
m4_deffn({Function}, bernpoly, <<<(@var{x}, @var{n})>>>)

Returns the @var{n}'th Bernoulli polynomial in the
variable @var{x}.

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory, Numerical evaluation)
@anchor{bfzeta}
@c @deffn {Function} bfzeta (@var{s}, @var{n})
m4_deffn({Function}, bfzeta, <<<(@var{s}, @var{n})>>>)

Returns the Riemann zeta function for the argument @var{s}.
The return value is a big float (bfloat);
@var{n} is the number of digits in the return value.

@c @opencatbox
@c @category{Number theory}
@c @category{Numerical evaluation}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory, Numerical evaluation)
@anchor{bfhzeta}
@c @deffn {Function} bfhzeta (@var{s}, @var{h}, @var{n})
m4_deffn({Function}, bfhzeta, <<<(@var{s}, @var{h}, @var{n})>>>)

Returns the Hurwitz zeta function for the arguments @var{s} and @var{h}.
The return value is a big float (bfloat);
@var{n} is the number of digits in the return value.

The Hurwitz zeta function is defined as

m4_mathjax(
<<<$$\zeta \left(s,h\right) = \sum_{k=0}^\infty {1 \over
\left(k+h\right)^{s}}$$>>>,
<<<@example
                        inf
                        ====
                        \        1
         zeta (s,h)  =   >    --------
                        /            s
                        ====  (k + h)
                        k = 0
@end example
>>>)

@code{load ("bffac")} loads this function.

@c @opencatbox
@c @category{Number theory}
@c @category{Numerical evaluation}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{burn}
@c @deffn {Function} burn (@var{n})
m4_deffn({Function}, burn, <<<(@var{n})>>>)

Returns a rational number, which is an approximation of the @var{n}'th Bernoulli
number for integer @var{n}.  @code{burn} exploits the observation that
(rational) Bernoulli numbers can be approximated by (transcendental) zetas with
tolerable efficiency:

@example
                   n - 1  1 - 2 n
              (- 1)      2        zeta(2 n) (2 n)!
     B(2 n) = ------------------------------------
                                2 n
                             %pi
@end example

@code{burn} may be more efficient than @mref{bern} for large, isolated @var{n}
as @mref{bern} computes all the Bernoulli numbers up to index @var{n} before 
returning.  @code{burn} invokes the approximation for even integers @var{n} >
255.  For odd integers and @var{n} <= 255 the function @mref{bern} is called.

@code{load ("bffac")} loads this function.  See also @mrefdot{bern}

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{chinese}
@c @deffn {Function} chinese ([@var{r_1}, @dots{}, @var{r_n}], [@var{m_1}, @dots{}, @var{m_n}])
m4_deffn({Function}, chinese, <<<([@var{r_1}, @dots{}, @var{r_n}], [@var{m_1}, @dots{}, @var{m_n}])>>>)

Solves the system of congruences @code{x = r_1 mod m_1}, @dots{}, @code{x = r_n mod m_n}.
The remainders @var{r_n} may be arbitrary integers while the moduli @var{m_n} have to be 
positive and pairwise coprime integers.

@example
(%i1) mods : [1000, 1001, 1003, 1007];
(%o1)                   [1000, 1001, 1003, 1007]
(%i2) lreduce('gcd, mods);
(%o2)                               1
(%i3) x : random(apply("*", mods));
(%o3)                         685124877004
(%i4) rems : map(lambda([z], mod(x, z)), mods);
(%o4)                       [4, 568, 54, 624]
(%i5) chinese(rems, mods);
(%o5)                         685124877004
(%i6) chinese([1, 2], [3, n]);
(%o6)                    chinese([1, 2], [3, n])
(%i7) %, n = 4;
(%o7)                              10
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Continued fractions)
@anchor{cf}
@c @deffn {Function} cf (@var{expr})
m4_deffn({Function}, cf, <<<(@var{expr})>>>)

Computes a continued fraction approximation.
@var{expr} is an expression comprising continued fractions,
square roots of integers, and literal real numbers
(integers, rational numbers, ordinary floats, and bigfloats).
@code{cf} computes exact expansions for rational numbers,
but expansions are truncated at @code{ratepsilon} for ordinary floats
and @code{10^(-fpprec)} for bigfloats.

Operands in the expression may be combined with arithmetic operators.
Maxima does not know about operations on continued fractions
outside of @code{cf}.

@code{cf} evaluates its arguments after binding @code{listarith} to
@code{false}.  @code{cf} returns a continued fraction, represented as a list.

A continued fraction @code{a + 1/(b + 1/(c + ...))} is represented by the list
@code{[a, b, c, ...]}.  The list elements @code{a}, @code{b}, @code{c}, @dots{}
must evaluate to integers.  @var{expr} may also contain @code{sqrt (n)} where
@code{n} is an integer.  In this case @code{cf} will give as many terms of the
continued fraction as the value of the variable @mref{cflength} times the
period.

A continued fraction can be evaluated to a number by evaluating the arithmetic
representation returned by @mrefdot{cfdisrep}  See also @mref{cfexpand} for
another way to evaluate a continued fraction.

See also @mrefcomma{cfdisrep} @mrefcomma{cfexpand} and @mrefdot{cflength}

Examples:

@itemize @bullet
@item
@var{expr} is an expression comprising continued fractions and square roots of
integers.

@example
(%i1) cf ([5, 3, 1]*[11, 9, 7] + [3, 7]/[4, 3, 2]);
(%o1)               [59, 17, 2, 1, 1, 1, 27]
(%i2) cf ((3/17)*[1, -2, 5]/sqrt(11) + (8/13));
(%o2)        [0, 1, 1, 1, 3, 2, 1, 4, 1, 9, 1, 9, 2]
@end example

@item
@code{cflength} controls how many periods of the continued fraction
are computed for algebraic, irrational numbers.

@example
(%i1) cflength: 1$
(%i2) cf ((1 + sqrt(5))/2);
(%o2)                    [1, 1, 1, 1, 2]
(%i3) cflength: 2$
(%i4) cf ((1 + sqrt(5))/2);
(%o4)               [1, 1, 1, 1, 1, 1, 1, 2]
(%i5) cflength: 3$
(%i6) cf ((1 + sqrt(5))/2);
(%o6)           [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2]
@end example

@item
A continued fraction can be evaluated by evaluating the arithmetic
representation returned by @mrefdot{cfdisrep}

@example
(%i1) cflength: 3$
(%i2) cfdisrep (cf (sqrt (3)))$
(%i3) ev (%, numer);
(%o3)                   1.731707317073171
@end example

@item
Maxima does not know about operations on continued fractions outside of
@code{cf}.

@example
(%i1) cf ([1,1,1,1,1,2] * 3);
(%o1)                     [4, 1, 5, 2]
(%i2) cf ([1,1,1,1,1,2]) * 3;
(%o2)                  [3, 3, 3, 3, 3, 6]
@end example

@end itemize

@c @opencatbox
@c @category{Continued fractions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c NEEDS CLARIFICATION -- MAKE EXPLICIT HOW list IS RELATED TO a, b, c, ...
@c ALSO, CAN list CONTAIN ANYTHING OTHER THAN LITERAL INTEGERS ??

@c -----------------------------------------------------------------------------
@anchor{cfdisrep}
@c @deffn {Function} cfdisrep (@var{list})
m4_deffn({Function}, cfdisrep, <<<(@var{list})>>>)

Constructs and returns an ordinary arithmetic expression
of the form @code{a + 1/(b + 1/(c + ...))}
from the list representation of a continued fraction @code{[a, b, c, ...]}.

@example
(%i1) cf ([1, 2, -3] + [1, -2, 1]);
(%o1)                     [1, 1, 1, 2]
(%i2) cfdisrep (%);
                                  1
(%o2)                     1 + ---------
                                    1
                              1 + -----
                                      1
                                  1 + -
                                      2
@end example

@c @opencatbox
@c @category{Continued fractions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{cfexpand}
@c @deffn {Function} cfexpand (@var{x})
m4_deffn({Function}, cfexpand, <<<(@var{x})>>>)

Returns a matrix of the numerators and denominators of the last (column 1) and
next-to-last (column 2) convergents of the continued fraction @var{x}.

@example
(%i1) cf (rat (ev (%pi, numer)));

`rat' replaced 3.141592653589793 by 103993/33102 =3.141592653011902
(%o1)                  [3, 7, 15, 1, 292]
(%i2) cfexpand (%); 
                         [ 103993  355 ]
(%o2)                    [             ]
                         [ 33102   113 ]
(%i3) %[1,1]/%[2,1], numer;
(%o3)                   3.141592653011902
@end example

@c @opencatbox
@c @category{Continued fractions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Continued fractions)
@anchor{cflength}
@c @defvr {Option variable} cflength
m4_defvr({Option variable}, cflength)
Default value: 1

@code{cflength} controls the number of terms of the continued fraction the
function @code{cf} will give, as the value @code{cflength} times the period.
Thus the default is to give one period.

@example
(%i1) cflength: 1$
(%i2) cf ((1 + sqrt(5))/2);
(%o2)                    [1, 1, 1, 1, 2]
(%i3) cflength: 2$
(%i4) cf ((1 + sqrt(5))/2);
(%o4)               [1, 1, 1, 1, 1, 1, 1, 2]
(%i5) cflength: 3$
(%i6) cf ((1 + sqrt(5))/2);
(%o6)           [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2]
@end example

@c @opencatbox
@c @category{Continued fractions}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{divsum}
@c @deffn  {Function} divsum @
m4_deffn( {Function}, divsum, <<<>>>) @
@fname{divsum} (@var{n}, @var{k}) @
@fname{divsum} (@var{n})

@code{divsum (@var{n}, @var{k})} returns the sum of the divisors of @var{n}
raised to the @var{k}'th power.

@code{divsum (@var{n})} returns the sum of the divisors of @var{n}.

@example
(%i1) divsum (12);
(%o1)                          28
(%i2) 1 + 2 + 3 + 4 + 6 + 12;
(%o2)                          28
(%i3) divsum (12, 2);
(%o3)                          210
(%i4) 1^2 + 2^2 + 3^2 + 4^2 + 6^2 + 12^2;
(%o4)                          210
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{euler}
@c @deffn {Function} euler (@var{n})
m4_deffn({Function}, euler, <<<(@var{n})>>>)

Returns the @var{n}'th Euler number for nonnegative integer @var{n}.
Euler numbers equal to zero are suppressed if @code{zerobern} is
@code{false}.

For the Euler-Mascheroni constant, see @code{%gamma}.

@example
(%i1) zerobern: true$
(%i2) map (euler, [0, 1, 2, 3, 4, 5, 6]);
(%o2)               [1, 0, - 1, 0, 5, 0, - 61]
(%i3) zerobern: false$
(%i4) map (euler, [0, 1, 2, 3, 4, 5, 6]);
(%o4)               [1, - 1, 5, - 61, 1385, - 50521, 2702765]
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{factors_only}
@c @defvr {Option variable} factors_only
m4_defvr({Option variable}, factors_only)
Default value: @code{false}

Controls the value returned by @mrefdot{ifactors} The default @code{false} 
causes @code{ifactors} to provide information about multiplicities of the 
computed prime factors. If @code{factors_only} is set to @code{true}, 
@code{ifactors} returns nothing more than a list of prime factors.

Example: See @mrefdot{ifactors}

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{fib}
@c @deffn {Function} fib (@var{n})
m4_deffn({Function}, fib, <<<(@var{n})>>>)

Returns the @var{n}'th Fibonacci number.
@code{fib(0)} is equal to 0 and @code{fib(1)} equal to 1, and 
@code{fib (-@var{n})} equal to @code{(-1)^(@var{n} + 1) * fib(@var{n})}.

After calling @code{fib},
@code{prevfib} is equal to @code{fib(@var{n} - 1)},
the Fibonacci number preceding the last one computed.

@example
(%i1) map (fib, [-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8]);
(%o1)           [- 3, 2, - 1, 1, 0, 1, 1, 2, 3, 5, 8, 13, 21]
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{fibtophi}
@c @deffn {Function} fibtophi (@var{expr})
m4_deffn({Function}, fibtophi, <<<(@var{expr})>>>)

Expresses Fibonacci numbers in @var{expr} in terms of the constant @code{%phi},
which is @code{(1 + sqrt(5))/2}, approximately 1.61803399.

Examples:

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

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{ifactors}
@c @deffn {Function} ifactors (@var{n})
m4_deffn({Function}, ifactors, <<<(@var{n})>>>)

For a positive integer @var{n} returns the factorization of @var{n}.  If
@code{n=p1^e1..pk^nk} is the decomposition of @var{n} into prime
factors, ifactors returns @code{[[p1, e1], ... , [pk, ek]]}.

Factorization methods used are trial divisions by primes up to 9973,
Pollard's rho and p-1 method and elliptic curves.

If the variable @code{ifactor_verbose} is set to @code{true}
ifactor produces detailed output about what it is doing including
immediate feedback as soon as a factor has been found.

The value returned by @code{ifactors} is controlled by the option variable @mrefdot{factors_only}
The default @code{false} causes @code{ifactors} to provide information about 
the multiplicities of the computed prime factors. If @code{factors_only} 
is set to @code{true}, @code{ifactors} simply returns the list of 
prime factors.

@example
(%i1) ifactors(51575319651600);
(%o1)     [[2, 4], [3, 2], [5, 2], [1583, 1], [9050207, 1]]
(%i2) apply("*", map(lambda([u], u[1]^u[2]), %));
(%o2)                        51575319651600
(%i3) ifactors(51575319651600), factors_only : true;
(%o3)                   [2, 3, 5, 1583, 9050207]
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{igcdex}
@c @deffn {Function} igcdex (@var{n}, @var{k})
m4_deffn({Function}, igcdex, <<<(@var{n}, @var{k})>>>)

Returns a list @code{[@var{a}, @var{b}, @var{u}]} where @var{u} is the greatest
common divisor of @var{n} and @var{k}, and @var{u} is equal to
@code{@var{a} @var{n} + @var{b} @var{k}}.  The arguments @var{n} and @var{k}
must be integers.

@code{igcdex} implements the Euclidean algorithm.  See also @mrefdot{gcdex}

The command @code{load("gcdex")} loads the function.

Examples:

@example
(%i1) load("gcdex")$

(%i2) igcdex(30,18);
(%o2)                      [- 1, 2, 6]
(%i3) igcdex(1526757668, 7835626735736);
(%o3)            [845922341123, - 164826435, 4]
(%i4) igcdex(fib(20), fib(21));
(%o4)                   [4181, - 2584, 1]
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{inrt}
@c @deffn {Function} inrt (@var{x}, @var{n})
m4_deffn({Function}, inrt, <<<(@var{x}, @var{n})>>>)

Returns the integer @var{n}'th root of the absolute value of @var{x}.

@example
(%i1) l: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]$
(%i2) map (lambda ([a], inrt (10^a, 3)), l);
(%o2) [2, 4, 10, 21, 46, 100, 215, 464, 1000, 2154, 4641, 10000]
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{inv_mod}
@c @deffn {Function} inv_mod (@var{n}, @var{m})
m4_deffn({Function}, inv_mod, <<<(@var{n}, @var{m})>>>)

Computes the inverse of @var{n} modulo @var{m}.
@code{inv_mod (n,m)} returns @code{false}, 
if @var{n} is a zero divisor modulo @var{m}.

@example
(%i1) inv_mod(3, 41);
(%o1)                           14
(%i2) ratsimp(3^-1), modulus = 41;
(%o2)                           14
(%i3) inv_mod(3, 42);
(%o3)                          false
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Mathematical functions)
@anchor{isqrt}
@c @deffn {Function} isqrt (@var{x})
m4_deffn({Function}, isqrt, <<<(@var{x})>>>)

Returns the "integer square root" of the absolute value of @var{x}, which is an
integer.

@c @opencatbox
@c @category{Mathematical functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{jacobi}
@c @deffn {Function} jacobi (@var{p}, @var{q})
m4_deffn({Function}, jacobi, <<<(@var{p}, @var{q})>>>)

Returns the Jacobi symbol of @var{p} and @var{q}.

@example
(%i1) l: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]$
(%i2) map (lambda ([a], jacobi (a, 9)), l);
(%o2)         [1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0]
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{lcm}
@c @deffn {Function} lcm (@var{expr_1}, @dots{}, @var{expr_n})
m4_deffn({Function}, lcm, <<<(@var{expr_1}, @dots{}, @var{expr_n})>>>)

Returns the least common multiple of its arguments.
The arguments may be general expressions as well as integers.

@code{load ("functs")} loads this function.

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{lucas}
@c @deffn {Function} lucas (@var{n})
m4_deffn({Function}, lucas, <<<(@var{n})>>>)

Returns the @var{n}'th Lucas number.
@code{lucas(0)} is equal to 2 and @code{lucas(1)} equal to 1, and 
@code{lucas(-@var{n})} equal to @code{(-1)^(-@var{n}) * lucas(@var{n})}.

@example
(%i1) map (lucas, [-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8]);
(%o1)             [7, - 4, 3, - 1, 2, 1, 3, 4, 7, 11, 18, 29, 47]
@end example

After calling @code{lucas}, the global variable
@code{next_lucas} is equal to @code{lucas (@var{n} + 1)},
the Lucas number following the last returned. The example shows 
how Fibonacci numbers can be computed via @code{lucas} and @code{next_lucas}. 

@example
(%i1) fib_via_lucas(n) := 
         block([lucas : lucas(n)],
         signum(n) * (2*next_lucas - lucas)/5 )$
(%i2) map (fib_via_lucas, [-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8]);
(%o2)             [- 3, 2, - 1, 1, 0, 1, 1, 2, 3, 5, 8, 13, 21]
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Mathematical functions)
@anchor{mod}
@c @deffn {Function} mod (@var{x}, @var{y})
m4_deffn({Function}, mod, <<<(@var{x}, @var{y})>>>)

If @var{x} and @var{y} are real numbers and @var{y} is nonzero, return
@code{@var{x} - @var{y} * floor(@var{x} / @var{y})}.  Further for all real
@var{x}, we have @code{mod (@var{x}, 0) = @var{x}}.  For a discussion of the
definition @code{mod (@var{x}, 0) = @var{x}}, see Section 3.4, of
"Concrete Mathematics," by Graham, Knuth, and Patashnik.  The function
@code{mod (@var{x}, 1)} is a sawtooth function with period 1 with
@code{mod (1, 1) = 0} and @code{mod (0, 1) = 0}.

To find the principal argument (a number in the interval @code{(-%pi, %pi]}) of
a complex number, use the function
@code{@var{x} |-> %pi - mod (%pi - @var{x}, 2*%pi)}, where @var{x} is an
argument.

When @var{x} and @var{y} are constant expressions (@code{10 * %pi}, for 
example), @code{mod} uses the same big float evaluation scheme that @code{floor}
and @code{ceiling} uses.  Again, it's possible, although unlikely, that
@code{mod} could return an erroneous value in such cases.

For nonnumerical arguments @var{x} or @var{y}, @code{mod} knows several
simplification rules:

@c ===beg===
@c mod (x, 0);
@c mod (a*x, a*y);
@c mod (0, x);
@c ===end===
@example
(%i1) mod (x, 0);
(%o1)                           x
(%i2) mod (a*x, a*y);
(%o2)                      a mod(x, y)
(%i3) mod (0, x);
(%o3)                           0
@end example

@c @opencatbox
@c @category{Mathematical functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{next_prime}
@c @deffn {Function} next_prime (@var{n})
m4_deffn({Function}, next_prime, <<<(@var{n})>>>)

Returns the smallest prime bigger than @var{n}.

@example
(%i1) next_prime(27);
(%o1)                       29
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat()
@anchor{partfrac}
@c @deffn {Function} partfrac (@var{expr}, @var{var})
m4_deffn({Function}, partfrac, <<<(@var{expr}, @var{var})>>>)

Expands the expression @var{expr} in partial fractions
with respect to the main variable @var{var}.  @code{partfrac} does a complete
partial fraction decomposition.  The algorithm employed is based on
the fact that the denominators of the partial fraction expansion (the
factors of the original denominator) are relatively prime.  The
numerators can be written as linear combinations of denominators, and
the expansion falls out.

@code{partfrac} ignores the value @code{true} of the option variable
@code{keepfloat}.

@example
(%i1) 1/(1+x)^2 - 2/(1+x) + 2/(2+x);
                      2       2        1
(%o1)               ----- - ----- + --------
                    x + 2   x + 1          2
                                    (x + 1)
(%i2) ratsimp (%);
                                 x
(%o2)                 - -------------------
                         3      2
                        x  + 4 x  + 5 x + 2
(%i3) partfrac (%, x);
                      2       2        1
(%o3)               ----- - ----- + --------
                    x + 2   x + 1          2
                                    (x + 1)
@end example
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{power_mod}
@c @deffn {Function} power_mod (@var{a}, @var{n}, @var{m})
m4_deffn({Function}, power_mod, <<<(@var{a}, @var{n}, @var{m})>>>)

Uses a modular algorithm to compute @code{a^n mod m} 
where @var{a} and @var{n} are integers and @var{m} is a positive integer.
If @var{n} is negative, @code{inv_mod} is used to find the modular inverse.

@example
(%i1) power_mod(3, 15, 5);
(%o1)                          2
(%i2) mod(3^15,5);
(%o2)                          2
(%i3) power_mod(2, -1, 5);
(%o3)                          3
(%i4) inv_mod(2,5);
(%o4)                          3
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Predicate functions, Number theory)
@anchor{primep}
@c @deffn {Function} primep (@var{n})
m4_deffn({Function}, primep, <<<(@var{n})>>>)

Primality test.  If @code{primep (@var{n})} returns @code{false}, @var{n} is a
composite number and if it returns @code{true}, @var{n} is a prime number
with very high probability.

For @var{n} less than 341550071728321 a deterministic version of
Miller-Rabin's test is used.  If @code{primep (@var{n})} returns
@code{true}, then @var{n} is a prime number.

For @var{n} bigger than 341550071728321 @code{primep} uses
@code{primep_number_of_tests} Miller-Rabin's pseudo-primality tests and one 
Lucas pseudo-primality test.  The probability that a non-prime @var{n} will 
pass one Miller-Rabin test is less than 1/4.  Using the default value 25 for
@code{primep_number_of_tests}, the probability of @var{n} being
composite is much smaller that 10^-15.

@c @opencatbox
@c @category{Predicate functions}
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{primep_number_of_tests}
@c @defvr {Option variable} primep_number_of_tests
m4_defvr({Option variable}, primep_number_of_tests)
Default value: 25

Number of Miller-Rabin's tests used in @code{primep}.

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{primes}
@c @deffn {Function} primes (@var{start}, @var{end})
m4_deffn({Function}, primes, <<<(@var{start}, @var{end})>>>)

Returns the list of all primes from @var{start} to @var{end}.

@example
(%i1) primes(3, 7);
(%o1)                     [3, 5, 7]
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{prev_time}
@c @deffn {Function} prev_prime (@var{n})
m4_deffn({Function}, prev_prime, <<<(@var{n})>>>)

Returns the greatest prime smaller than @var{n}.

@example
(%i1) prev_prime(27);
(%o1)                       23
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{qunit}
@c @deffn {Function} qunit (@var{n})
m4_deffn({Function}, qunit, <<<(@var{n})>>>)

Returns the principal unit of the real quadratic number field
@code{sqrt (@var{n})} where @var{n} is an integer,
i.e., the element whose norm is unity.
This amounts to solving Pell's equation @code{a^2 - @var{n} b^2 = 1}.

@example
(%i1) qunit (17);
(%o1)                     sqrt(17) + 4
(%i2) expand (% * (sqrt(17) - 4));
(%o2)                           1
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{totient}
@c @deffn {Function} totient (@var{n})
m4_deffn({Function}, totient, <<<(@var{n})>>>)

Returns the number of integers less than or equal to @var{n} which
are relatively prime to @var{n}.

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@c @defvr {Option variable} zerobern
m4_defvr({Option variable}, zerobern)
Default value: @code{true}

When @code{zerobern} is @code{false}, @code{bern} excludes the Bernoulli numbers
and @code{euler} excludes the Euler numbers which are equal to zero.
See @code{bern} and @code{euler}.

@c @opencatbox
@c @category{pNumber theory}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{zeta}
@c @deffn {Function} zeta (@var{n})
m4_deffn({Function}, zeta, <<<(@var{n})>>>)

Returns the Riemann zeta function.  If @var{n} is a negative integer, 0, or a
positive even integer, the Riemann zeta function simplifies to an exact value.
For a positive even integer the option variable @code{zeta%pi} has to be
@code{true} in addition (See @code{zeta%pi}).  For a floating point or bigfloat
number the Riemann zeta function is evaluated numerically.  Maxima returns a
noun form @code{zeta (@var{n})} for all other arguments, including rational
noninteger, and complex arguments, or for even integers, if @code{zeta%pi} has
the value @code{false}.

@code{zeta(1)} is undefined, but Maxima knows the limit 
@code{limit(zeta(x), x, 1)} from above and below.

The Riemann zeta function distributes over lists, matrices, and equations.

See also @mref{bfzeta} and @mrefdot{zeta%pi}

Examples:

@c ===beg===
@c zeta([-2, -1, 0, 0.5, 2, 3,1+%i]);
@c limit(zeta(x),x,1,plus);
@c limit(zeta(x),x,1,minus);
@c ===end===
@example
(%i1) zeta([-2, -1, 0, 0.5, 2, 3, 1+%i]);
                                             2
            1     1                       %pi
(%o1) [0, - --, - -, - 1.460354508809586, ----, zeta(3), 
            12    2                        6
                                                    zeta(%i + 1)]
(%i2) limit(zeta(x),x,1,plus);
(%o2)                          inf
(%i3) limit(zeta(x),x,1,minus);
(%o3)                         minf
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{zeta%pi}
@c @defvr {Option variable} zeta%pi
m4_defvr({Option variable}, zeta%pi)
Default value: @mref{true}

When @code{zeta%pi} is @code{true}, @code{zeta} returns an expression 
proportional to @code{%pi^n} for even integer @code{n}.  Otherwise, @code{zeta} 
returns a noun form @code{zeta (n)} for even integer @code{n}.

Examples:

@c ===beg===
@c zeta%pi: true$
@c zeta (4);
@c zeta%pi: false$
@c zeta (4);
@c ===end===
@example
(%i1) zeta%pi: true$
(%i2) zeta (4);
                                 4
                              %pi
(%o2)                         ----
                               90
(%i3) zeta%pi: false$
(%i4) zeta (4);
(%o4)                        zeta(4)
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
@anchor{zn_add_table}
@c @deffn {Function} zn_add_table (@var{n}) 
m4_deffn({Function}, zn_add_table, <<<(@var{n})>>>) 

Shows an addition table of all elements in (Z/@var{n}Z).

See also @mrefcomma{zn_mult_table}  @mrefdot{zn_power_table}

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{zn_characteristic_factors}
@c @deffn {Function} zn_characteristic_factors (@var{n}) 
m4_deffn({Function}, zn_characteristic_factors, <<<(@var{n})>>>) 

Returns a list containing the characteristic factors of the totient of @var{n}.

Using the characteristic factors a multiplication group modulo @var{n} 
can be expressed as a group direct product of cyclic subgroups.

In case the group itself is cyclic the list only contains the totient 
and using @code{zn_primroot} a generator can be computed. 
If the totient splits into more than one characteristic factors 
@code{zn_factor_generators} finds generators of the corresponding subgroups.
 
Each of the @code{r} factors in the list divides the right following factors. 
For the last factor @code{f_r} therefore holds @code{a^f_r = 1 (mod n)} 
for all @code{a} coprime to @var{n}.  
This factor is also known as Carmichael function or Carmichael lambda.

If @code{n > 2}, then @code{totient(n)/2^r} is the number of quadratic residues, 
and each of these has @code{2^r} square roots.

See also @mrefcomma{totient}  @mrefcomma{zn_primroot}  @mrefdot{zn_factor_generators}

Examples:

The multiplication group modulo @code{14} is cyclic and its @code{6} elements 
can be generated by a primitive root.

@example
(%i1) [zn_characteristic_factors(14), phi: totient(14)];
(%o1)                              [[6], 6]
(%i2) [zn_factor_generators(14), g: zn_primroot(14)];
(%o2)                              [[3], 3]
(%i3) M14: makelist(power_mod(g,i,14), i,0,phi-1);
(%o3)                         [1, 3, 9, 13, 11, 5]
@end example

The multiplication group modulo @code{15} is not cyclic and its @code{8} elements 
can be generated by two factor generators.

@example
(%i1) [[f1,f2]: zn_characteristic_factors(15), totient(15)];
(%o1)                             [[2, 4], 8]
(%i2) [[g1,g2]: zn_factor_generators(15), zn_primroot(15)];
(%o2)                           [[11, 7], false]
(%i3) UG1: makelist(power_mod(g1,i,15), i,0,f1-1);
(%o3)                               [1, 11]
(%i4) UG2: makelist(power_mod(g2,i,15), i,0,f2-1);
(%o4)                            [1, 7, 4, 13]
(%i5) M15: create_list(mod(i*j,15), i,UG1, j,UG2);
(%o5)                      [1, 7, 4, 13, 11, 2, 14, 8]
@end example

For the last characteristic factor @code{4} it holds that @code{a^4 = 1 (mod 15)} 
for all @code{a} in @code{M15}. 

@code{M15} has two characteristic factors and therefore @code{8/2^2} quadratic residues, 
and each of these has @code{2^2} square roots.

@example
(%i6) zn_power_table(15);
                               [ 1   1  1   1 ]
                               [              ]
                               [ 2   4  8   1 ]
                               [              ]
                               [ 4   1  4   1 ]
                               [              ]
                               [ 7   4  13  1 ]
(%o6)                          [              ]
                               [ 8   4  2   1 ]
                               [              ]
                               [ 11  1  11  1 ]
                               [              ]
                               [ 13  4  7   1 ]
                               [              ]
                               [ 14  1  14  1 ]
(%i7) map(lambda([i], zn_nth_root(i,2,15)), [1,4]);
(%o7)                   [[1, 4, 11, 14], [2, 7, 8, 13]]
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{zn_carmichael_lambda}
@c @deffn {Function} zn_carmichael_lambda (@var{n}) 
m4_deffn({Function}, zn_carmichael_lambda, <<<(@var{n})>>>) 

Returns @code{1} if @var{n} is @code{1} and otherwise 
the greatest characteristic factor of the totient of @var{n}.

For remarks and examples see @mrefdot{zn_characteristic_factors}

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{zn_determinant}
@c @deffn {Function} zn_determinant (@var{matrix}, @var{p}) 
m4_deffn({Function}, zn_determinant, <<<(@var{matrix}, @var{p})>>>) 

Uses the technique of LU-decomposition to compute the determinant of @var{matrix} 
over (Z/@var{p}Z). @var{p} must be a prime. 

However if the determinant is equal to zero the LU-decomposition might fail. 
In that case @code{zn_determinant} computes the determinant non-modular 
and reduces thereafter.

See also @mrefdot{zn_invert_by_lu}

Examples:

@example
(%i1) m : matrix([1,3],[2,4]);
                                [ 1  3 ]
(%o1)                           [      ]
                                [ 2  4 ]
(%i2) zn_determinant(m, 5);
(%o2)                               3
(%i3) m : matrix([2,4,1],[3,1,4],[4,3,2]);
                               [ 2  4  1 ]
                               [         ]
(%o3)                          [ 3  1  4 ]
                               [         ]
                               [ 4  3  2 ]
(%i4) zn_determinant(m, 5);
(%o4)                               0
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{zn_factor_generators}
@c @deffn {Function} zn_factor_generators (@var{n}) 
m4_deffn({Function}, zn_factor_generators, <<<(@var{n})>>>) 

Returns a list containing factor generators corresponding to the 
characteristic factors of the totient of @var{n}.

For remarks and examples see @mrefdot{zn_characteristic_factors}

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{zn_invert_by_lu}
@c @deffn {Function} zn_invert_by_lu (@var{matrix}, @var{p}) 
m4_deffn({Function}, zn_invert_by_lu, <<<(@var{matrix}, @var{p})>>>) 

Uses the technique of LU-decomposition to compute the modular inverse of 
@var{matrix} over (Z/@var{p}Z). @var{p} must be a prime and @var{matrix} 
invertible. @code{zn_invert_by_lu} returns @code{false} if @var{matrix} 
is not invertible.

See also @mrefdot{zn_determinant}

Example:

@example
(%i1) m : matrix([1,3],[2,4]);
                                [ 1  3 ]
(%o1)                           [      ]
                                [ 2  4 ]
(%i2) zn_determinant(m, 5);
(%o2)                               3
(%i3) mi : zn_invert_by_lu(m, 5);
                                [ 3  4 ]
(%o3)                           [      ]
                                [ 1  2 ]
(%i4) matrixmap(lambda([a], mod(a, 5)), m . mi);
                                [ 1  0 ]
(%o4)                           [      ]
                                [ 0  1 ]
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{zn_log}
@c @deffn {Function} zn_log @
m4_deffn({Function}, zn_log, <<<>>>) @
@fname{zn_log} (@var{a}, @var{g}, @var{n})  @
@fname{zn_log} (@var{a}, @var{g}, @var{n}, [[@var{p1}, @var{e1}], @dots{}, [@var{pk}, @var{ek}]])

Computes the discrete logarithm. Let (Z/@var{n}Z)* be a cyclic group, @var{g} a 
primitive root modulo @var{n} or a generator of a subgroup of (Z/@var{n}Z)* 
and let @var{a} be a member of this group.  
@code{zn_log (a, g, n)} then solves the congruence @code{g^x = a mod n}.
Please note that if @var{a} is not a power of @var{g} modulo @var{n}, 
@code{zn_log} will not terminate.

The applied algorithm needs a prime factorization of @code{zn_order(g)} resp. @code{totient(n)} 
in case @var{g} is a primitive root modulo @var{n}. 
A precomputed list of factors of @code{zn_order(g)} might be used as the optional fourth argument.
This list must be of the same form as the list returned by @code{ifactors(zn_order(g))} 
using the default option @code{factors_only : false}.
However, compared to the running time of the logarithm algorithm 
providing the list of factors has only a quite small effect.

The algorithm uses a Pohlig-Hellman-reduction and Pollard's Rho-method for 
discrete logarithms. The running time of @code{zn_log} primarily depends on the 
bitlength of the greatest prime factor of @code{zn_order(g)}.

See also @mrefcomma{zn_primroot}  @mrefcomma{zn_order}  @mrefcomma{ifactors}  @mrefdot{totient}

Examples:

@code{zn_log (a, g, n)} solves the congruence @code{g^x = a mod n}.

@example
(%i1) n : 22$
(%i2) g : zn_primroot(n);
(%o2)                               7
(%i3) ord_7 : zn_order(7, n);
(%o3)                              10
(%i4) powers_7 : makelist(power_mod(g, x, n), x, 0, ord_7 - 1);
(%o4)              [1, 7, 5, 13, 3, 21, 15, 17, 9, 19]
(%i5) zn_log(9, g, n);
(%o5)                               8
(%i6) map(lambda([x], zn_log(x, g, n)), powers_7);
(%o6)                [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
(%i7) ord_5 : zn_order(5, n);
(%o7)                               5
(%i8) powers_5 : makelist(power_mod(5,x,n), x, 0, ord_5 - 1);
(%o8)                       [1, 5, 3, 15, 9]
(%i9) zn_log(9, 5, n);
(%o9)                               4
@end example

The optional fourth argument must be of the same form as the list returned by 
@code{ifactors(zn_order(g))}.
The running time primarily depends on the bitlength of the totient's greatest prime factor.

@example
(%i1) (p : 2^127-1, primep(p));
(%o1)                             true
(%i2) ifs : ifactors(p - 1)$
(%i3) g : zn_primroot(p, ifs);
(%o3)                              43
(%i4) a : power_mod(g, 4711, p)$
(%i5) zn_log(a, g, p, ifs);
(%o5)                             4711
(%i6) f_max : last(ifs);  
(%o6)                       [77158673929, 1]
(%i7) ord_5 : zn_order(5,p,ifs)$
(%i8) (p - 1)/ord_5;
(%o8)                              73
(%i9) ifs_5 : ifactors(ord_5)$
(%i10) a : power_mod(5, 4711, p)$
(%i11) zn_log(a, 5, p, ifs_5);
(%o11)                            4711
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{zn_mult_table}
@c @deffn {Function} zn_mult_table @
m4_deffn({Function}, zn_mult_table, <<<>>>) @
@fname{zn_mult_table} (@var{n})  @
@fname{zn_mult_table} (@var{n}, @var{gcd})

Without the optional argument @var{gcd} @code{zn_mult_table(n)} shows a 
multiplication table of all elements in (Z/@var{n}Z)* which are all elements 
coprime to @var{n}.

The optional second argument @var{gcd} allows to select a specific 
subset of (Z/@var{n}Z). If @var{gcd} is an integer, a multiplication table of 
all residues @code{x} with @code{gcd(x,n) = }@var{gcd} are returned.
Additionally row and column headings are added for better readability. 
If necessary, these can be easily removed by @code{submatrix(1, table, 1)}. 
 
If @var{gcd} is set to @code{all}, the table is printed for all non-zero 
elements in (Z/@var{n}Z).

The second example shows an alternative way to create a multiplication table 
for subgroups.

See also @mrefcomma{zn_add_table}  @mrefdot{zn_power_table}

Examples:

The default table shows all elements in (Z/@var{n}Z)* and allows to demonstrate 
and study basic properties of modular multiplication groups.
E.g. the principal diagonal contains all quadratic residues, 
each row and column contains every element, the tables are symmetric, etc..

If @var{gcd} is set to @code{all}, the table is printed for all non-zero 
elements in (Z/@var{n}Z).

@example
(%i1) zn_mult_table(8);
                                [ 1  3  5  7 ]
                                [            ]
                                [ 3  1  7  5 ]
(%o1)                           [            ]
                                [ 5  7  1  3 ]
                                [            ]
                                [ 7  5  3  1 ]
(%i2) zn_mult_table(8, all);
                            [ 1  2  3  4  5  6  7 ]
                            [                     ]
                            [ 2  4  6  0  2  4  6 ]
                            [                     ]
                            [ 3  6  1  4  7  2  5 ]
                            [                     ]
(%o2)                       [ 4  0  4  0  4  0  4 ]
                            [                     ]
                            [ 5  2  7  4  1  6  3 ]
                            [                     ]
                            [ 6  4  2  0  6  4  2 ]
                            [                     ]
                            [ 7  6  5  4  3  2  1 ]
@end example

If @var{gcd} is an integer, row and column headings are added for better readability. 

If the subset chosen by @var{gcd} is a group there is another way to create 
a multiplication table. An isomorphic mapping from a group with @code{1} as 
identity builds a table which is easy to read. The mapping is accomplished via CRT.

In the second version of @code{T36_4} the identity, here @code{28}, is placed in 
the top left corner, just like in table @code{T9}. 

@example
(%i1) T36_4: zn_mult_table(36,4);
                        [ *   4   8   16  20  28  32 ]
                        [                            ]
                        [ 4   16  32  28  8   4   20 ]
                        [                            ]
                        [ 8   32  28  20  16  8   4  ]
                        [                            ]
(%o1)                   [ 16  28  20  4   32  16  8  ]
                        [                            ]
                        [ 20  8   16  32  4   20  28 ]
                        [                            ]
                        [ 28  4   8   16  20  28  32 ]
                        [                            ]
                        [ 32  20  4   8   28  32  16 ]
(%i2) T9: zn_mult_table(36/4);
                             [ 1  2  4  5  7  8 ]
                             [                  ]
                             [ 2  4  8  1  5  7 ]
                             [                  ]
                             [ 4  8  7  2  1  5 ]
(%o2)                        [                  ]
                             [ 5  1  2  7  8  4 ]
                             [                  ]
                             [ 7  5  1  8  4  2 ]
                             [                  ]
                             [ 8  7  5  4  2  1 ]
(%i3) T36_4: matrixmap(lambda([x], chinese([0,x],[4,9])), T9);
                          [ 28  20  4   32  16  8  ]
                          [                        ]
                          [ 20  4   8   28  32  16 ]
                          [                        ]
                          [ 4   8   16  20  28  32 ]
(%o3)                     [                        ]
                          [ 32  28  20  16  8   4  ]
                          [                        ]
                          [ 16  32  28  8   4   20 ]
                          [                        ]
                          [ 8   16  32  4   20  28 ]
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{zn_nth_root}
@c @deffn {Function} zn_nth_root @
m4_deffn({Function}, zn_nth_root, <<<>>>) @
@fname{zn_nth_root} (@var{x}, @var{n}, @var{m})  @
@fname{zn_nth_root} (@var{x}, @var{n}, @var{m}, [[@var{p1}, @var{e1}], @dots{}, [@var{pk}, @var{ek}]])

Returns a list with all @var{n}-th roots of @var{x} from the multiplication 
subgroup of (Z/@var{m}Z) which contains @var{x}, or @code{false}, if @var{x} 
is no @var{n}-th power modulo @var{m} or not contained in any multiplication 
subgroup of (Z/@var{m}Z).

@var{x} is an element of a multiplication subgroup modulo @var{m}, if the 
greatest common divisor @code{g = gcd(x,m)} is coprime to @code{m/g}.

@code{zn_nth_root} is based on an algorithm by Adleman, Manders and Miller 
and on theorems about modulo multiplication groups by Daniel Shanks.
 
The algorithm needs a prime factorization of the modulus @var{m}. 
So in case the factorization of @var{m} is known, the list of factors 
can be passed as the fourth argument. This optional argument
must be of the same form as the list returned by @code{ifactors(m)} 
using the default option @code{factors_only: false}.


Examples:

A power table of the multiplication group modulo @code{14} 
followed by a list of lists containing all @var{n}-th roots of @code{1} 
with @var{n} from @code{1} to @code{6}.

@example
(%i1) zn_power_table(14);
                         [ 1   1   1   1   1   1 ]
                         [                       ]
                         [ 3   9   13  11  5   1 ]
                         [                       ]
                         [ 5   11  13  9   3   1 ]
(%o1)                    [                       ]
                         [ 9   11  1   9   11  1 ]
                         [                       ]
                         [ 11  9   1   11  9   1 ]
                         [                       ]
                         [ 13  1   13  1   13  1 ]
(%i2) makelist(zn_nth_root(1,n,14), n,1,6);
(%o2)  [[1], [1, 13], [1, 9, 11], [1, 13], [1], [1, 3, 5, 9, 11, 13]]
@end example

In the following example @var{x} is not coprime to @var{m}, 
but is a member of a multiplication subgroup of (Z/@var{m}Z) 
and any @var{n}-th root is a member of the same subgroup.

The residue class @code{3} is no member of any multiplication subgroup of (Z/63Z) 
and is therefore not returned as a third root of @code{27}.

Here @code{zn_power_table} shows all residues @code{x} in (Z/63Z) 
with @code{gcd(x,63) = 9}. This subgroup is isomorphic to (Z/7Z)*  
and its identity @code{36} is computed via CRT.

@example
(%i1) m: 7*9$

(%i2) zn_power_table(m,9);
                         [ 9   18  36  9   18  36 ]
                         [                        ]
                         [ 18  9   36  18  9   36 ]
                         [                        ]
                         [ 27  36  27  36  27  36 ]
(%o2)                    [                        ]
                         [ 36  36  36  36  36  36 ]
                         [                        ]
                         [ 45  9   27  18  54  36 ]
                         [                        ]
                         [ 54  18  27  9   45  36 ]
(%i3) zn_nth_root(27,3,m);
(%o3)                           [27, 45, 54]
(%i4) id7:1$  id63_9: chinese([id7,0],[7,9]);
(%o5)                                36
@end example

In the following RSA-like example, where the modulus @code{N} is squarefree, 
i.e. it splits into 
exclusively first power factors, every @code{x} from @code{0} to @code{N-1} 
is contained in a multiplication subgroup.

The process of decryption needs the @code{e}-th root. 
@code{e} is coprime to @code{totient(N)} and therefore the @code{e}-th root is unique. 
In this case @code{zn_nth_root} effectively performs CRT-RSA. 
(Please note that @code{flatten} removes braces but no solutions.)

@example
(%i1) [p,q,e]: [5,7,17]$  N: p*q$

(%i3) xs: makelist(x,x,0,N-1)$

(%i4) ys: map(lambda([x],power_mod(x,e,N)),xs)$

(%i5) zs: flatten(map(lambda([y], zn_nth_root(y,e,N)), ys))$

(%i6) is(zs = xs);
(%o6)                             true
@end example

In the following example the factorization of the modulus is known 
and passed as the fourth argument.

@example
(%i1) p: 2^107-1$  q: 2^127-1$  N: p*q$

(%i4) ibase: obase: 16$

(%i5) msg: 11223344556677889900aabbccddeeff$

(%i6) enc: power_mod(msg, 10001, N);
(%o6)    1a8db7892ae588bdc2be25dd5107a425001fe9c82161abc673241c8b383
(%i7) zn_nth_root(enc, 10001, N, [[p,1],[q,1]]);
(%o7)               [11223344556677889900aabbccddeeff]
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{zn_order}
@c @deffn {Function} zn_order @
m4_deffn({Function}, zn_order, <<<>>>) @
@fname{zn_order} (@var{x}, @var{n})  @
@fname{zn_order} (@var{x}, @var{n}, [[@var{p1}, @var{e1}], @dots{}, [@var{pk}, @var{ek}]])

Returns the order of @var{x} if it is an unit of the finite group (Z/@var{n}Z)* 
or returns @code{false}.  @var{x} is an unit modulo @var{n} if it is coprime to @var{n}.

The applied algorithm needs a prime factorization of @code{totient(n)}. This factorization 
might be time consuming in some cases and it can be useful to factor first 
and then to pass the list of factors to @code{zn_log} as the third argument. 
The list must be of the same form as the list returned by @code{ifactors(totient(n))} 
using the default option @code{factors_only : false}.

See also @mrefcomma{zn_primroot}  @mrefcomma{ifactors}  @mrefdot{totient}

Examples:

@code{zn_order} computes the order of the unit @var{x} in (Z/@var{n}Z)*.

@example
(%i1) n : 22$
(%i2) g : zn_primroot(n);
(%o2)                               7
(%i3) units_22 : sublist(makelist(i,i,1,21), lambda([x], gcd(x, n) = 1));
(%o3)              [1, 3, 5, 7, 9, 13, 15, 17, 19, 21]
(%i4) (ord_7 : zn_order(7, n)) = totient(n);
(%o4)                            10 = 10
(%i5) powers_7 : makelist(power_mod(g,i,n), i,0,ord_7 - 1);
(%o5)              [1, 7, 5, 13, 3, 21, 15, 17, 9, 19]
(%i6) map(lambda([x], zn_order(x, n)), powers_7);
(%o6)              [1, 10, 5, 10, 5, 2, 5, 10, 5, 10]
(%i7) map(lambda([x], ord_7/gcd(x, ord_7)), makelist(i, i,0,ord_7 - 1));
(%o7)              [1, 10, 5, 10, 5, 2, 5, 10, 5, 10]
(%i8) totient(totient(n));
(%o8)                               4
@end example

The optional third argument must be of the same form as the list returned by 
@code{ifactors(totient(n))}.

@example
(%i1) (p : 2^142 + 217, primep(p));
(%o1)                             true
(%i2) ifs : ifactors( totient(p) )$
(%i3) g : zn_primroot(p, ifs);
(%o3)                               3
(%i4) is( (ord_3 : zn_order(g, p, ifs)) = totient(p) );
(%o4)                             true
(%i5) map(lambda([x], ord_3/zn_order(x, p, ifs)), makelist(i,i,2,15));
(%o5)        [22, 1, 44, 10, 5, 2, 22, 2, 8, 2, 1, 1, 20, 1]
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{zn_power_table}
@c @deffn {Function} zn_power_table @
m4_deffn({Function}, zn_power_table, <<<>>>) @
@fname{zn_power_table} (@var{n})  @
@fname{zn_power_table} (@var{n}, @var{gcd})  @
@fname{zn_power_table} (@var{n}, @var{gcd}, @var{max_exp})

Without any optional argument @code{zn_power_table(n)} 
shows a power table of all elements in (Z/@var{n}Z)* 
which are all residue classes coprime to @var{n}. 
The exponent loops from @code{1} to the greatest characteristic factor of 
@code{totient(n)} (also known as Carmichael function or Carmichael lambda)
and the table ends with a column of ones on the right side. 

The optional second argument @var{gcd} allows to select powers of a specific 
subset of (Z/@var{n}Z). If @var{gcd} is an integer, powers of all residue 
classes @code{x} with @code{gcd(x,n) = }@var{gcd} are returned,
i.e. the default value for @var{gcd} is @code{1}.   
If @var{gcd} is set to @code{all}, the table contains powers of all elements 
in (Z/@var{n}Z).

If the optional third argument @var{max_exp} is given, the exponent loops from 
@code{1} to @var{max_exp}. 

See also @mrefcomma{zn_add_table}  @mrefdot{zn_mult_table}

Examples:

The default which is @var{gcd}@code{ = 1} allows to demonstrate and study basic 
theorems of e.g. Fermat and Euler.

The argument @var{gcd} allows to select subsets of (Z/@var{n}Z) and to study 
multiplication subgroups and isomorphisms. 
E.g. the groups @code{G10} and @code{G10_2} are under multiplication both 
isomorphic to @code{G5}. @code{1} is the identity in @code{G5}. 
So are @code{1} resp. @code{6} the identities in @code{G10} resp. @code{G10_2}. 
There are corresponding mappings for primitive roots, n-th roots, etc..

@example
(%i1) zn_power_table(10);
                              [ 1  1  1  1 ]
                              [            ]
                              [ 3  9  7  1 ]
(%o1)                         [            ]
                              [ 7  9  3  1 ]
                              [            ]
                              [ 9  1  9  1 ]
(%i2) zn_power_table(10,2);
                              [ 2  4  8  6 ]
                              [            ]
                              [ 4  6  4  6 ]
(%o2)                         [            ]
                              [ 6  6  6  6 ]
                              [            ]
                              [ 8  4  2  6 ]
(%i3) zn_power_table(10,5);
(%o3)                         [ 5  5  5  5 ]
(%i4) zn_power_table(10,10);
(%o4)                         [ 0  0  0  0 ]
(%i5) G5: [1,2,3,4];
(%o6)                          [1, 2, 3, 4]
(%i6) G10_2: map(lambda([x], chinese([0,x],[2,5])), G5);
(%o6)                          [6, 2, 8, 4]
(%i7) G10: map(lambda([x], power_mod(3, zn_log(x,2,5), 10)), G5);
(%o7)                          [1, 3, 7, 9]
@end example

If @var{gcd} is set to @code{all}, the table contains powers of all elements 
in (Z/@var{n}Z).

The third argument @var{max_exp} allows to set the highest exponent. 
The following table shows a very small example of RSA.

@example
(%i1) N:2*5$ phi:totient(N)$ e:7$ d:inv_mod(e,phi)$

(%i5) zn_power_table(N, all, e*d);
       [ 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 ]
       [                                                               ]
       [ 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 ]
       [                                                               ]
       [ 2  4  8  6  2  4  8  6  2  4  8  6  2  4  8  6  2  4  8  6  2 ]
       [                                                               ]
       [ 3  9  7  1  3  9  7  1  3  9  7  1  3  9  7  1  3  9  7  1  3 ]
       [                                                               ]
       [ 4  6  4  6  4  6  4  6  4  6  4  6  4  6  4  6  4  6  4  6  4 ]
(%o5)  [                                                               ]
       [ 5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5 ]
       [                                                               ]
       [ 6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6 ]
       [                                                               ]
       [ 7  9  3  1  7  9  3  1  7  9  3  1  7  9  3  1  7  9  3  1  7 ]
       [                                                               ]
       [ 8  4  2  6  8  4  2  6  8  4  2  6  8  4  2  6  8  4  2  6  8 ]
       [                                                               ]
       [ 9  1  9  1  9  1  9  1  9  1  9  1  9  1  9  1  9  1  9  1  9 ]
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
@anchor{zn_primroot}
@c @deffn {Function} zn_primroot @
m4_deffn({Function}, zn_primroot, <<<>>>) @
@fname{zn_primroot} (@var{n})  @
@fname{zn_primroot} (@var{n}, [[@var{p1}, @var{e1}], @dots{}, [@var{pk}, @var{ek}]])

If the multiplicative group (Z/@var{n}Z)* is cyclic, @code{zn_primroot} computes the 
smallest primitive root modulo @var{n}.  (Z/@var{n}Z)* is cyclic if @var{n} is equal to 
@code{2}, @code{4}, @code{p^k} or @code{2*p^k}, where @code{p} is prime and 
greater than @code{2} and @code{k} is a natural number.  @code{zn_primroot} 
performs an according pretest if the option variable @mref{zn_primroot_pretest}
(default: @code{false}) is set to @code{true}.  In any case the computation is limited 
by the upper bound @mrefdot{zn_primroot_limit}

If (Z/@var{n}Z)* is not cyclic or if there is no primitive root up to 
@code{zn_primroot_limit}, @code{zn_primroot} returns @code{false}.

The applied algorithm needs a prime factorization of @code{totient(n)}. This factorization 
might be time consuming in some cases and it can be useful to factor first 
and then to pass the list of factors to @code{zn_log} as an additional argument. 
The list must be of the same form as the list returned by @code{ifactors(totient(n))} 
using the default option @code{factors_only : false}.

See also @mrefcomma{zn_primroot_p}  @mrefcomma{zn_order}  @mrefcomma{ifactors}  @mrefdot{totient}

Examples:

@code{zn_primroot} computes the smallest primitive root modulo @var{n} or returns 
@code{false}.

@example
(%i1) n : 14$
(%i2) g : zn_primroot(n);
(%o2)                               3
(%i3) zn_order(g, n) = totient(n);
(%o3)                             6 = 6
(%i4) n : 15$
(%i5) zn_primroot(n);
(%o5)                             false
@end example

The optional second argument must be of the same form as the list returned by 
@code{ifactors(totient(n))}.

@example
(%i1) (p : 2^142 + 217, primep(p));
(%o1)                             true
(%i2) ifs : ifactors( totient(p) )$
(%i3) g : zn_primroot(p, ifs);
(%o3)                               3
(%i4) [time(%o2), time(%o3)];
(%o4)                    [[15.556972], [0.004]]
(%i5) is(zn_order(g, p, ifs) = p - 1);
(%o5)                             true
(%i6) n : 2^142 + 216$
(%i7) ifs : ifactors(totient(n))$
(%i8) zn_primroot(n, ifs), 
      zn_primroot_limit : 200, zn_primroot_verbose : true;
`zn_primroot' stopped at zn_primroot_limit = 200
(%o8)                             false
@end example

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{zn_primroot_limit}
@c @defvr {Option variable} zn_primroot_limit
m4_defvr({Option variable}, zn_primroot_limit)
Default value: @code{1000} 

If @mref{zn_primroot} cannot find a primitve root, it stops at this upper bound. 
If the option variable @mref{zn_primroot_verbose} (default: @code{false}) is 
set to @code{true}, a message will be printed when @code{zn_primroot_limit} is reached. 

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
m4_setcat(Predicate functions, Number theory)
@anchor{zn_primroot_p}
@c @deffn {Function} zn_primroot_p @
m4_deffn({Function}, zn_primroot_p, <<<>>>) @
@fname{zn_primroot_p} (@var{x}, @var{n})  @
@fname{zn_primroot_p} (@var{x}, @var{n}, [[@var{p1}, @var{e1}], @dots{}, [@var{pk}, @var{ek}]])

Checks whether @var{x} is a primitive root in the multiplicative group (Z/@var{n}Z)*.

The applied algorithm needs a prime factorization of @code{totient(n)}. This factorization 
might be time consuming and in case @code{zn_primroot_p} will be consecutively 
applied to a list of candidates it can be useful to factor first and then to 
pass the list of factors to @code{zn_log} as a third argument. 
The list must be of the same form as the list returned by @code{ifactors(totient(n))} 
using the default option @code{factors_only : false}.

See also @mrefcomma{zn_primroot}  @mrefcomma{zn_order}  @mrefcomma{ifactors}  @mrefdot{totient}

Examples:

@code{zn_primroot_p} as a predicate function.

@example
(%i1) n : 14$
(%i2) units_14 : sublist(makelist(i,i,1,13), lambda([i], gcd(i, n) = 1));
(%o2)                     [1, 3, 5, 9, 11, 13]
(%i3) zn_primroot_p(13, n);
(%o3)                            false
(%i4) sublist(units_14, lambda([x], zn_primroot_p(x, n)));
(%o4)                            [3, 5]
(%i5) map(lambda([x], zn_order(x, n)), units_14);
(%o5)                      [1, 6, 6, 3, 3, 2]
@end example

The optional third argument must be of the same form as the list returned by 
@code{ifactors(totient(n))}.

@example
(%i1) (p : 2^142 + 217, primep(p));
(%o1)                             true
(%i2) ifs : ifactors( totient(p) )$
(%i3) sublist(makelist(i,i,1,50), lambda([x], zn_primroot_p(x, p, ifs)));
(%o3)      [3, 12, 13, 15, 21, 24, 26, 27, 29, 33, 38, 42, 48]
(%i4) [time(%o2), time(%o3)];
(%o4)                   [[7.748484], [0.036002]]
@end example

@c @opencatbox
@c @category{Predicate functions}
@c @category{Number theory}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{zn_primroot_pretest}
@c @defvr {Option variable} zn_primroot_pretest
m4_defvr({Option variable}, zn_primroot_pretest)
Default value: @code{false} 

The multiplicative group (Z/@var{n}Z)* is cyclic if @var{n} is equal to 
@code{2}, @code{4}, @code{p^k} or @code{2*p^k}, where @code{p} is prime and 
greater than @code{2} and @code{k} is a natural number.  

@code{zn_primroot_pretest} controls whether @mref{zn_primroot} will check 
if one of these cases occur before it computes the smallest primitive root. 
Only if @code{zn_primroot_pretest} is set to @code{true} this pretest will be 
performed.

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@c -----------------------------------------------------------------------------
m4_setcat(Number theory)
@anchor{zn_primroot_verbose}
@c @defvr {Option variable} zn_primroot_verbose
m4_defvr({Option variable}, zn_primroot_verbose)
Default value: @code{false} 

Controls whether @mref{zn_primroot} prints a message when reaching 
@mrefdot{zn_primroot_limit}

@c @opencatbox
@c @category{Number theory}
@c @closecatbox
@c @end defvr
m4_end_defvr()

