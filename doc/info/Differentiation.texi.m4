@c -*- Mode: texinfo -*-
@menu
* Functions and Variables for Differentiation::  
@end menu

@c -----------------------------------------------------------------------------
@node Functions and Variables for Differentiation,  , Differentiation, Differentiation
@section Functions and Variables for Differentiation
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
m4_setcat(Integral calculus)
@anchor{antid}
@c @deffn {Function} antid (@var{expr}, @var{x}, @var{u(x)}) 
m4_deffn({Function}, antid, <<<(@var{expr}, @var{x}, @var{u(x)}) >>>)

@c TODO: The antid package is currently undocumented except of this function.
Returns a two-element list, such that an antiderivative of @var{expr} with
respect to @var{x} can be constructed from the list.  The expression @var{expr}
may contain an unknown function @var{u} and its derivatives.

Let @var{L}, a list of two elements, be the return value of @code{antid}.
Then @code{@var{L}[1] + 'integrate (@var{L}[2], @var{x})}
is an antiderivative of @var{expr} with respect to @var{x}.

When @code{antid} succeeds entirely,
the second element of the return value is zero.
Otherwise, the second element is nonzero,
and the first element is nonzero or zero.
If @code{antid} cannot make any progress,
the first element is zero and the second nonzero.

@code{load ("antid")} loads this function.  The @code{antid} package also
defines the functions @code{nonzeroandfreeof} and @code{linear}.

@code{antid} is related to @mref{antidiff} as follows.
Let @var{L}, a list of two elements, be the return value of @code{antid}.
Then the return value of @code{antidiff} is equal to
@code{@var{L}[1] + 'integrate (@var{L}[2], @var{x})} where @var{x} is the
variable of integration.

Examples:

@c ===beg===
@c load ("antid")$
@c expr: exp (z(x)) * diff (z(x), x) * y(x);
@c a1: antid (expr, x, z(x));
@c a2: antidiff (expr, x, z(x));
@c a2 - (first (a1) + 'integrate (second (a1), x));
@c antid (expr, x, y(x));
@c antidiff (expr, x, y(x));
@c ===end===
@example
(%i1) load ("antid")$
(%i2) expr: exp (z(x)) * diff (z(x), x) * y(x);
                            z(x)  d
(%o2)                y(x) %e     (-- (z(x)))
                                  dx
(%i3) a1: antid (expr, x, z(x));
                       z(x)      z(x)  d
(%o3)          [y(x) %e    , - %e     (-- (y(x)))]
                                       dx
(%i4) a2: antidiff (expr, x, z(x));
                            /
                     z(x)   [   z(x)  d
(%o4)         y(x) %e     - I %e     (-- (y(x))) dx
                            ]         dx
                            /
(%i5) a2 - (first (a1) + 'integrate (second (a1), x));
(%o5)                           0
(%i6) antid (expr, x, y(x));
                             z(x)  d
(%o6)             [0, y(x) %e     (-- (z(x)))]
                                   dx
(%i7) antidiff (expr, x, y(x));
                  /
                  [        z(x)  d
(%o7)             I y(x) %e     (-- (z(x))) dx
                  ]              dx
                  /
@end example

@c @opencatbox
@c @category{Integral calculus}
@c @closecatbox
@c @end deffn
m4_end_deffn()
@c -----------------------------------------------------------------------------
@anchor{antidiff}
@c @deffn {Function} antidiff (@var{expr}, @var{x}, @var{u}(@var{x}))
m4_deffn({Function}, antidiff, <<<(@var{expr}, @var{x}, @var{u}(@var{x}))>>>)

Returns an antiderivative of @var{expr} with respect to @var{x}.
The expression @var{expr} may contain an unknown function @var{u} and its
derivatives.

When @code{antidiff} succeeds entirely, the resulting expression is free of
integral signs (that is, free of the @code{integrate} noun).
Otherwise, @code{antidiff} returns an expression
which is partly or entirely within an integral sign.
If @code{antidiff} cannot make any progress,
the return value is entirely within an integral sign.

@code{load ("antid")} loads this function.
The @code{antid} package also defines the functions @code{nonzeroandfreeof} and
@code{linear}.

@code{antidiff} is related to @code{antid} as follows.
Let @var{L}, a list of two elements, be the return value of @code{antid}.
Then the return value of @code{antidiff} is equal to
@code{@var{L}[1] + 'integrate (@var{L}[2], @var{x})} where @var{x} is the
variable of integration.

Examples:

@c THERE IS A DEMO FILE share/integration/antid.dem, EXECUTED BY demo('antid)
@c BUT I THINK THE FOLLOWING ILLUSTRATES THE BASIC FUNCTIONALITY MORE CLEARLY
@c MAYBE MERGE IN THE DEMO PROBLEMS LATER
@c ===beg===
@c load ("antid")$
@c expr: exp (z(x)) * diff (z(x), x) * y(x);
@c a1: antid (expr, x, z(x));
@c a2: antidiff (expr, x, z(x));
@c a2 - (first (a1) + 'integrate (second (a1), x));
@c antid (expr, x, y(x));
@c antidiff (expr, x, y(x));
@c ===end===
@example
(%i1) load ("antid")$
(%i2) expr: exp (z(x)) * diff (z(x), x) * y(x);
                            z(x)  d
(%o2)                y(x) %e     (-- (z(x)))
                                  dx
(%i3) a1: antid (expr, x, z(x));
                       z(x)      z(x)  d
(%o3)          [y(x) %e    , - %e     (-- (y(x)))]
                                       dx
(%i4) a2: antidiff (expr, x, z(x));
                            /
                     z(x)   [   z(x)  d
(%o4)         y(x) %e     - I %e     (-- (y(x))) dx
                            ]         dx
                            /
(%i5) a2 - (first (a1) + 'integrate (second (a1), x));
(%o5)                           0
(%i6) antid (expr, x, y(x));
                             z(x)  d
(%o6)             [0, y(x) %e     (-- (z(x)))]
                                   dx
(%i7) antidiff (expr, x, y(x));
                  /
                  [        z(x)  d
(%o7)             I y(x) %e     (-- (z(x))) dx
                  ]              dx
                  /
@end example

@c @opencatbox
@c @category{Integral calculus}
@c @closecatbox
@c @end deffn
m4_end_deffn()
@c -----------------------------------------------------------------------------
@anchor{at}
@deffn  {Function} at @
@fname{at} (@var{expr}, [@var{eqn_1}, @dots{}, @var{eqn_n}]) @
@fname{at} (@var{expr}, @var{eqn})

Evaluates the expression @var{expr} with the variables assuming the values as
specified for them in the list of equations @code{[@var{eqn_1}, ...,
@var{eqn_n}]} or the single equation @var{eqn}.

If a subexpression depends on any of the variables for which a value is
specified but there is no @code{atvalue} specified and it can't be otherwise
evaluated, then a noun form of the @code{at} is returned which displays in a
two-dimensional form.

@code{at} carries out multiple substitutions in parallel.

See also @mrefdot{atvalue}  For other functions which carry out substitutions,
see also @mref{subst} and @mrefdot{ev}

Examples:

@c ===beg===
@c atvalue (f(x,y), [x = 0, y = 1], a^2);
@c atvalue ('diff (f(x,y), x), x = 0, 1 + y);
@c printprops (all, atvalue);
@c diff (4*f(x, y)^2 - u(x, y)^2, x);
@c at (%, [x = 0, y = 1]);
@c ===end===
@example
@group
(%i1) atvalue (f(x,y), [x = 0, y = 1], a^2);
                                2
(%o1)                          a
@end group
@group
(%i2) atvalue ('diff (f(x,y), x), x = 0, 1 + y);
(%o2)                        @@2 + 1
@end group
(%i3) printprops (all, atvalue);
                                !
                  d             !
                 --- (f(@@1, @@2))!       = @@2 + 1
                 d@@1            !
                                !@@1 = 0

                                     2
                          f(0, 1) = a

(%o3)                         done
@group
(%i4) diff (4*f(x, y)^2 - u(x, y)^2, x);
                  d                          d
(%o4)  8 f(x, y) (-- (f(x, y))) - 2 u(x, y) (-- (u(x, y)))
                  dx                         dx
@end group
@group
(%i5) at (%, [x = 0, y = 1]);
                                            !
                 2              d           !
(%o5)        16 a  - 2 u(0, 1) (-- (u(x, 1))!     )
                                dx          !
                                            !x = 0
@end group
@end example

Note that in the last line @code{y} is treated differently to @code{x}
as @code{y} isn't used as a differentiation variable.

The difference between @mrefcomma{subst} @mref{at} and @mref{ev} can be
seen in the following example:

@c ===beg===
@c e1:I(t)=C*diff(U(t),t)$
@c e2:U(t)=L*diff(I(t),t)$
@c at(e1,e2);
@c subst(e2,e1);
@c ev(e1,e2,diff);
@c ===end===
@example
(%i1) e1:I(t)=C*diff(U(t),t)$
(%i2) e2:U(t)=L*diff(I(t),t)$
@group
(%i3) at(e1,e2);
                               !
                      d        !
(%o3)       I(t) = C (-- (U(t))!                    )
                      dt       !          d
                               !U(t) = L (-- (I(t)))
                                          dt
@end group
@group
(%i4) subst(e2,e1);
                            d      d
(%o4)             I(t) = C (-- (L (-- (I(t)))))
                            dt     dt
@end group
@group
(%i5) ev(e1,e2,diff);
                                  2
                                 d
(%o5)                I(t) = C L (--- (I(t)))
                                   2
                                 dt
@end group
@end example


@opencatbox
@category{Evaluation}
@category{Differential equations}
@closecatbox
@end deffn

@c I SUSPECT THERE IS MORE TO BE SAID HERE
@c
@c Me, too: Where is desolve, for example?

@c -----------------------------------------------------------------------------
m4_setcat(Differential calculus)
@anchor{atomgrad}
@c @defvr {Property} atomgrad
m4_defvr({Property}, atomgrad)

@code{atomgrad} is the atomic gradient property of an expression.
This property is assigned by @code{gradef}.

@c NEED EXAMPLE HERE
@c @opencatbox
@c @category{Differential calculus}
@c @closecatbox
@c @end defvr
m4_end_defvr()
@c -----------------------------------------------------------------------------
@anchor{atvalue}
@deffn  {Function} atvalue @
@fname{atvalue} (@var{expr}, [@var{x_1} = @var{a_1}, @dots{}, @var{x_m} = @var{a_m}], @var{c}) @
@fname{atvalue} (@var{expr}, @var{x_1} = @var{a_1}, @var{c})

Assigns the value @var{c} to @var{expr} at the point @code{@var{x} = @var{a}}.
Typically boundary values are established by this mechanism.

@var{expr} is a function evaluation, @code{@var{f}(@var{x_1}, ..., @var{x_m})},
or a derivative, @code{diff (@var{f}(@var{x_1}, ..., @var{x_m}), @var{x_1},
@var{n_1}, ..., @var{x_n}, @var{n_m})}
@c HMM, WHAT IS THIS NEXT PHRASE GETTING AT ??
@c DOES IT INTEND TO IMPLY THAT IMPLICIT DEPENDENCIES ARE IGNORED ??
in which the function arguments explicitly appear.
@var{n_i} is the order of differentiation with respect to @var{x_i}.

The point at which the atvalue is established is given by the list of equations
@code{[@var{x_1} = @var{a_1}, ..., @var{x_m} = @var{a_m}]}.
If there is a single variable @var{x_1},
the sole equation may be given without enclosing it in a list.

@code{printprops ([@var{f_1}, @var{f_2}, ...], atvalue)} displays the atvalues
of the functions @code{@var{f_1}, @var{f_2}, ...} as specified by calls to
@code{atvalue}.  @code{printprops (@var{f}, atvalue)} displays the atvalues of
one function @var{f}.  @code{printprops (all, atvalue)} displays the atvalues
of all functions for which atvalues are defined.

The symbols @code{@@1}, @code{@@2}, @dots{} represent the
variables @var{x_1}, @var{x_2}, @dots{} when atvalues are displayed.

@code{atvalue} evaluates its arguments.
@code{atvalue} returns @var{c}, the atvalue.

See also @mrefdot{at}

Examples:

@c ===beg===
@c atvalue (f(x,y), [x = 0, y = 1], a^2);
@c atvalue ('diff (f(x,y), x), x = 0, 1 + y);
@c printprops (all, atvalue);
@c diff (4*f(x,y)^2 - u(x,y)^2, x);
@c at (%, [x = 0, y = 1]);
@c ===end===
@example
@group
(%i1) atvalue (f(x,y), [x = 0, y = 1], a^2);
                                2
(%o1)                          a
@end group
@group
(%i2) atvalue ('diff (f(x,y), x), x = 0, 1 + y);
(%o2)                        @@2 + 1
@end group
(%i3) printprops (all, atvalue);
                                !
                  d             !
                 --- (f(@@1, @@2))!       = @@2 + 1
                 d@@1            !
                                !@@1 = 0

                                     2
                          f(0, 1) = a

(%o3)                         done
@group
(%i4) diff (4*f(x,y)^2 - u(x,y)^2, x);
                  d                          d
(%o4)  8 f(x, y) (-- (f(x, y))) - 2 u(x, y) (-- (u(x, y)))
                  dx                         dx
@end group
@group
(%i5) at (%, [x = 0, y = 1]);
                                            !
                 2              d           !
(%o5)        16 a  - 2 u(0, 1) (-- (u(x, 1))!     )
                                dx          !
                                            !x = 0
@end group
@end example

@opencatbox
@category{Differential equations}
@category{Declarations and inferences}
@closecatbox
@end deffn

@c LOOKS LIKE cartan IS THE NAME OF A PACKAGE AND NOT A FUNCTION OR VARIABLE
@c PROBABLY SHOULD SPLIT OUT cartan AND ITS CONTENTS INTO ITS OWN TEXINFO FILE
@c ext_diff AND lie_diff NOT DOCUMENTED (OTHER THAN HERE)

@c -----------------------------------------------------------------------------
@anchor{cartan}
@deffn {Function} cartan

The exterior calculus of differential forms is a basic tool
of differential geometry developed by Elie Cartan and has important
applications in the theory of partial differential equations.
The @code{cartan} package
implements the functions @code{ext_diff} and @code{lie_diff},
along with the operators @code{~} (wedge product) and @code{|} (contraction
of a form with a vector.)
Type @code{demo ("tensor")} to see a brief
description of these commands along with examples.

@code{cartan} was implemented by F.B. Estabrook and H.D. Wahlquist.

@opencatbox
@category{Differential geometry}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
m4_setcat(Differential calculus)
@anchor{del}
@c @deffn {Function} del (@var{x})
m4_deffn({Function}, del, <<<(@var{x})>>>)

@code{del (@var{x})} represents the differential of the variable @math{x}.

@code{diff} returns an expression containing @code{del}
if an independent variable is not specified.
In this case, the return value is the so-called "total differential".

Examples:

@c ===beg===
@c diff (log (x));
@c diff (exp (x*y));
@c diff (x*y*z);
@c ===end===
@example
(%i1) diff (log (x));
                             del(x)
(%o1)                        ------
                               x
(%i2) diff (exp (x*y));
                     x y              x y
(%o2)            x %e    del(y) + y %e    del(x)
(%i3) diff (x*y*z);
(%o3)         x y del(z) + x z del(y) + y z del(x)
@end example

@c @opencatbox
@c @category{Differential calculus}
@c @closecatbox
@c @end deffn
m4_end_deffn()
@c -----------------------------------------------------------------------------
m4_setcat(Mathematical functions, Laplace transform)
@anchor{delta}
@c @deffn {Function} delta (@var{t})
m4_deffn({Function}, delta, <<<(@var{t})>>>)

The Dirac Delta function.

Currently only @mref{laplace} knows about the @code{delta} function.

Example:

@c ===beg===
@c laplace (delta (t - a) * sin(b*t), t, s);
@c input:p;
@c ===end===
@example
(%i1) laplace (delta (t - a) * sin(b*t), t, s);
Is  a  positive, negative, or zero?

p;
                                   - a s
(%o1)                   sin(a b) %e
@end example

@c @opencatbox
@c @category{Mathematical functions}
@c @category{Laplace transform}
@c @closecatbox
@c @end deffn
m4_end_deffn()
@c -----------------------------------------------------------------------------
m4_setcat(Declarations and inferences, Global variables)
@anchor{dependencies}
@c @defvr {System variable} dependencies
m4_defvr({System variable}, dependencies)
@defvrx {Function} dependencies (@var{f_1}, @dots{}, @var{f_n})

The variable @code{dependencies} is the list of atoms which have functional
dependencies, assigned by @mref{depends}, the function @code{dependencies}, or @mref{gradef}.
The @code{dependencies} list is cumulative:
each call to @code{depends}, @code{dependencies}, or @code{gradef} appends additional items.
The default value of @code{dependencies} is @code{[]}.

The function @code{dependencies(@var{f_1}, @dots{}, @var{f_n})} appends @var{f_1}, @dots{}, @var{f_n},
to the @code{dependencies} list,
where @var{f_1}, @dots{}, @var{f_n} are expressions of the form @code{@var{f}(@var{x_1}, @dots{}, @var{x_m})},
and @var{x_1}, @dots{}, @var{x_m} are any number of arguments.

@code{dependencies(@var{f}(@var{x_1}, @dots{}, @var{x_m}))} is equivalent to @code{depends(@var{f}, [@var{x_1}, @dots{}, @var{x_m}])}.

See also @mref{depends} and @mrefdot{gradef}

@c ===beg===
@c dependencies;
@c depends (foo, [bar, baz]);
@c depends ([g, h], [a, b, c]);
@c dependencies;
@c dependencies (quux (x, y), mumble (u));
@c dependencies;
@c remove (quux, dependency);
@c dependencies;
@c ===end===
@example
@group
(%i1) dependencies;
(%o1)                          []
@end group
@group
(%i2) depends (foo, [bar, baz]);
(%o2)                    [foo(bar, baz)]
@end group
@group
(%i3) depends ([g, h], [a, b, c]);
(%o3)               [g(a, b, c), h(a, b, c)]
@end group
@group
(%i4) dependencies;
(%o4)        [foo(bar, baz), g(a, b, c), h(a, b, c)]
@end group
@group
(%i5) dependencies (quux (x, y), mumble (u));
(%o5)                [quux(x, y), mumble(u)]
@end group
@group
(%i6) dependencies;
(%o6) [foo(bar, baz), g(a, b, c), h(a, b, c), quux(x, y), 
                                                       mumble(u)]
@end group
@group
(%i7) remove (quux, dependency);
(%o7)                         done
@end group
@group
(%i8) dependencies;
(%o8)  [foo(bar, baz), g(a, b, c), h(a, b, c), mumble(u)]
@end group
@end example

@c @opencatbox
@c @category{Declarations and inferences}
@c @category{Global variables}
@c @closecatbox
@c @end defvr
m4_end_defvr()
@c -----------------------------------------------------------------------------
m4_setcat(Differential calculus, Declarations and inferences)
@anchor{depends}
@c @deffn {Function} depends (@var{f_1}, @var{x_1}, @dots{}, @var{f_n}, @var{x_n})
m4_deffn({Function}, depends, <<<(@var{f_1}, @var{x_1}, @dots{}, @var{f_n}, @var{x_n})>>>)

Declares functional dependencies among variables for the purpose of computing
derivatives.  In the absence of declared dependence, @code{diff (f, x)} yields
zero.  If @code{depends (f, x)} is declared, @code{diff (f, x)} yields a
symbolic derivative (that is, a @code{diff} noun).

Each argument @var{f_1}, @var{x_1}, etc., can be the name of a variable or
array, or a list of names.
Every element of @var{f_i} (perhaps just a single element)
is declared to depend
on every element of @var{x_i} (perhaps just a single element).
If some @var{f_i} is the name of an array or contains the name of an array,
all elements of the array depend on @var{x_i}.

@code{diff} recognizes indirect dependencies established by @code{depends}
and applies the chain rule in these cases.

@code{remove (@var{f}, dependency)} removes all dependencies declared for
@var{f}.

@code{depends} returns a list of the dependencies established.
The dependencies are appended to the global variable @mref{dependencies}.
@code{depends} evaluates its arguments.

@code{diff} is the only Maxima command which recognizes dependencies established
by @code{depends}.  Other functions (@code{integrate}, @code{laplace}, etc.)
only recognize dependencies explicitly represented by their arguments.
For example, @mref{integrate} does not recognize the dependence of @code{f} on
@code{x} unless explicitly represented as @code{integrate (f(x), x)}.

@code{depends(@var{f}, [@var{x_1}, @dots{}, @var{x_n}])} is equivalent to @code{dependencies(@var{f}(@var{x_1}, @dots{}, @var{x_n}))}.

@c ===beg===
@c depends ([f, g], x);
@c depends ([r, s], [u, v, w]);
@c depends (u, t);
@c dependencies;
@c diff (r.s, u);
@c ===end===
@example
(%i1) depends ([f, g], x);
(%o1)                     [f(x), g(x)]
(%i2) depends ([r, s], [u, v, w]);
(%o2)               [r(u, v, w), s(u, v, w)]
(%i3) depends (u, t);
(%o3)                        [u(t)]
(%i4) dependencies;
(%o4)      [f(x), g(x), r(u, v, w), s(u, v, w), u(t)]
(%i5) diff (r.s, u);
                         dr           ds
(%o5)                    -- . s + r . --
                         du           du
@end example

@c ===beg===
@c diff (r.s, t);
@c ===end===
@example
(%i6) diff (r.s, t);
                      dr du           ds du
(%o6)                 -- -- . s + r . -- --
                      du dt           du dt
@end example

@c ===beg===
@c remove (r, dependency);
@c diff (r.s, t);
@c ===end===
@example
(%i7) remove (r, dependency);
(%o7)                         done
(%i8) diff (r.s, t);
                                ds du
(%o8)                       r . -- --
                                du dt
@end example

@c @opencatbox
@c @category{Differential calculus}
@c @category{Declarations and inferences}
@c @closecatbox
@c @end deffn
m4_end_deffn()
@c -----------------------------------------------------------------------------
m4_setcat(Differential calculus, Global flags)
@anchor{derivabrev}
@c @defvr {Option variable} derivabbrev
m4_defvr({Option variable}, derivabbrev)
Default value: @code{false}

When @code{derivabbrev} is @code{true},
symbolic derivatives (that is, @code{diff} nouns) are displayed as subscripts.
Otherwise, derivatives are displayed in the Leibniz notation @code{dy/dx}.

@c NEED EXAMPLES HERE
@c @opencatbox
@c @category{Differential calculus}
@c @category{Global flags}
@c @closecatbox
@c @end defvr
m4_end_defvr()
@c SEEMS LIKE THIS STATEMENT COULD BE LESS CLUMSY

@c -----------------------------------------------------------------------------
m4_setcat(Differential calculus, Expressions)
@anchor{derivdegree}
@c @deffn {Function} derivdegree (@var{expr}, @var{y}, @var{x})
m4_deffn({Function}, derivdegree, <<<(@var{expr}, @var{y}, @var{x})>>>)

Returns the highest degree of the derivative
of the dependent variable @var{y} with respect to the independent variable
@var{x} occurring in @var{expr}.

Example:

@c ===beg===
@c 'diff (y, x, 2) + 'diff (y, z, 3) + 'diff (y, x) * x^2;
@c derivdegree (%, y, x);
@c ===end===
@example
(%i1) 'diff (y, x, 2) + 'diff (y, z, 3) + 'diff (y, x) * x^2;
                         3     2
                        d y   d y    2 dy
(%o1)                   --- + --- + x  --
                          3     2      dx
                        dz    dx
(%i2) derivdegree (%, y, x);
(%o2)                           2
@end example

@c @opencatbox
@c @category{Differential calculus}
@c @category{Expressions}
@c @closecatbox
@c @end deffn
m4_end_deffn()
@c I HAVE NO IDEA WHAT THIS DOES

@c -----------------------------------------------------------------------------
m4_setcat(Differential calculus, Evaluation)
@anchor{derivlist}
@c @deffn {Function} derivlist (@var{var_1}, @dots{}, @var{var_k})
m4_deffn({Function}, derivlist, <<<(@var{var_1}, @dots{}, @var{var_k})>>>)

Causes only differentiations with respect to
the indicated variables, within the @mref{ev} command.

@c @opencatbox
@c @category{Differential calculus}
@c @category{Evaluation}
@c @closecatbox
@c @end deffn
m4_end_deffn()
@c -----------------------------------------------------------------------------
m4_setcat(Differential calculus, Expressions)
@anchor{derivsubst}
@c @defvr {Option variable} derivsubst
m4_defvr({Option variable}, derivsubst)
Default value: @code{false}

When @code{derivsubst} is @code{true}, a non-syntactic substitution such as
@code{subst (x, 'diff (y, t), 'diff (y, t, 2))} yields @code{'diff (x, t)}.

@c @opencatbox
@c @category{Differential calculus}
@c @category{Expressions}
@c @closecatbox
@c @end defvr
m4_end_defvr()
@c -----------------------------------------------------------------------------
@anchor{diff}
@deffn  {Function} diff @
@fname{diff} (@var{expr}, @var{x_1}, @var{n_1}, @dots{}, @var{x_m}, @var{n_m}) @
@fname{diff} (@var{expr}, @var{x}, @var{n}) @
@fname{diff} (@var{expr}, @var{x}) @
@fname{diff} (@var{expr})

Returns the derivative or differential of @var{expr} with respect to some or
all variables in @var{expr}.

@code{diff (@var{expr}, @var{x}, @var{n})} returns the @var{n}'th derivative of
@var{expr} with respect to @var{x}.

@code{diff (@var{expr}, @var{x_1}, @var{n_1}, ..., @var{x_m}, @var{n_m})}
returns the mixed partial derivative of @var{expr} with respect to @var{x_1}, 
@dots{}, @var{x_m}.  It is equivalent to @code{diff (... (diff (@var{expr},
@var{x_m}, @var{n_m}) ...), @var{x_1}, @var{n_1})}.

@code{diff (@var{expr}, @var{x})}
returns the first derivative of @var{expr} with respect to
the variable @var{x}.

@code{diff (@var{expr})} returns the total differential of @var{expr}, that is,
the sum of the derivatives of @var{expr} with respect to each its variables
times the differential @code{del} of each variable.
@c WHAT DOES THIS NEXT STATEMENT MEAN, EXACTLY ??
No further simplification of @code{del} is offered.

The noun form of @code{diff} is required in some contexts,
such as stating a differential equation.
In these cases, @code{diff} may be quoted (as @code{'diff}) to yield the noun
form instead of carrying out the differentiation.

When @code{derivabbrev} is @code{true}, derivatives are displayed as subscripts.
Otherwise, derivatives are displayed in the Leibniz notation, @code{dy/dx}.

Examples:

@c ===beg===
@c diff (exp (f(x)), x, 2);
@c derivabbrev: true$
@c 'integrate (f(x, y), y, g(x), h(x));
@c diff (%, x);
@c ===end===
@example
(%i1) diff (exp (f(x)), x, 2);
                     2
              f(x)  d               f(x)  d         2
(%o1)       %e     (--- (f(x))) + %e     (-- (f(x)))
                      2                   dx
                    dx
(%i2) derivabbrev: true$
(%i3) 'integrate (f(x, y), y, g(x), h(x));
                         h(x)
                        /
                        [
(%o3)                   I     f(x, y) dy
                        ]
                        /
                         g(x)
(%i4) diff (%, x);
       h(x)
      /
      [
(%o4) I     f(x, y)  dy + f(x, h(x)) h(x)  - f(x, g(x)) g(x)
      ]            x                     x                  x
      /
       g(x)
@end example

For the tensor package, the following modifications have been
incorporated:

(1) The derivatives of any indexed objects in @var{expr} will have the
variables @var{x_i} appended as additional arguments.  Then all the
derivative indices will be sorted.

(2) The @var{x_i} may be integers from 1 up to the value of the variable
@code{dimension} [default value: 4].  This will cause the differentiation to be
carried out with respect to the @var{x_i}'th member of the list
@code{coordinates} which should be set to a list of the names of the
coordinates, e.g., @code{[x, y, z, t]}. If @code{coordinates} is bound to an
atomic variable, then that variable subscripted by @var{x_i} will be used for
the variable of differentiation.  This permits an array of coordinate names or
subscripted names like @code{X[1]}, @code{X[2]}, @dots{} to be used.  If
@code{coordinates} has not been assigned a value, then the variables will be
treated as in (1) above.

@c NEED EXAMPLES FOR TENSOR STUFF
@opencatbox
@category{Differential calculus}
@closecatbox
@end deffn

@c MERGE THIS INTO @defun diff

@c -----------------------------------------------------------------------------
m4_setcat()
@anchor{symbol_diff}
@c @defvr {Special symbol} diff
m4_defvr({Special symbol}, diff)

When @code{diff} is present as an @code{evflag} in call to @code{ev},
all differentiations indicated in @code{expr} are carried out.

@c NEED EXAMPLE HERE
@end defvr

@c NOT SURE HOW THIS IS SUPPOSED TO WORK

@c -----------------------------------------------------------------------------
m4_setcat(Differential calculus, Package ctensor)
@anchor{dscalar}
@c @deffn {Function} dscalar (@var{f})
m4_deffn({Function}, dscalar, <<<(@var{f})>>>)

Applies the scalar d'Alembertian to the scalar function @var{f}.

@c APPARENTLY dscalar DOESN'T EXIST IN THE CORE FILES ANYMORE
@c ctensor HAS THE ONLY DEFN I FOUND (OUTSIDE OF archive/)
@code{load ("ctensor")} loads this function.

@c FOLLOWING EXAMPLE DOESN'T WORK; I GET dscalar (field) ==> 0
@c (I GET 0 FOR THE ctensor VERSION OF dscalar, AND SAME FOR
@c THE DEFN OF dscalar GIVEN IN archive/share/lisp/ctensr.trl)
@c INCIDENTALLY dependencies IS DOCUMENTED ONLY AS A VARIABLE

@c @example
@c (%i41) dependencies(field(r));
@c (%o41)                           [field(r)]
@c (%i42) dscalar(field);
@c (%o43)
@c     -m
@c   %e  ((field  n - field  m + 2 field   ) r + 4 field )
@c              r  r       r  r         r r             r
@c 
@c - -----------------------------------------------------
@c                              2 r
@c @end example

@c @opencatbox
@c @category{Differential calculus}
@c @category{Package ctensor}
@c @closecatbox
@c @end deffn
m4_end_deffn()
@c -----------------------------------------------------------------------------
m4_setcat(Differential calculus, Vectors, Operators)
@anchor{express}
@c @deffn {Function} express (@var{expr})
m4_deffn({Function}, express, <<<(@var{expr})>>>)

@c HERE IS THE PREVIOUS TEXT. WHAT IS THE POINT ABOUT depends ?? I'M NOT GETTING IT
@c The result uses the noun form of any
@c derivatives arising from expansion of the vector differential
@c operators.  To force evaluation of these derivatives, the built-in @code{ev}
@c function can be used together with the @code{diff} evflag, after using the
@c built-in @code{depends} function to establish any new implicit dependencies.
@c
@c TODO: curl, grad, div and laplacian aren't currently documented.

Expands differential operator nouns into expressions in terms of partial
derivatives.  @code{express} recognizes the operators @code{grad}, @code{div},
@code{curl}, @code{laplacian}.  @code{express} also expands the cross product
@mref{~}.

Symbolic derivatives (that is, @code{diff} nouns)
in the return value of express may be evaluated by including @code{diff}
in the @code{ev} function call or command line.
In this context, @mref{diff} acts as an @mref{evfun}.

@code{load ("vect")} loads this function.
@c IN POINT OF FACT, express IS A SIMPLIFICATION RULE, AND express1 IS THE FCN WHICH DOES ALL THE WORK

Examples:

@c ===beg===
@c load ("vect")$
@c grad (x^2 + y^2 + z^2);
@c express (%);
@c ev (%, diff);
@c div ([x^2, y^2, z^2]);
@c express (%);
@c ev (%, diff);
@c curl ([x^2, y^2, z^2]);
@c express (%);
@c ev (%, diff);
@c laplacian (x^2 * y^2 * z^2);
@c express (%);
@c ev (%, diff);
@c [a, b, c] ~ [x, y, z];
@c express (%);
@c ===end===
@example
(%i1) load ("vect")$
(%i2) grad (x^2 + y^2 + z^2);
                              2    2    2
(%o2)                  grad (z  + y  + x )
(%i3) express (%);
       d    2    2    2   d    2    2    2   d    2    2    2
(%o3) [-- (z  + y  + x ), -- (z  + y  + x ), -- (z  + y  + x )]
       dx                 dy                 dz
(%i4) ev (%, diff);
(%o4)                    [2 x, 2 y, 2 z]
(%i5) div ([x^2, y^2, z^2]);
                              2   2   2
(%o5)                   div [x , y , z ]
(%i6) express (%);
                   d    2    d    2    d    2
(%o6)              -- (z ) + -- (y ) + -- (x )
                   dz        dy        dx
(%i7) ev (%, diff);
(%o7)                    2 z + 2 y + 2 x
(%i8) curl ([x^2, y^2, z^2]);
                               2   2   2
(%o8)                   curl [x , y , z ]
(%i9) express (%);
       d    2    d    2   d    2    d    2   d    2    d    2
(%o9) [-- (z ) - -- (y ), -- (x ) - -- (z ), -- (y ) - -- (x )]
       dy        dz       dz        dx       dx        dy
(%i10) ev (%, diff);
(%o10)                      [0, 0, 0]
(%i11) laplacian (x^2 * y^2 * z^2);
                                  2  2  2
(%o11)                laplacian (x  y  z )
(%i12) express (%);
         2                2                2
        d     2  2  2    d     2  2  2    d     2  2  2
(%o12)  --- (x  y  z ) + --- (x  y  z ) + --- (x  y  z )
          2                2                2
        dz               dy               dx
(%i13) ev (%, diff);
                      2  2      2  2      2  2
(%o13)             2 y  z  + 2 x  z  + 2 x  y
(%i14) [a, b, c] ~ [x, y, z];
(%o14)                [a, b, c] ~ [x, y, z]
(%i15) express (%);
(%o15)          [b z - c y, c x - a z, a y - b x]
@end example

@c @opencatbox
@c @category{Differential calculus}
@c @category{Vectors}
@c @category{Operators}
@c @closecatbox
@c @end deffn
m4_end_deffn()
@c COMMENTING OUT THIS TEXT PENDING RESOLUTION OF BUG REPORT # 836704:
@c "gendiff is all bugs: should be deprecated"
@c @defun gendiff
@c Sometimes @code{diff(e,x,n)} can be reduced even though N is
@c symbolic.
@c 
@c @example
@c batch("gendif")$
@c @end example
@c 
@c and you can try, for example,
@c 
@c @example
@c diff(%e^(a*x),x,q)
@c @end example
@c 
@c by using @code{gendiff} rather than @code{diff}.  Unevaluable
@c items come out quoted.  Some items are in terms of @code{genfact}, which
@c see.
@c 
@c @end defun

@c -----------------------------------------------------------------------------
@anchor{gradef}
@deffn  {Function} gradef @
@fname{gradef} (@var{f}(@var{x_1}, @dots{}, @var{x_n}), @var{g_1}, @dots{}, @var{g_m}) @
@fname{gradef} (@var{a}, @var{x}, @var{expr})

Defines the partial derivatives (i.e., the components of the gradient) of the
function @var{f} or variable @var{a}.

@code{gradef (@var{f}(@var{x_1}, ..., @var{x_n}), @var{g_1}, ..., @var{g_m})}
defines @code{d@var{f}/d@var{x_i}} as @var{g_i}, where @var{g_i} is an
expression; @var{g_i} may be a function call, but not the name of a function.
The number of partial derivatives @var{m} may be less than the number of
arguments @var{n}, in which case derivatives are defined with respect to
@var{x_1} through @var{x_m} only.

@code{gradef (@var{a}, @var{x}, @var{expr})} defines the derivative of variable
@var{a} with respect to @var{x} as @var{expr}.  This also establishes the
dependence of @var{a} on @var{x} (via @code{depends (@var{a}, @var{x})}).

The first argument @code{@var{f}(@var{x_1}, ..., @var{x_n})} or @var{a} is
quoted, but the remaining arguments @var{g_1}, ..., @var{g_m} are evaluated.
@code{gradef} returns the function or variable for which the partial derivatives
are defined.

@code{gradef} can redefine the derivatives of Maxima's built-in functions.
For example, @code{gradef (sin(x), sqrt (1 - sin(x)^2))} redefines the
derivative of @code{sin}.

@code{gradef} cannot define partial derivatives for a subscripted function.

@code{printprops ([@var{f_1}, ..., @var{f_n}], gradef)} displays the partial
derivatives of the functions @var{f_1}, ..., @var{f_n}, as defined by
@code{gradef}.

@code{printprops ([@var{a_n}, ..., @var{a_n}], atomgrad)} displays the partial
derivatives of the variables @var{a_n}, ..., @var{a_n}, as defined by
@code{gradef}.

@code{gradefs} is the list of the functions
for which partial derivatives have been defined by @code{gradef}.
@code{gradefs} does not include any variables
for which partial derivatives have been defined by @code{gradef}.

@c REPHRASE THIS NEXT BIT
Gradients are needed when, for example, a function is not known
explicitly but its first derivatives are and it is desired to obtain
higher order derivatives.

@c NEED EXAMPLES HERE
@opencatbox
@category{Differential calculus}
@category{Declarations and inferences}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
m4_setcat(Differential calculus, Declarations and inferences)
@anchor{gradefs}
@c @defvr {System variable} gradefs
m4_defvr({System variable}, gradefs)
Default value: @code{[]}

@code{gradefs} is the list of the functions
for which partial derivatives have been defined by @code{gradef}.
@code{gradefs} does not include any variables
for which partial derivatives have been defined by @code{gradef}.

@c @opencatbox
@c @category{Differential calculus}
@c @category{Declarations and inferences}
@c @closecatbox
@c @end defvr
m4_end_defvr()
@c -----------------------------------------------------------------------------
m4_setcat(Laplace transform, Differential equations)
@anchor{laplace}
@c @deffn {Function} laplace (@var{expr}, @var{t}, @var{s})
m4_deffn({Function}, laplace, <<<(@var{expr}, @var{t}, @var{s})>>>)

Attempts to compute the Laplace transform of @var{expr} with respect to the 
variable @var{t} and transform parameter @var{s}.

@code{laplace} recognizes in @var{expr} the functions @mrefcomma{delta} @mrefcomma{exp}
@mrefcomma{log} @mrefcomma{sin} @mrefcomma{cos} @mrefcomma{sinh} @mrefcomma{cosh} and @mrefcomma{erf}
as well as @code{derivative}, @mrefcomma{integrate} @mrefcomma{sum} and @mrefdot{ilt} If
laplace fails to find a transform the function @mref{specint} is called.
@code{specint} can find the laplace transform for expressions with special
functions like the bessel functions @mrefcomma{bessel_j} @mrefcomma{bessel_i} @dots{}
and can handle the @mref{unit_step} function.  See also @mrefdot{specint}

If @code{specint} cannot find a solution too, a noun @code{laplace} is returned.

@c REPHRASE THIS
@var{expr} may also be a linear, constant coefficient differential equation in
which case @mref{atvalue} of the dependent variable is used.
@c "used" -- USED HOW ??
The required atvalue may be supplied either before or after the transform is
computed.  Since the initial conditions must be specified at zero, if one has
boundary conditions imposed elsewhere he can impose these on the general
solution and eliminate the constants by solving the general solution
for them and substituting their values back.

@code{laplace} recognizes convolution integrals of the form
@code{integrate (f(x) * g(t - x), x, 0, t)};
other kinds of convolutions are not recognized.

Functional relations must be explicitly represented in @var{expr};
implicit relations, established by @mrefcomma{depends} are not recognized.
That is, if @var{f} depends on @var{x} and @var{y},
@code{f (x, y)} must appear in @var{expr}.

See also @mrefcomma{ilt} the inverse Laplace transform.

Examples:

@c ===beg===
@c laplace (exp (2*t + a) * sin(t) * t, t, s);
@c laplace ('diff (f (x), x), x, s);
@c diff (diff (delta (t), t), t);
@c laplace (%, t, s);
@c assume(a>0)$
@c declare(a, integer)$
@c laplace(gamma_incomplete(a,t),t,s),gamma_expand:true;
@c factor(laplace(gamma_incomplete(1/2,t),t,s));
@c assume(exp(%pi*s)>1)$
@c laplace(sum((-1)^n*unit_step(t-n*%pi)*sin(t),n,0,inf),t,s),
@c    simpsum;
@c ===end===
@example
(%i1) laplace (exp (2*t + a) * sin(t) * t, t, s);
                            a
                          %e  (2 s - 4)
(%o1)                    ---------------
                           2           2
                         (s  - 4 s + 5)
(%i2) laplace ('diff (f (x), x), x, s);
(%o2)             s laplace(f(x), x, s) - f(0)
(%i3) diff (diff (delta (t), t), t);
                          2
                         d
(%o3)                    --- (delta(t))
                           2
                         dt
(%i4) laplace (%, t, s);
                            !
               d            !         2
(%o4)        - -- (delta(t))!      + s  - delta(0) s
               dt           !
                            !t = 0
(%i5) assume(a>0)$
(%i6) laplace(gamma_incomplete(a,t),t,s),gamma_expand:true;
                                              - a - 1
                         gamma(a)   gamma(a) s
(%o6)                    -------- - -----------------
                            s            1     a
                                        (- + 1)
                                         s
(%i7) factor(laplace(gamma_incomplete(1/2,t),t,s));
                                              s + 1
                      sqrt(%pi) (sqrt(s) sqrt(-----) - 1)
                                                s
(%o7)                 -----------------------------------
                                3/2      s + 1
                               s    sqrt(-----)
                                           s
(%i8) assume(exp(%pi*s)>1)$
(%i9) laplace(sum((-1)^n*unit_step(t-n*%pi)*sin(t),n,0,inf),t,s),
         simpsum;
@group
                         %i                         %i
              ------------------------ - ------------------------
                              - %pi s                    - %pi s
              (s + %i) (1 - %e       )   (s - %i) (1 - %e       )
(%o9)         ---------------------------------------------------
                                       2
@end group
(%i9) factor(%);
                                      %pi s
                                    %e
(%o9)                   -------------------------------
                                             %pi s
                        (s - %i) (s + %i) (%e      - 1)

@end example

@c @opencatbox
@c @category{Laplace transform}
@c @category{Differential equations}
@c @closecatbox
@c @end deffn
m4_end_deffn()
