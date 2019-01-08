@c -*- Mode: texinfo -*-
@menu
* Introduction to Rules and Patterns::  
* Functions and Variables for Rules and Patterns::  
@end menu

@c -----------------------------------------------------------------------------
@node Introduction to Rules and Patterns, Functions and Variables for Rules and Patterns, Rules and Patterns, Rules and Patterns
@section Introduction to Rules and Patterns
@c -----------------------------------------------------------------------------

This section describes user-defined pattern matching and simplification rules.
There are two groups of functions which implement somewhat different pattern
matching schemes.  In one group are @code{tellsimp}, @code{tellsimpafter},
@code{defmatch}, @code{defrule}, @code{apply1}, @code{applyb1}, and
@code{apply2}.  In the other group are @code{let} and @code{letsimp}.
Both schemes define patterns in terms of pattern variables declared by
@code{matchdeclare}.

Pattern-matching rules defined by @code{tellsimp} and @code{tellsimpafter} are
applied automatically by the Maxima simplifier.  Rules defined by
@code{defmatch}, @code{defrule}, and @code{let} are applied by an explicit
function call.

There are additional mechanisms for rules applied to polynomials by
@code{tellrat}, and for commutative and noncommutative algebra in @code{affine}
package.

@opencatbox
@category{Simplification} @category{Rules and patterns}
@closecatbox

@c end concepts Rules and Patterns

@c -----------------------------------------------------------------------------
@node Functions and Variables for Rules and Patterns,  , Introduction to Rules and Patterns, Rules and Patterns
@section Functions and Variables for Rules and Patterns
@c -----------------------------------------------------------------------------

@c NEEDS CLARIFICATION AND EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{apply1}
@deffn {Function} apply1 (@var{expr}, @var{rule_1}, @dots{}, @var{rule_n})

Repeatedly applies @var{rule_1} to
@var{expr} until it fails, then repeatedly applies the same rule to all
subexpressions of @var{expr}, left to right, until @var{rule_1} has failed
on all subexpressions.  Call the result of transforming @var{expr} in this
manner @var{expr_2}.  Then @var{rule_2} is applied in the same fashion
starting at the top of @var{expr_2}.  When @var{rule_n} fails on the final
subexpression, the result is returned.

@code{maxapplydepth} is the depth of the deepest subexpressions processed by
@code{apply1} and @code{apply2}.

See also @mrefcomma{applyb1} @mref{apply2} and @mrefdot{let}

@opencatbox
@category{Rules and patterns}
@closecatbox
@end deffn

@c NEEDS CLARIFICATION AND EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{apply2}
@deffn {Function} apply2 (@var{expr}, @var{rule_1}, @dots{}, @var{rule_n})

If @var{rule_1} fails on a given subexpression, then @var{rule_2} is
repeatedly applied, etc.  Only if all rules fail on a given
subexpression is the whole set of rules repeatedly applied to the next
subexpression.  If one of the rules succeeds, then the same
subexpression is reprocessed, starting with the first rule.

@code{maxapplydepth} is the depth of the deepest subexpressions processed by
@code{apply1} and @code{apply2}.

See also @mref{apply1} and @mrefdot{let}

@opencatbox
@category{Rules and patterns}
@closecatbox
@end deffn

@c NEEDS CLARIFICATION AND EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{applyb1}
@deffn {Function} applyb1 (@var{expr}, @var{rule_1}, @dots{}, @var{rule_n})

Repeatedly applies @var{rule_1} to the deepest subexpression of @var{expr}
until it fails, then repeatedly applies the same rule one level higher (i.e.,
larger subexpressions), until @var{rule_1} has failed on the top-level
expression.  Then @var{rule_2} is applied in the same fashion to the result of
@var{rule_1}.  After @var{rule_n} has been applied to the top-level expression,
the result is returned.

@code{applyb1} is similar to @code{apply1} but works from
the bottom up instead of from the top down.

@code{maxapplyheight} is the maximum height which @code{applyb1} reaches
before giving up.

See also @mrefcomma{apply1} @mref{apply2} and @mrefdot{let}

@opencatbox
@category{Rules and patterns}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{current_let_rule_package}
@defvr {Option variable} current_let_rule_package
Default value: @code{default_let_rule_package}

@code{current_let_rule_package} is the name of the rule package that is used by
functions in the @code{let} package (@code{letsimp}, etc.) @c NEED TO GIVE AN EXPLICIT LIST HERE (NOT "ETC")
if no other rule package is specified.
This variable may be assigned the name of any rule package defined
via the @code{let} command.

If a call such as @code{letsimp (expr, rule_pkg_name)} is made,
the rule package @code{rule_pkg_name} is used for that function call only,
and the value of @code{current_let_rule_package} is not changed.

@opencatbox
@category{Rules and patterns}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{default_let_rule_package}
@defvr {Option variable} default_let_rule_package
@c DEFAULT BINDING OF default_let_rule_package IS default_let_rule_package (BOUND TO ITSELF)
Default value: @code{default_let_rule_package}

@c THIS IS SORT OF CONFUSING. PROBABLY NEED TO GIVE MORE DETAIL HERE
@code{default_let_rule_package} is the name of the rule package used when one
is not explicitly set by the user with @code{let} or by changing the value of
@code{current_let_rule_package}.

@opencatbox
@category{Rules and patterns}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{defmatch}
@deffn  {Function} defmatch @
@fname{defmatch} (@var{progname}, @var{pattern}, @var{x_1}, @dots{}, @var{x_n}) @
@fname{defmatch} (@var{progname}, @var{pattern})

Defines a function @code{@var{progname}(@var{expr}, @var{x_1}, ..., @var{x_n})}
which tests @var{expr} to see if it matches @var{pattern}.

@var{pattern} is an expression containing the pattern arguments @var{x_1},
@dots{}, @var{x_n} (if any) and some pattern variables (if any).  The pattern
arguments are given explicitly as arguments to @code{defmatch} while the pattern
variables are declared by the @code{matchdeclare} function.  Any variable not
declared as a pattern variable in @code{matchdeclare} or as a pattern argument
in @code{defmatch} matches only itself.

The first argument to the created function @var{progname} is an expression to be
matched against the pattern and the other arguments are the actual arguments
which correspond to the dummy variables @var{x_1}, @dots{}, @var{x_n} in the
pattern.

If the match is successful, @var{progname} returns a list of equations whose
left sides are the pattern arguments and pattern variables, and whose right
sides are the subexpressions which the pattern arguments and variables matched.
The pattern variables, but not the pattern arguments, are assigned the
subexpressions they match.  If the match fails, @var{progname} returns
@code{false}.

A literal pattern (that is, a pattern which contains neither pattern arguments
nor pattern variables) returns @code{true} if the match succeeds.

See also @mrefcomma{matchdeclare} @mrefcomma{defrule} @mref{tellsimp} and
@mrefdot{tellsimpafter}

Examples:

Define a function @code{linearp(expr, x)} which
tests @code{expr} to see if it is of the form @code{a*x + b}
such that @code{a} and @code{b} do not contain @code{x} and @code{a} is nonzero.
This match function matches expressions which are linear in any variable,
because the pattern argument @code{x} is given to @code{defmatch}.
@c HOW HARD WILL MAXIMA TRY TO COLLECT TERMS AND DO OTHER MUNGEING TO FIT THE PATTERN ??

@c ===beg===
@c matchdeclare (a, lambda ([e], e#0 and freeof(x, e)), b, 
@c                     freeof(x));
@c defmatch (linearp, a*x + b, x);
@c linearp (3*z + (y + 1)*z + y^2, z);
@c a;
@c b;
@c x;
@c ===end===
@example
@group
(%i1) matchdeclare (a, lambda ([e], e#0 and freeof(x, e)), b,
                    freeof(x));
(%o1)                         done
@end group
@group
(%i2) defmatch (linearp, a*x + b, x);
(%o2)                        linearp
@end group
@group
(%i3) linearp (3*z + (y + 1)*z + y^2, z);
                         2
(%o3)              [b = y , a = y + 4, x = z]
@end group
@group
(%i4) a;
(%o4)                         y + 4
@end group
@group
(%i5) b;
                                2
(%o5)                          y
@end group
@group
(%i6) x;
(%o6)                           x
@end group
@end example

Define a function @code{linearp(expr)} which tests @code{expr}
to see if it is of the form @code{a*x + b}
such that @code{a} and @code{b} do not contain @code{x} and @code{a} is nonzero.
This match function only matches expressions linear in @code{x},
not any other variable, because no pattern argument is given to @code{defmatch}.

@c ===beg===
@c matchdeclare (a, lambda ([e], e#0 and freeof(x, e)), b, 
@c                     freeof(x));
@c defmatch (linearp, a*x + b);
@c linearp (3*z + (y + 1)*z + y^2);
@c linearp (3*x + (y + 1)*x + y^2);
@c ===end===
@example
@group
(%i1) matchdeclare (a, lambda ([e], e#0 and freeof(x, e)), b,
                    freeof(x));
(%o1)                         done
@end group
@group
(%i2) defmatch (linearp, a*x + b);
(%o2)                        linearp
@end group
@group
(%i3) linearp (3*z + (y + 1)*z + y^2);
(%o3)                         false
@end group
@group
(%i4) linearp (3*x + (y + 1)*x + y^2);
                             2
(%o4)                  [b = y , a = y + 4]
@end group
@end example

Define a function @code{checklimits(expr)} which tests @code{expr}
to see if it is a definite integral.

@c ===beg===
@c matchdeclare ([a, f], true);
@c constinterval (l, h) := constantp (h - l);
@c matchdeclare (b, constinterval (a));
@c matchdeclare (x, atom);
@c simp : false;
@c defmatch (checklimits, 'integrate (f, x, a, b));
@c simp : true;
@c 'integrate (sin(t), t, %pi + x, 2*%pi + x);
@c checklimits (%);
@c ===end===
@example
@group
(%i1) matchdeclare ([a, f], true);
(%o1)                         done
@end group
@group
(%i2) constinterval (l, h) := constantp (h - l);
(%o2)        constinterval(l, h) := constantp(h - l)
@end group
@group
(%i3) matchdeclare (b, constinterval (a));
(%o3)                         done
@end group
@group
(%i4) matchdeclare (x, atom);
(%o4)                         done
@end group
@group
(%i5) simp : false;
(%o5)                         false
@end group
@group
(%i6) defmatch (checklimits, 'integrate (f, x, a, b));
(%o6)                      checklimits
@end group
@group
(%i7) simp : true;
(%o7)                         true
@end group
@group
(%i8) 'integrate (sin(t), t, %pi + x, 2*%pi + x);
                       x + 2 %pi
                      /
                      [
(%o8)                 I          sin(t) dt
                      ]
                      /
                       x + %pi
@end group
@group
(%i9) checklimits (%);
(%o9)    [b = x + 2 %pi, a = x + %pi, x = t, f = sin(t)]
@end group
@end example

@opencatbox
@category{Rules and patterns}
@closecatbox
@end deffn

@c NEEDS CLARIFICATION AND EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{defrule}
@deffn {Function} defrule (@var{rulename}, @var{pattern}, @var{replacement})

Defines and names a replacement rule for the given pattern.  If the rule named
@var{rulename} is applied to an expression (by @code{apply1}, @code{applyb1}, or
@code{apply2}), every subexpression matching the pattern will be replaced by the
replacement.  All variables in the replacement which have been
assigned values by the pattern match are assigned those values in the
replacement which is then simplified.

The rules themselves can be
treated as functions which transform an expression by one
operation of the pattern match and replacement.
If the match fails, the rule function returns @code{false}.

@opencatbox
@category{Rules and patterns}
@closecatbox
@end deffn

@c NEEDS EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{disprule}
@deffn  {Function} disprule @
@fname{disprule} (@var{rulename_1}, @dots{}, @var{rulename_2}) @
@fname{disprule} (all)

Display rules with the names @var{rulename_1}, @dots{}, @var{rulename_n},
as returned by @code{defrule}, @code{tellsimp}, or @code{tellsimpafter},
or a pattern defined by @code{defmatch}.
Each rule is displayed with an intermediate expression label (@code{%t}).

@code{disprule (all)} displays all rules.

@code{disprule} quotes its arguments.
@code{disprule} returns the list of intermediate expression labels corresponding
to the displayed rules.

See also @mrefcomma{letrules}  which displays rules defined by @mrefdot{let}

Examples:

@c ===beg===
@c tellsimpafter (foo (x, y), bar (x) + baz (y));
@c tellsimpafter (x + y, special_add (x, y));
@c defmatch (quux, mumble (x));
@c disprule (foorule1, ?\+rule1, quux);
@c ev(%);
@c ===end===
@example
@group
(%i1) tellsimpafter (foo (x, y), bar (x) + baz (y));
(%o1)                   [foorule1, false]
@end group
@group
(%i2) tellsimpafter (x + y, special_add (x, y));
(%o2)                   [+rule1, simplus]
@end group
@group
(%i3) defmatch (quux, mumble (x));
(%o3)                         quux
@end group
@group
(%i4) disprule (foorule1, ?\+rule1, quux);
(%t4)        foorule1 : foo(x, y) -> baz(y) + bar(x)

(%t5)          +rule1 : y + x -> special_add(x, y)

(%t6)                quux : mumble(x) -> []

(%o6)                    [%t4, %t5, %t6]
@end group
@group
(%i7) ev(%);
(%o7) [foorule1 : foo(x, y) -> baz(y) + bar(x), 
     +rule1 : y + x -> special_add(x, y), quux : mumble(x) -> []]
@end group
@end example

@opencatbox
@category{Rules and patterns} @category{Display functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{let}
@deffn  {Function} let @
@fname{let} (@var{prod}, @var{repl}, @var{predname}, @var{arg_1}, @dots{}, @var{arg_n}) @
@fname{let} ([@var{prod}, @var{repl}, @var{predname}, @var{arg_1}, @dots{}, @var{arg_n}], @var{package_name})

Defines a substitution rule for @code{letsimp} such that @var{prod} is replaced
by @var{repl}.  @var{prod} is a product of positive or negative powers of the
following terms:

@itemize @bullet
@item
Atoms which @code{letsimp} will search for literally unless previous to calling
@code{letsimp} the @code{matchdeclare} function is used to associate a
predicate with the atom.  In this case @code{letsimp} will match the atom to
any term of a product satisfying the predicate.
@item
Kernels such as @code{sin(x)}, @code{n!}, @code{f(x,y)}, etc.  As with atoms
above @code{letsimp} will look for a literal match unless @code{matchdeclare}
is used to associate a predicate with the argument of the kernel.
@end itemize

A term to a positive power will only match a term having at least that
power.  A term to a negative power
on the other hand will only match a term with a power at least as
negative.  In the case of negative powers in @var{prod} the switch
@code{letrat} must be set to @code{true}.
See also @mrefdot{letrat}

If a predicate is included in the @code{let} function followed by a list of
arguments, a tentative match (i.e. one that would be accepted if the predicate
were omitted) is accepted only if @code{predname (arg_1', ..., arg_n')}
evaluates to @code{true} where @var{arg_i'} is the value matched to @var{arg_i}.
The @var{arg_i} may be the name of any atom or the argument of any kernel
appearing in @var{prod}.
@var{repl} may be any rational expression.  @c ONLY RATIONAL -- REALLY ??
If any of the atoms or arguments from @var{prod} appear in @var{repl} the
appropriate substitutions are made.  @c SPELL OUT "APPROPRIATE" IN THIS CONTEXT

The global flag @code{letrat} controls the simplification of quotients by
@code{letsimp}.  When @code{letrat} is @code{false}, @code{letsimp} simplifies
the numerator and denominator of @var{expr} separately, and does not simplify
the quotient.  Substitutions such as @code{n!/n} goes to @code{(n-1)!} then
fail.  When @code{letrat} is @code{true}, then the numerator, denominator, and
the quotient are simplified in that order.

These substitution functions allow you to work with several rule packages at
once.  Each rule package can contain any number of @code{let} rules and is
referenced by a user-defined name.  The command @code{let ([@var{prod},
@var{repl}, @var{predname}, @var{arg_1}, ..., @var{arg_n}], @var{package_name})}
adds the rule @var{predname} to the rule package @var{package_name}.  The
command @code{letsimp (@var{expr}, @var{package_name})} applies the rules in 
@var{package_name}.  @code{letsimp (@var{expr}, @var{package_name1},
@var{package_name2}, ...)} is equivalent to @code{letsimp (@var{expr},
@var{package_name1})} followed by @code{letsimp (%, @var{package_name2})},
@dots{}

@code{current_let_rule_package} is the name of the rule package that is
presently being used.  This variable may be assigned the name of any rule
package defined via the @code{let} command.  Whenever any of the functions
comprising the @code{let} package are called with no package name, the package
named by @code{current_let_rule_package} is used.  If a call such as
@code{letsimp (@var{expr}, @var{rule_pkg_name})} is made, the rule package
@var{rule_pkg_name} is used for that @code{letsimp} command only, and
@code{current_let_rule_package} is not changed.  If not otherwise specified,
@code{current_let_rule_package} defaults to @code{default_let_rule_package}.

@example
(%i1) matchdeclare ([a, a1, a2], true)$
(%i2) oneless (x, y) := is (x = y-1)$
(%i3) let (a1*a2!, a1!, oneless, a2, a1);
(%o3)         a1 a2! --> a1! where oneless(a2, a1)
(%i4) letrat: true$
(%i5) let (a1!/a1, (a1-1)!);
                        a1!
(%o5)                   --- --> (a1 - 1)!
                        a1
(%i6) letsimp (n*m!*(n-1)!/m);
(%o6)                      (m - 1)! n!
(%i7) let (sin(a)^2, 1 - cos(a)^2);
                        2               2
(%o7)                sin (a) --> 1 - cos (a)
(%i8) letsimp (sin(x)^4);
                        4           2
(%o8)                cos (x) - 2 cos (x) + 1
@end example

@c NEEDS ADDITIONAL EXAMPLES
@opencatbox
@category{Rules and patterns}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{letrat}
@defvr {Option variable} letrat
Default value: @code{false}

When @code{letrat} is @code{false}, @code{letsimp} simplifies the
numerator and denominator of a ratio separately,
and does not simplify the quotient.

When @code{letrat} is @code{true},
the numerator, denominator, and their quotient are simplified in that order.

@example
(%i1) matchdeclare (n, true)$
(%i2) let (n!/n, (n-1)!);
                         n!
(%o2)                    -- --> (n - 1)!
                         n
(%i3) letrat: false$
(%i4) letsimp (a!/a);
                               a!
(%o4)                          --
                               a
(%i5) letrat: true$
(%i6) letsimp (a!/a);
(%o6)                       (a - 1)!
@end example

@opencatbox
@category{Rules and patterns}
@closecatbox
@end defvr

@c NEEDS EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{letrules}
@deffn  {Function} letrules @
@fname{letrules} () @
@fname{letrules} (@var{package_name})

Displays the rules in a rule package.
@code{letrules ()} displays the rules in the current rule package.
@code{letrules (@var{package_name})} displays the rules in @var{package_name}.

The current rule package is named by @code{current_let_rule_package}.
If not otherwise specified, @code{current_let_rule_package}
defaults to @code{default_let_rule_package}.

See also @mrefcomma{disprule} which displays rules defined by @mref{tellsimp} and
@mrefdot{tellsimpafter}
@c WHAT ABOUT defmatch AND defrule ??

@opencatbox
@category{Rules and patterns}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{letsimp}
@deffn  {Function} letsimp @
@fname{letsimp} (@var{expr}) @
@fname{letsimp} (@var{expr}, @var{package_name}) @
@fname{letsimp} (@var{expr}, @var{package_name_1}, @dots{}, @var{package_name_n})

Repeatedly applies the substitution rules defined by @code{let}
until no further change is made to @var{expr}.

@code{letsimp (@var{expr})} uses the rules from @code{current_let_rule_package}.

@code{letsimp (@var{expr}, @var{package_name})} uses the rules from
@var{package_name} without changing @code{current_let_rule_package}.

@code{letsimp (@var{expr}, @var{package_name_1}, ..., @var{package_name_n})}
is equivalent to @code{letsimp (@var{expr}, @var{package_name_1})},
followed by @code{letsimp (%, @var{package_name_2})}, and so on.

See also @mrefdot{let}
For other ways to do substitutions see also @mrefcomma{subst}
@mrefcomma{psubst} @mref{at} and @mrefdot{ratsubst}

@c NEEDS more or better EXAMPLES

@c ===beg===
@c e0:e(k)=-(9*y(k))/(5*z)-u(k-1)/(5*z)+(4*y(k))/(5*z^2)+(3*u(k-1))/(5*z^2)+y(k)-(2*u(k-1))/5;
@c matchdeclare(h,any)$
@c let(u(h)/z,u(h-1));
@c let(y(h)/z, y(h-1));
@c e1:letsimp(e0);
@c ===end===
@example
@group
(%i1) e0:e(k)=-(9*y(k))/(5*z)-u(k-1)/(5*z)+(4*y(k))/(5*z^2)+(3*u(k-1))/(5*z^2)+y(k)-(2*u(k-1))/5;
                9 y(k)    u(k - 1)   4 y(k)   3 u(k - 1)
(%o1) e(k) = (- ------) - -------- + ------ + ---------- + y(k)
                 5 z        5 z          2          2
                                      5 z        5 z
                                                       2 u(k - 1)
                                                     - ----------
                                                           5
@end group
(%i2) matchdeclare(h,any)$
@group
(%i3) let(u(h)/z,u(h-1));
                        u(h)
(%o3)                   ---- --> u(h - 1)
                         z
@end group
@group
(%i4) let(y(h)/z, y(h-1));
                        y(h)
(%o4)                   ---- --> y(h - 1)
                         z
@end group
@group
(%i5) e1:letsimp(e0);
                2 u(k - 1)           3 u(k - 3)   4 y(k - 2)
(%o5) e(k) = (- ----------) + y(k) + ---------- + ----------
                    5                    5            5
                                       u(k - 2)       9 y(k - 1)
                                  + (- --------) + (- ----------)
                                          5               5
@end group
@end example
@opencatbox
@category{Rules and patterns}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{let_rule_packages}
@defvr {Option variable} let_rule_packages
Default value: @code{[default_let_rule_package]}

@code{let_rule_packages} is a list of all user-defined let rule packages
plus the default package @code{default_let_rule_package}.

@opencatbox
@category{Rules and patterns}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{matchdeclare}
@deffn {Function} matchdeclare (@var{a_1}, @var{pred_1}, @dots{}, @var{a_n}, @var{pred_n})

Associates a predicate @var{pred_k} 
with a variable or list of variables @var{a_k}
so that @var{a_k} matches expressions
for which the predicate returns anything other than @code{false}.

A predicate is the name of a function,
or a lambda expression,
or a function call or lambda call missing the last argument,
or @code{true} or @code{all}.
Any expression matches @code{true} or @code{all}.
If the predicate is specified as a function call or lambda call,
the expression to be tested is appended to the list of arguments;
the arguments are evaluated at the time the match is evaluated.
Otherwise, the predicate is specified as a function name or lambda expression,
and the expression to be tested is the sole argument.
A predicate function need not be defined when @code{matchdeclare} is called;
the predicate is not evaluated until a match is attempted.

A predicate may return a Boolean expression as well as @code{true} or
@code{false}.  Boolean expressions are evaluated by @code{is} within the
constructed rule function, so it is not necessary to call @code{is} within the
predicate.

If an expression satisfies a match predicate, the match variable is assigned the
expression, except for match variables which are operands of addition @code{+}
or multiplication @code{*}.  Only addition and multiplication are handled
specially; other n-ary operators (both built-in and user-defined) are treated
like ordinary functions.
@c WOULD BE GREAT TO EXTEND PART+/PART* PROCESSING TO ALL N-ARY OPERATORS

In the case of addition and multiplication, the match variable may be assigned a
single expression which satisfies the match predicate, or a sum or product
(respectively) of such expressions.  Such multiple-term matching is greedy:
predicates are evaluated in the order in which their associated variables
appear in the match pattern, and a term which satisfies more than one predicate
is taken by the first predicate which it satisfies.  Each predicate is tested
against all operands of the sum or product before the next predicate is
evaluated.  In addition, if 0 or 1 (respectively) satisfies a match predicate,
and there are no other terms which satisfy the predicate, 0 or 1 is assigned to
the match variable associated with the predicate.

The algorithm for processing addition and multiplication patterns makes some
match results (for example, a pattern in which a "match anything" variable
appears) dependent on the ordering of terms in the match pattern and in the
expression to be matched.  However, if all match predicates are mutually
exclusive, the match result is insensitive to ordering, as one match predicate
cannot accept terms matched by another.

Calling @code{matchdeclare} with a variable @var{a} as an argument changes the
@code{matchdeclare} property for @var{a}, if one was already declared; only the
most recent @code{matchdeclare} is in effect when a rule is defined.  Later
changes to the @code{matchdeclare} property (via @code{matchdeclare} or
@code{remove}) do not affect existing rules.

@code{propvars (matchdeclare)} returns the list of all variables for which there
is a @code{matchdeclare} property.  @code{printprops (@var{a}, matchdeclare)}
returns the predicate for variable @code{a}.
@code{printprops (all, matchdeclare)} returns the list of predicates for all
@code{matchdeclare} variables.  @code{remove (@var{a}, matchdeclare)} removes
the @code{matchdeclare} property from @var{a}.

The functions @code{defmatch}, @code{defrule}, @code{tellsimp},
@code{tellsimpafter}, and @code{let} construct rules which test expressions
against patterns.

@code{matchdeclare} quotes its arguments.
@code{matchdeclare} always returns @code{done}.

Examples:

A predicate is the name of a function,
or a lambda expression,
or a function call or lambda call missing the last argument,
or @code{true} or @code{all}.

@c ===beg===
@c matchdeclare (aa, integerp);
@c matchdeclare (bb, lambda ([x], x > 0));
@c matchdeclare (cc, freeof (%e, %pi, %i));
@c matchdeclare (dd, lambda ([x, y], gcd (x, y) = 1) (1728));
@c matchdeclare (ee, true);
@c matchdeclare (ff, all);
@c ===end===
@example
@group
(%i1) matchdeclare (aa, integerp);
(%o1)                         done
@end group
@group
(%i2) matchdeclare (bb, lambda ([x], x > 0));
(%o2)                         done
@end group
@group
(%i3) matchdeclare (cc, freeof (%e, %pi, %i));
(%o3)                         done
@end group
@group
(%i4) matchdeclare (dd, lambda ([x, y], gcd (x, y) = 1) (1728));
(%o4)                         done
@end group
@group
(%i5) matchdeclare (ee, true);
(%o5)                         done
@end group
@group
(%i6) matchdeclare (ff, all);
(%o6)                         done
@end group
@end example

If an expression satisfies a match predicate,
the match variable is assigned the expression.

@c ===beg===
@c matchdeclare (aa, integerp, bb, atom);
@c defrule (r1, bb^aa, ["integer" = aa, "atom" = bb]);
@c r1 (%pi^8);
@c ===end===
@example
@group
(%i1) matchdeclare (aa, integerp, bb, atom);
(%o1)                         done
@end group
@group
(%i2) defrule (r1, bb^aa, ["integer" = aa, "atom" = bb]);
                    aa
(%o2)        r1 : bb   -> [integer = aa, atom = bb]
@end group
@group
(%i3) r1 (%pi^8);
(%o3)               [integer = 8, atom = %pi]
@end group
@end example

In the case of addition and multiplication, the match variable may be assigned
a single expression which satisfies the match predicate, or a sum or product
(respectively) of such expressions.

@c ===beg===
@c matchdeclare (aa, atom, bb, lambda ([x], not atom(x)));
@c defrule (r1, aa + bb, ["all atoms" = aa, "all nonatoms" = 
@c                bb]);
@c r1 (8 + a*b + sin(x));
@c defrule (r2, aa * bb, ["all atoms" = aa, "all nonatoms" = 
@c                bb]);
@c r2 (8 * (a + b) * sin(x));
@c ===end===
@example
@group
(%i1) matchdeclare (aa, atom, bb, lambda ([x], not atom(x)));
(%o1)                         done
@end group
@group
(%i2) defrule (r1, aa + bb, ["all atoms" = aa, "all nonatoms" =
               bb]);
(%o2)  r1 : bb + aa -> [all atoms = aa, all nonatoms = bb]
@end group
@group
(%i3) r1 (8 + a*b + sin(x));
(%o3)     [all atoms = 8, all nonatoms = sin(x) + a b]
@end group
@group
(%i4) defrule (r2, aa * bb, ["all atoms" = aa, "all nonatoms" =
               bb]);
(%o4)   r2 : aa bb -> [all atoms = aa, all nonatoms = bb]
@end group
@group
(%i5) r2 (8 * (a + b) * sin(x));
(%o5)    [all atoms = 8, all nonatoms = (b + a) sin(x)]
@end group
@end example

When matching arguments of @code{+} and @code{*},
if all match predicates are mutually exclusive,
the match result is insensitive to ordering,
as one match predicate cannot accept terms matched by another.

@c ===beg===
@c matchdeclare (aa, atom, bb, lambda ([x], not atom(x)));
@c defrule (r1, aa + bb, ["all atoms" = aa, "all nonatoms" = 
@c                bb]);
@c r1 (8 + a*b + %pi + sin(x) - c + 2^n);
@c defrule (r2, aa * bb, ["all atoms" = aa, "all nonatoms" = 
@c                bb]);
@c r2 (8 * (a + b) * %pi * sin(x) / c * 2^n);
@c ===end===
@example
@group
(%i1) matchdeclare (aa, atom, bb, lambda ([x], not atom(x)));
(%o1)                         done
@end group
@group
(%i2) defrule (r1, aa + bb, ["all atoms" = aa, "all nonatoms" =
               bb]);
(%o2)  r1 : bb + aa -> [all atoms = aa, all nonatoms = bb]
@end group
@group
(%i3) r1 (8 + a*b + %pi + sin(x) - c + 2^n);
                                                     n
(%o3) [all atoms = %pi + 8, all nonatoms = sin(x) + 2  - c + a b]
@end group
@group
(%i4) defrule (r2, aa * bb, ["all atoms" = aa, "all nonatoms" =
               bb]);
(%o4)   r2 : aa bb -> [all atoms = aa, all nonatoms = bb]
@end group
@group
(%i5) r2 (8 * (a + b) * %pi * sin(x) / c * 2^n);
                                                n + 3
                                       (b + a) 2      sin(x)
(%o5) [all atoms = %pi, all nonatoms = ---------------------]
                                                 c
@end group
@end example

The functions @code{propvars} and @code{printprops} return information about
match variables.

@c ===beg===
@c matchdeclare ([aa, bb, cc], atom, [dd, ee], integerp);
@c matchdeclare (ff, floatnump, gg, lambda ([x], x > 100));
@c propvars (matchdeclare);
@c printprops (ee, matchdeclare);
@c printprops (gg, matchdeclare);
@c printprops (all, matchdeclare);
@c ===end===
@example
@group
(%i1) matchdeclare ([aa, bb, cc], atom, [dd, ee], integerp);
(%o1)                         done
@end group
@group
(%i2) matchdeclare (ff, floatnump, gg, lambda ([x], x > 100));
(%o2)                         done
@end group
@group
(%i3) propvars (matchdeclare);
(%o3)             [aa, bb, cc, dd, ee, ff, gg]
@end group
@group
(%i4) printprops (ee, matchdeclare);
(%o4)                    [integerp(ee)]
@end group
@group
(%i5) printprops (gg, matchdeclare);
(%o5)              [lambda([x], x > 100, gg)]
@end group
@group
(%i6) printprops (all, matchdeclare);
(%o6) [lambda([x], x > 100, gg), floatnump(ff), integerp(ee), 
                      integerp(dd), atom(cc), atom(bb), atom(aa)]
@end group
@end example

@opencatbox
@category{Rules and patterns} @category{Declarations and inferences}
@closecatbox
@end deffn

@c NEEDS EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{maxapplydepth}
@defvr {Option variable} maxapplydepth
Default value: 10000

@code{maxapplydepth} is the maximum depth to which @code{apply1}
and @code{apply2} will delve.

@opencatbox
@category{Function application}
@closecatbox
@end defvr

@c NEEDS EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{maxapplyheight}
@defvr {Option variable} maxapplyheight
Default value: 10000

@code{maxapplyheight} is the maximum height to which @code{applyb1}
will reach before giving up.

@opencatbox
@category{Function application}
@closecatbox
@end defvr

@c NEEDS CLARIFICATION AND EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{remlet}
@deffn  {Function} remlet @
@fname{remlet} (@var{prod}, @var{name}) @
@fname{remlet} () @
@fname{remlet} (all) @
@fname{remlet} (all, @var{name})

Deletes the substitution rule, @var{prod} --> repl, most
recently defined by the @code{let} function.  If name is supplied the rule is
deleted from the rule package name.

@code{remlet()} and @code{remlet(all)} delete all substitution rules from the
current rule package.  If the name of a rule package is supplied, e.g.
@code{remlet (all, @var{name})}, the rule package @var{name} is also deleted.

If a substitution is to be changed using the same
product, @code{remlet} need not be called, just redefine the substitution
using the same product (literally) with the @code{let} function and the new
replacement and/or predicate name.  Should @code{remlet (@var{prod})} now be
called the original substitution rule is revived.

See also @mrefcomma{remrule} which removes a rule defined by @mref{tellsimp} or
@mrefdot{tellsimpafter}

@opencatbox
@category{Rules and patterns}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{remrule}
@deffn  {Function} remrule @
@fname{remrule} (@var{op}, @var{rulename}) @
@fname{remrule} (@var{op}, all)

Removes rules defined by @code{tellsimp} or @code{tellsimpafter}.

@code{remrule (@var{op}, @var{rulename})}
removes the rule with the name @var{rulename} from the operator @var{op}.
When @var{op} is a built-in or user-defined operator
(as defined by @code{infix}, @code{prefix}, etc.),
@var{op} and @var{rulename} must be enclosed in double quote marks.

@code{remrule (@var{op}, all)} removes all rules for the operator @var{op}.

See also @mrefcomma{remlet} which removes a rule defined by @mrefdot{let}

Examples:

@c ===beg===
@c tellsimp (foo (aa, bb), bb - aa);
@c tellsimpafter (aa + bb, special_add (aa, bb));
@c infix ("@@");
@c tellsimp (aa @@ bb, bb/aa);
@c tellsimpafter (quux (%pi, %e), %pi - %e);
@c tellsimpafter (quux (%e, %pi), %pi + %e);
@c [foo (aa, bb), aa + bb, aa @@ bb, quux (%pi, %e), 
@c        quux (%e, %pi)];
@c remrule (foo, foorule1);
@c remrule ("+", ?\+rule1);
@c remrule ("@@", ?\@\@rule1);
@c remrule (quux, all);
@c [foo (aa, bb), aa + bb, aa @@ bb, quux (%pi, %e), 
@c         quux (%e, %pi)];
@c ===end===
@example
@group
(%i1) tellsimp (foo (aa, bb), bb - aa);
(%o1)                   [foorule1, false]
@end group
@group
(%i2) tellsimpafter (aa + bb, special_add (aa, bb));
(%o2)                   [+rule1, simplus]
@end group
@group
(%i3) infix ("@@@@");
(%o3)                          @@@@
@end group
@group
(%i4) tellsimp (aa @@@@ bb, bb/aa);
(%o4)                   [@@@@rule1, false]
@end group
@group
(%i5) tellsimpafter (quux (%pi, %e), %pi - %e);
(%o5)                  [quuxrule1, false]
@end group
@group
(%i6) tellsimpafter (quux (%e, %pi), %pi + %e);
(%o6)             [quuxrule2, quuxrule1, false]
@end group
@group
(%i7) [foo (aa, bb), aa + bb, aa @@@@ bb, quux (%pi, %e),
       quux (%e, %pi)];
                                     bb
(%o7) [bb - aa, special_add(aa, bb), --, %pi - %e, %pi + %e]
                                     aa
@end group
@group
(%i8) remrule (foo, foorule1);
(%o8)                          foo
@end group
@group
(%i9) remrule ("+", ?\+rule1);
(%o9)                           +
@end group
@group
(%i10) remrule ("@@@@", ?\@@\@@rule1);
(%o10)                         @@@@
@end group
@group
(%i11) remrule (quux, all);
(%o11)                        quux
@end group
@group
(%i12) [foo (aa, bb), aa + bb, aa @@@@ bb, quux (%pi, %e),
        quux (%e, %pi)];
(%o12) [foo(aa, bb), bb + aa, aa @@@@ bb, quux(%pi, %e), 
                                                   quux(%e, %pi)]
@end group
@end example

@opencatbox
@category{Rules and patterns}
@closecatbox
@end deffn

@c NEEDS EXPANSION OR MAYBE JUST APPROPRIATE REFS TO tellsimpafter

@c -----------------------------------------------------------------------------
@anchor{tellsimp}
@deffn {Function} tellsimp (@var{pattern}, @var{replacement})

is similar to @code{tellsimpafter} but places
new information before old so that it is applied before the built-in
simplification rules.

@code{tellsimp} is used when it is important to modify
the expression before the simplifier works on it, for instance if the
simplifier "knows" something about the expression, but what it returns
is not to your liking.
If the simplifier "knows" something about the
main operator of the expression, but is simply not doing enough for
you, you probably want to use @code{tellsimpafter}.

The pattern may not be a
sum, product, single variable, or number.

The system variable @code{rules} is the list of rules defined by
@code{defrule}, @code{defmatch}, @code{tellsimp}, and @code{tellsimpafter}.

Examples:

@example
(%i1) matchdeclare (x, freeof (%i));
(%o1)                         done
(%i2) %iargs: false$
(%i3) tellsimp (sin(%i*x), %i*sinh(x));
(%o3)                 [sinrule1, simp-%sin]
(%i4) trigexpand (sin (%i*y + x));
(%o4)         sin(x) cos(%i y) + %i cos(x) sinh(y)
(%i5) %iargs:true$
(%i6) errcatch(0^0);
 0
0  has been generated
(%o6)                          []
(%i7) ev (tellsimp (0^0, 1), simp: false);
(%o7)                  [^rule1, simpexpt]
(%i8) 0^0;
(%o8)                           1
(%i9) remrule ("^", %th(2)[1]);
(%o9)                           ^
(%i10) tellsimp (sin(x)^2, 1 - cos(x)^2);
(%o10)                 [^rule2, simpexpt]
(%i11) (1 + sin(x))^2;
                                      2
(%o11)                    (sin(x) + 1)
(%i12) expand (%);
                                   2
(%o12)               2 sin(x) - cos (x) + 2
(%i13) sin(x)^2;
                                  2
(%o13)                     1 - cos (x)
(%i14) kill (rules);
(%o14)                        done
(%i15) matchdeclare (a, true);
(%o15)                        done
(%i16) tellsimp (sin(a)^2, 1 - cos(a)^2);
(%o16)                 [^rule3, simpexpt]
(%i17) sin(y)^2;
                                  2
(%o17)                     1 - cos (y)
@end example

@opencatbox
@category{Rules and patterns}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{tellsimpafter}
@deffn {Function} tellsimpafter (@var{pattern}, @var{replacement})

Defines a simplification rule which the Maxima simplifier applies after built-in
simplification rules.  @var{pattern} is an expression, comprising pattern
variables (declared by @code{matchdeclare}) and other atoms and operators,
considered literals for the purpose of pattern matching.  @var{replacement} is
substituted for an actual expression which matches @var{pattern}; pattern
variables in @var{replacement} are assigned the values matched in the actual
expression.

@var{pattern} may be any nonatomic expression in which the main operator is not
a pattern variable; the simplification rule is associated with the main
operator.  The names of functions (with one exception, described below), lists,
and arrays may appear in @var{pattern} as the main operator only as literals
(not pattern variables); this rules out expressions such as @code{aa(x)} and
@code{bb[y]} as patterns, if @code{aa} and @code{bb} are pattern variables.
Names of functions, lists, and arrays which are pattern variables may appear as
operators other than the main operator in @var{pattern}.

There is one exception to the above rule concerning names of functions.
The name of a subscripted function in an expression such as @code{aa[x](y)}
may be a pattern variable, because the main operator is not @code{aa} but rather
the Lisp atom @code{mqapply}.  This is a consequence of the representation of
expressions involving subscripted functions.

@c LET'S NOT GO INTO DETAILS ABOUT MAIN OPERATORS HERE; BUT PRESERVE THIS FOR REFERENCE
@c The main operator of an expression @code{expr} is @code{caar $expr}.
@c For most kinds of expressions,
@c the main operator is the operator returned by @code{op (@var{pattern})};
@c the sole exception is the operator @code{mqapply},
@c which appears in indexed function expressions (e.g., @code{foo[i](x)}).

@c NEED TO REVIEW THIS PARAGRAPH FOR ACCURACY
Simplification rules are applied after evaluation 
(if not suppressed through quotation or the flag @code{noeval}).
Rules established by @code{tellsimpafter} are applied in the order they were
defined, and after any built-in rules.
Rules are applied bottom-up, that is,
applied first to subexpressions before application to the whole expression.
@c NO IT IS MORE COMPLICATED THAN THIS, ALTHOUGH IN SOME CIRCUMSTANCE IT APPEARS TO BE THE CASE:
@c For a given expression, at most one rule per operator is applied.
It may be necessary to repeatedly simplify a result (for example, via the
quote-quote operator @code{'@w{}'} or the flag @code{infeval})
to ensure that all rules are applied.

Pattern variables are treated as local variables in simplification rules.
Once a rule is defined, the value of a pattern variable
does not affect the rule, and is not affected by the rule.
An assignment to a pattern variable which results from a successful rule match
does not affect the current assignment (or lack of it) of the pattern variable.
However, as with all atoms in Maxima, the properties of pattern variables (as
declared by @code{put} and related functions) are global.

The rule constructed by @code{tellsimpafter} is named after the main operator of
@var{pattern}.  Rules for built-in operators, and user-defined operators defined
by @code{infix}, @code{prefix}, @code{postfix}, @code{matchfix}, and
@code{nofix}, have names which are Lisp identifiers.
@c SLIGHTLY TOO MUCH DETAIL
@c (that is, the name is written with a leading question mark @code{?} in Maxima).
Rules for other functions have names which are Maxima identifiers.
@c SLIGHTLY TOO MUCH DETAIL
@c (that is, the name begins with dollar sign @code{$}).

The treatment of noun and verb forms is slightly confused.  @c THIS IS A BUG.
If a rule is defined for a noun (or verb) form
and a rule for the corresponding verb (or noun) form already exists, 
the newly-defined rule applies to both forms (noun and verb).
If a rule for the corresponding verb (or noun) form does not exist,
the newly-defined rule applies only to the noun (or verb) form.

The rule constructed by @code{tellsimpafter} is an ordinary Lisp function.
If the name of the rule is @code{$foorule1},
the construct @code{:lisp (trace $foorule1)} traces the function,
and @code{:lisp (symbol-function '$foorule1)} displays its definition.

@code{tellsimpafter} quotes its arguments.
@code{tellsimpafter} returns the list of rules for the main operator of
@var{pattern}, including the newly established rule.
@c WHAT IS simpfoo THAT SOMETIMES APPEARS, AND WHY DOES false SOMETIMES APPEAR IN RETURN VALUE ??

See also @mrefcomma{matchdeclare} @mrefcomma{defmatch} @mrefcomma{defrule} @mrefcomma{tellsimp}
@mrefcomma{let} @mrefcomma{kill} @mref{remrule} and @mrefdot{clear_rules}

Examples:

@var{pattern} may be any nonatomic expression in which the 
main operator is not a pattern variable.

@c ===beg===
@c matchdeclare (aa, atom, [ll, mm], listp, xx, true)$
@c tellsimpafter (sin (ll), map (sin, ll));
@c sin ([1/6, 1/4, 1/3, 1/2, 1]*%pi);
@c tellsimpafter (ll^mm, map ("^", ll, mm));
@c [a, b, c]^[1, 2, 3];
@c tellsimpafter (foo (aa (xx)), aa (foo (xx)));
@c foo (bar (u - v));
@c ===end===
@example
(%i1) matchdeclare (aa, atom, [ll, mm], listp, xx, true)$
@group
(%i2) tellsimpafter (sin (ll), map (sin, ll));
(%o2)                 [sinrule1, simp-%sin]
@end group
@group
(%i3) sin ([1/6, 1/4, 1/3, 1/2, 1]*%pi);
                    1     1     sqrt(3)
(%o3)              [-, -------, -------, 1, 0]
                    2  sqrt(2)     2
@end group
@group
(%i4) tellsimpafter (ll^mm, map ("^", ll, mm));
(%o4)                  [^rule1, simpexpt]
@end group
@group
(%i5) [a, b, c]^[1, 2, 3];
                                2   3
(%o5)                      [a, b , c ]
@end group
@group
(%i6) tellsimpafter (foo (aa (xx)), aa (foo (xx)));
(%o6)                   [foorule1, false]
@end group
@group
(%i7) foo (bar (u - v));
(%o7)                    bar(foo(u - v))
@end group
@end example

Rules are applied in the order they were defined.
If two rules can match an expression,
the rule which was defined first is applied.

@c ===beg===
@c matchdeclare (aa, integerp);
@c tellsimpafter (foo (aa), bar_1 (aa));
@c tellsimpafter (foo (aa), bar_2 (aa));
@c foo (42);
@c ===end===
@example
@group
(%i1) matchdeclare (aa, integerp);
(%o1)                         done
@end group
@group
(%i2) tellsimpafter (foo (aa), bar_1 (aa));
(%o2)                   [foorule1, false]
@end group
@group
(%i3) tellsimpafter (foo (aa), bar_2 (aa));
(%o3)              [foorule2, foorule1, false]
@end group
@group
(%i4) foo (42);
(%o4)                       bar_1(42)
@end group
@end example

Pattern variables are treated as local variables in simplification rules.
(Compare to @code{defmatch}, which treats pattern variables as global
variables.)

@c ===beg===
@c matchdeclare (aa, integerp, bb, atom);
@c tellsimpafter (foo(aa, bb), bar('aa=aa, 'bb=bb));
@c bb: 12345;
@c foo (42, %e);
@c bb;
@c ===end===
@example
@group
(%i1) matchdeclare (aa, integerp, bb, atom);
(%o1)                         done
@end group
@group
(%i2) tellsimpafter (foo(aa, bb), bar('aa=aa, 'bb=bb));
(%o2)                   [foorule1, false]
@end group
@group
(%i3) bb: 12345;
(%o3)                         12345
@end group
@group
(%i4) foo (42, %e);
(%o4)                 bar(aa = 42, bb = %e)
@end group
@group
(%i5) bb;
(%o5)                         12345
@end group
@end example

As with all atoms, properties of pattern variables are global even though values
are local.  In this example, an assignment property is declared via
@code{define_variable}.  This is a property of the atom @code{bb} throughout
Maxima.

@c ===beg===
@c matchdeclare (aa, integerp, bb, atom);
@c tellsimpafter (foo(aa, bb), bar('aa=aa, 'bb=bb));
@c foo (42, %e);
@c define_variable (bb, true, boolean);
@c foo (42, %e);
@c ===end===
@example
@group
(%i1) matchdeclare (aa, integerp, bb, atom);
(%o1)                         done
@end group
@group
(%i2) tellsimpafter (foo(aa, bb), bar('aa=aa, 'bb=bb));
(%o2)                   [foorule1, false]
@end group
@group
(%i3) foo (42, %e);
(%o3)                 bar(aa = 42, bb = %e)
@end group
@group
(%i4) define_variable (bb, true, boolean);
(%o4)                         true
@end group
@group
(%i5) foo (42, %e);
translator: bb was declared with mode boolean, but it has value: 
                                                               %e
 -- an error. To debug this try: debugmode(true);
@end group
@end example

Rules are named after main operators.
Names of rules for built-in and user-defined operators are Lisp identifiers,
while names for other functions are Maxima identifiers.

@c ===beg===
@c tellsimpafter (foo (%pi + %e), 3*%pi);
@c tellsimpafter (foo (%pi * %e), 17*%e);
@c tellsimpafter (foo (%i ^ %e), -42*%i);
@c tellsimpafter (foo (9) + foo (13), quux (22));
@c tellsimpafter (foo (9) * foo (13), blurf (22));
@c tellsimpafter (foo (9) ^ foo (13), mumble (22));
@c rules;
@c foorule_name: first (%o1);
@c plusrule_name: first (%o4);
@c remrule (foo, foorule1);
@c remrule ("^", ?\^rule1);
@c rules;
@c ===end===
@example
@group
(%i1) tellsimpafter (foo (%pi + %e), 3*%pi);
(%o1)                   [foorule1, false]
@end group
@group
(%i2) tellsimpafter (foo (%pi * %e), 17*%e);
(%o2)              [foorule2, foorule1, false]
@end group
@group
(%i3) tellsimpafter (foo (%i ^ %e), -42*%i);
(%o3)         [foorule3, foorule2, foorule1, false]
@end group
@group
(%i4) tellsimpafter (foo (9) + foo (13), quux (22));
(%o4)                   [+rule1, simplus]
@end group
@group
(%i5) tellsimpafter (foo (9) * foo (13), blurf (22));
(%o5)                  [*rule1, simptimes]
@end group
@group
(%i6) tellsimpafter (foo (9) ^ foo (13), mumble (22));
(%o6)                  [^rule1, simpexpt]
@end group
@group
(%i7) rules;
(%o7) [foorule1, foorule2, foorule3, +rule1, *rule1, ^rule1]
@end group
@group
(%i8) foorule_name: first (%o1);
(%o8)                       foorule1
@end group
@group
(%i9) plusrule_name: first (%o4);
(%o9)                        +rule1
@end group
@group
(%i10) remrule (foo, foorule1);
(%o10)                         foo
@end group
@group
(%i11) remrule ("^", ?\^rule1);
(%o11)                          ^
@end group
@group
(%i12) rules;
(%o12)        [foorule2, foorule3, +rule1, *rule1]
@end group
@end example

A worked example: anticommutative multiplication.

@c ===beg===
@c gt (i, j) := integerp(j) and i < j;
@c matchdeclare (i, integerp, j, gt(i));
@c tellsimpafter (s[i]^^2, 1);
@c tellsimpafter (s[i] . s[j], -s[j] . s[i]);
@c s[1] . (s[1] + s[2]);
@c expand (%);
@c factor (expand (sum (s[i], i, 0, 9)^^5));
@c ===end===
@example
@group
(%i1) gt (i, j) := integerp(j) and i < j;
(%o1)          gt(i, j) := integerp(j) and (i < j)
@end group
@group
(%i2) matchdeclare (i, integerp, j, gt(i));
(%o2)                         done
@end group
@group
(%i3) tellsimpafter (s[i]^^2, 1);
(%o3)                 [^^rule1, simpncexpt]
@end group
@group
(%i4) tellsimpafter (s[i] . s[j], -s[j] . s[i]);
(%o4)                   [.rule1, simpnct]
@end group
@group
(%i5) s[1] . (s[1] + s[2]);
(%o5)                    s  . (s  + s )
                          1     2    1
@end group
@group
(%i6) expand (%);
(%o6)                      1 - s  . s
                                2    1
@end group
@group
(%i7) factor (expand (sum (s[i], i, 0, 9)^^5));
(%o7) 100 (s  + s  + s  + s  + s  + s  + s  + s  + s  + s )
            9    8    7    6    5    4    3    2    1    0
@end group
@end example

@opencatbox
@category{Rules and patterns}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{clear_rules}
@deffn {Function} clear_rules ()

Executes @code{kill (rules)} and then resets the next rule number to 1
for addition @code{+}, multiplication @code{*}, and exponentiation @code{^}.

@opencatbox
@category{Rules and patterns}
@closecatbox
@end deffn
