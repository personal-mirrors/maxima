@c -*- Mode: texinfo -*-
@c end concepts Symmetries

@menu
* Introduction to Symmetries::
* Functions and Variables for Symmetries::
@end menu

@node Introduction to Symmetries, Functions and Variables for Symmetries, Symmetries, Symmetries
@section Introduction to Symmetries

@code{sym} is a package for working with symmetric groups of polynomials.

@ifnottex
It was written for Macsyma-Symbolics by Annick Valibouze (@url{http://www-calfor.lip6.fr/~avb/}).
The algorithms are described in the following papers:
@end ifnottex
@iftex
It was written for Macsyma-Symbolics by Annick Valibouze@footnote{@url{www-calfor.lip6.fr/~avb}}.
The algorithms are described in the following papers:
@end iftex

@ifnottex
@enumerate
@item
Fonctions sym@'etriques et changements de bases. Annick Valibouze.
EUROCAL'87 (Leipzig, 1987), 323--332, Lecture Notes in Comput. Sci 378.
Springer, Berlin, 1989.@*
@url{http://www.stix.polytechnique.fr/publications/1984-1994.html}

@item R@'esolvantes et fonctions sym@'etriques. Annick Valibouze.
Proceedings of the ACM-SIGSAM 1989 International Symposium on Symbolic
and Algebraic Computation, ISSAC'89 (Portland, Oregon).
ACM Press, 390-399, 1989.@*
@url{http://www-calfor.lip6.fr/~avb/DonneesTelechargeables/MesArticles/issac89ACMValibouze.pdf}

@item Symbolic computation with symmetric polynomials, an extension to Macsyma.
Annick Valibouze. Computers and Mathematics (MIT, USA, June 13-17, 1989),
Springer-Verlag, New York Berlin, 308-320, 1989.@*
@url{http://www.stix.polytechnique.fr/publications/1984-1994.html}

@item Th@'eorie de Galois Constructive. Annick Valibouze. M@'emoire d'habilitation
@`a diriger les recherches (HDR), Universit@'e P. et M. Curie (Paris VI), 1994.
@end enumerate
@end ifnottex

@iftex
@enumerate
@item
Fonctions sym@'etriques et changements de bases
@footnote{@url{www.stix.polytechnique.fr/publications/1984-1994.html}}.
Annick Valibouze. EUROCAL'87 (Leipzig, 1987), 323--332, Lecture Notes in Comput. Sci 378.
Springer, Berlin, 1989.

@item R@'esolvantes et fonctions sym@'etriques
@footnote{@url{www-calfor.lip6.fr/~avb/DonneesTelechargeables/MesArticles/issac89ACMValibouze.pdf}}.
Annick Valibouze. Proceedings of the ACM-SIGSAM 1989 International Symposium on Symbolic
and Algebraic Computation, ISSAC'89 (Portland, Oregon). ACM Press, 390-399, 1989.

@item Symbolic computation with symmetric polynomials, an extension to Macsyma
@footnote{@url{www.stix.polytechnique.fr/publications/1984-1994.html}}.
Annick Valibouze. Computers and Mathematics (MIT, USA, June 13-17, 1989),
Springer-Verlag, New York Berlin, 308-320, 1989.

@item Th@'eorie de Galois Constructive. Annick Valibouze. M@'emoire d'habilitation
@`a diriger les recherches (HDR), Universit@'e P. et M. Curie (Paris VI), 1994.
@end enumerate
@end iftex

@opencatbox
@category{Group theory}
@category{Polynomials}
@category{Share packages}
@category{Package sym}
@closecatbox


@node Functions and Variables for Symmetries,  , Introduction to Symmetries, Symmetries
@section Functions and Variables for Symmetries


@subsection Changing bases

m4_setcat(Package sym)
@anchor{comp2pui}
@c @deffn {Function} comp2pui (@var{n}, @var{L})
m4_deffn({Function}, comp2pui, <<<(@var{n}, @var{L})>>>)
implements passing from the complete symmetric functions given in the list
@var{L} to the elementary symmetric functions from 0 to @var{n}. If the
list @var{L} contains fewer than @var{n+1} elements, it will be completed with
formal values of the type @var{h1}, @var{h2}, etc. If the first element
of the list @var{L} exists, it specifies the size of the alphabet,
otherwise the size is set to @var{n}.

@c GENERATED FROM THE FOLLOWING
@c comp2pui (3, [4, g]);
@example
@group
(%i1) comp2pui (3, [4, g]);
                        2                    2
(%o1)    [4, g, 2 h2 - g , 3 h3 - g h2 + g (g  - 2 h2)]
@end group
@end example

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{ele2pui}
@c @deffn {Function} ele2pui (@var{m}, @var{L})
m4_deffn({Function}, ele2pui, <<<(@var{m}, @var{L})>>>)
goes from the elementary symmetric functions to the complete functions.
Similar to @code{comp2ele} and @code{comp2pui}.

Other functions for changing bases: @code{comp2ele}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{ele2comp}
@c @deffn {Function} ele2comp (@var{m}, @var{L})
m4_deffn({Function}, ele2comp, <<<(@var{m}, @var{L})>>>)
Goes from the elementary symmetric functions to the compete functions.
Similar to @code{comp2ele} and @code{comp2pui}.

Other functions for changing bases: @code{comp2ele}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{elem}
@c @deffn {Function} elem (@var{ele}, @var{sym}, @var{lvar})
m4_deffn({Function}, elem, <<<(@var{ele}, @var{sym}, @var{lvar})>>>)
decomposes the symmetric polynomial @var{sym}, in the variables
contained in the list @var{lvar}, in terms of the elementary symmetric
functions given in the list @var{ele}.  If the first element of
@var{ele} is given, it will be the size of the alphabet, otherwise the
size will be the degree of the polynomial @var{sym}.  If values are
missing in the list @var{ele}, formal values of the type @var{e1},
@var{e2}, etc. will be added.  The polynomial @var{sym} may be given in
three different forms: contracted (@code{elem} should then be 1, its
default value), partitioned (@code{elem} should be 3), or extended
(i.e. the entire polynomial, and @code{elem} should then be 2).  The
function @code{pui} is used in the same way.

On an alphabet of size 3 with @var{e1}, the first elementary symmetric
function, with value 7, the symmetric polynomial in 3 variables whose
contracted form (which here depends on only two of its variables) is
@var{x^4-2*x*y} decomposes as follows in elementary symmetric functions:

@c GENERATED FROM THE FOLLOWING
@c elem ([3, 7], x^4 - 2*x*y, [x, y]);
@c ratsimp (%);
@example
@group
(%i1) elem ([3, 7], x^4 - 2*x*y, [x, y]);
(%o1) 7 (e3 - 7 e2 + 7 (49 - e2)) + 21 e3

                                         + (- 2 (49 - e2) - 2) e2
@end group
@group
(%i2) ratsimp (%);
                              2
(%o2)             28 e3 + 2 e2  - 198 e2 + 2401
@end group
@end example

@noindent
Other functions for changing bases: @code{comp2ele}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{mon2schur}
@c @deffn {Function} mon2schur (@var{L})
m4_deffn({Function}, mon2schur, <<<(@var{L})>>>)
The list @var{L} represents the Schur function @math{S_L}: we have
@iftex
@math{L = [i_1,i_2, \ldots, i_q]}, with @math{i_1 \le i_2 \le \ldots \le i_q}.
The Schur function @math{S_{i_1,i_2, \ldots, i_q}} is the minor
of the infinite matrix @math{h_{i-j}}, @math{i \ge 1, j \ge 1},
consisting of the @math{q} first rows and the columns @math{i_1+1,
i_2+2, \ldots, i_q+q}.
@end iftex
@ifnottex
@math{L = [i_1, i_2, ..., i_q]}, with @math{i_1 <= i_2 <= ... <= i_q}.
The Schur function @math{S_[i_1, i_2, ..., i_q]} is the minor
of the infinite matrix @math{h_[i-j]}, @math{i <= 1, j <= 1},
consisting of the @math{q} first rows and the columns @math{1 + i_1,
2 + i_2, ..., q + i_q}.
@end ifnottex

This Schur function can be written in terms of monomials by using
@mref{treinat} and @mrefdot{kostka}  The form returned is a symmetric
polynomial in a contracted representation in the variables
m4_mathjax(
<<<\(x_1,x_2,\ldots\)>>>,
<<<@math{x_1,x_2,...}>>>,
<<<$x_1,x_2,\ldots$>>>)


@c GENERATED FROM THE FOLLOWING
@c mon2schur ([1, 1, 1]);
@c mon2schur ([3]);
@c mon2schur ([1, 2]);
@example
@group
(%i1) mon2schur ([1, 1, 1]);
(%o1)                       x1 x2 x3
@end group
@group
(%i2) mon2schur ([3]);
                                  2        3
(%o2)                x1 x2 x3 + x1  x2 + x1
@end group
@group
(%i3) mon2schur ([1, 2]);
                                      2
(%o3)                  2 x1 x2 x3 + x1  x2
@end group
@end example

@noindent
which means that for 3 variables this gives:

@c UM, FROM WHAT ARGUMENTS WAS THE FOLLOWING GENERATED ?? (original comment)
@example
@group
   2 x1 x2 x3 + x1^2 x2 + x2^2 x1 + x1^2 x3 + x3^2 x1
    + x2^2 x3 + x3^2 x2
@end group
@end example
@noindent
Other functions for changing bases: @code{comp2ele}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{multi}
@c @deffn {Function} multi_elem (@var{l_elem}, @var{multi_pc}, @var{l_var})
m4_deffn({Function}, multi_elem, <<<(@var{l_elem}, @var{multi_pc}, @var{l_var})>>>)
decomposes a multi-symmetric polynomial in the multi-contracted form
@var{multi_pc} in the groups of variables contained in the list of lists
@var{l_var} in terms of the elementary symmetric functions contained in
@var{l_elem}.

@c GENERATED FROM THE FOLLOWING
@c multi_elem ([[2, e1, e2], [2, f1, f2]], a*x + a^2 + x^3, [[x, y], [a, b]]);
@c ratsimp (%);
@example
@group
(%i1) multi_elem ([[2, e1, e2], [2, f1, f2]], a*x + a^2 + x^3,
      [[x, y], [a, b]]);
                                                  3
(%o1)         - 2 f2 + f1 (f1 + e1) - 3 e1 e2 + e1
@end group
@group
(%i2) ratsimp (%);
                         2                       3
(%o2)         - 2 f2 + f1  + e1 f1 - 3 e1 e2 + e1
@end group
@end example

Other functions for changing bases: @code{comp2ele}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@c WHAT ARE THE ARGUMENTS FOR THIS FUNCTION ?? (original comment)
@anchor{multi_pui}
@c @deffn {Function} multi_pui
m4_deffn({Function}, multi_pui, <<<()>>>)
is to the function @code{pui} what the function @code{multi_elem} is to
the function @code{elem}.

@c GENERATED FROM THE FOLLOWING
@c multi_pui ([[2, p1, p2], [2, t1, t2]], a*x + a^2 + x^3, [[x, y], [a, b]]);
@example
@group
(%i1) multi_pui ([[2, p1, p2], [2, t1, t2]], a*x + a^2 + x^3,
      [[x, y], [a, b]]);
                                            3
                                3 p1 p2   p1
(%o1)              t2 + p1 t1 + ------- - ---
                                   2       2
@end group
@end example

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@c HMM, pui IS A VARIABLE AS WELL.  It's a function, for sure.
@anchor{pui}
@c @deffn {Function} pui (@var{L}, @var{sym}, @var{lvar})
m4_deffn({Function}, pui, <<<(@var{L}, @var{sym}, @var{lvar})>>>)
decomposes the symmetric polynomial @var{sym}, in the variables in the
list @var{lvar}, in terms of the power functions in the list @var{L}.
If the first element of @var{L} is given, it will be the size of the
alphabet, otherwise the size will be the degree of the polynomial
@var{sym}.  If values are missing in the list @var{L}, formal values of
the type @var{p1}, @var{p2} , etc. will be added. The polynomial
@var{sym} may be given in three different forms: contracted (@code{elem}
should then be 1, its default value), partitioned (@code{elem} should be
3), or extended (i.e. the entire polynomial, and @code{elem} should then
be 2). The function @code{pui} is used in the same way.

@c GENERATED FROM THE FOLLOWING
@c pui;
@c pui ([3, a, b], u*x*y*z, [x, y, z]);
@c ratsimp (%);
@example
@group
(%i1) pui;
(%o1)                           1
@end group
@group
(%i2) pui ([3, a, b], u*x*y*z, [x, y, z]);
                       2
                   a (a  - b) u   (a b - p3) u
(%o2)              ------------ - ------------
                        6              3
@end group
@group
(%i3) ratsimp (%);
                                       3
                      (2 p3 - 3 a b + a ) u
(%o3)                 ---------------------
                                6
@end group
@end example
@noindent
Other functions for changing bases: @code{comp2ele}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{pui2comp}
@c @deffn {Function} pui2comp (@var{n}, @var{lpui})
m4_deffn({Function}, pui2comp, <<<(@var{n}, @var{lpui})>>>)
renders the list of the first @var{n} complete functions (with the
length first) in terms of the power functions given in the list
@var{lpui}. If the list @var{lpui} is empty, the cardinal is @var{n},
otherwise it is its first element (as in @code{comp2ele} and
@code{comp2pui}).

@c GENERATED FROM THE FOLLOWING
@c pui2comp (2, []);
@c pui2comp (3, [2, a1]);
@c ratsimp (%);
@example
@group
(%i1) pui2comp (2, []);
                                       2
                                p2 + p1
(%o1)                   [2, p1, --------]
                                   2
@end group
@group
(%i2) pui2comp (3, [2, a1]);
                                            2
                                 a1 (p2 + a1 )
                         2  p3 + ------------- + a1 p2
                  p2 + a1              2
(%o2)     [2, a1, --------, --------------------------]
                     2                  3
@end group
@group
(%i3) ratsimp (%);
                            2                     3
                     p2 + a1   2 p3 + 3 a1 p2 + a1
(%o3)        [2, a1, --------, --------------------]
                        2               6
@end group
@end example
@noindent
Other functions for changing bases: @code{comp2ele}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{pui2ele}
@c @deffn {Function} pui2ele (@var{n}, @var{lpui})
m4_deffn({Function}, pui2ele, <<<(@var{n}, @var{lpui})>>>)
effects the passage from power functions to the elementary symmetric functions.
If the flag @code{pui2ele} is @code{girard}, it will return the list of
elementary symmetric functions from 1 to @var{n}, and if the flag is
@code{close}, it will return the @var{n}-th elementary symmetric function.

Other functions for changing bases: @code{comp2ele}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{puireduc}
@c @deffn {Function} puireduc (@var{n}, @var{lpui})
m4_deffn({Function}, puireduc, <<<(@var{n}, @var{lpui})>>>)
@var{lpui} is a list whose first element is an integer @var{m}.
@code{puireduc} gives the first @var{n} power functions in terms of the
first @var{m}.

@c GENERATED FROM THE FOLLOWING
@c puireduc (3, [2]);
@example
@group
(%i1) puireduc (3, [2]);
                                         2
                                   p1 (p1  - p2)
(%o1)          [2, p1, p2, p1 p2 - -------------]
                                         2
@end group
@group
(%i2) ratsimp (%);
                                           3
                               3 p1 p2 - p1
(%o2)              [2, p1, p2, -------------]
                                     2
@end group
@end example

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{schur2comp}
@c @deffn {Function} schur2comp (@var{P}, @var{l_var})
m4_deffn({Function}, schur2comp, <<<(@var{P}, @var{l_var})>>>)
@var{P} is a polynomial in the variables of the list @var{l_var}.  Each
of these variables represents a complete symmetric function.  In
@var{l_var} the @var{i}-th complete symmetric function is represented by
the concatenation of the letter @code{h} and the integer @var{i}:
@code{h@var{i}}.  This function expresses @var{P} in terms of Schur
functions.


@c GENERATED FROM THE FOLLOWING
@c schur2comp (h1*h2 - h3, [h1, h2, h3]);
@c schur2comp (a*h3, [h3]);
@example
@group
(%i1) schur2comp (h1*h2 - h3, [h1, h2, h3]);
(%o1)                         s
                               1, 2
@end group
@group
(%i2) schur2comp (a*h3, [h3]);
(%o2)                         s  a
                               3
@end group
@end example

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()





@subsection Changing representations
@anchor{cont2part}
@c @deffn {Function} cont2part (@var{pc}, @var{lvar})
m4_deffn({Function}, cont2part, <<<(@var{pc}, @var{lvar})>>>)
returns the partitioned polynomial associated to the contracted form
@var{pc} whose variables are in @var{lvar}.

@c GENERATED FROM THE FOLLOWING
@c pc: 2*a^3*b*x^4*y + x^5;
@c cont2part (pc, [x, y]);
@example
@group
(%i1) pc: 2*a^3*b*x^4*y + x^5;
                           3    4      5
(%o1)                   2 a  b x  y + x
@end group
@group
(%i2) cont2part (pc, [x, y]);
                                   3
(%o2)              [[1, 5, 0], [2 a  b, 4, 1]]
@end group
@end example

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{contract}
@c @deffn {Function} contract (@var{psym}, @var{lvar})
m4_deffn({Function}, contract, <<<(@var{psym}, @var{lvar})>>>)
returns a contracted form (i.e. a monomial orbit under the action of the
@c CHECK ME!!
symmetric group) of the polynomial @var{psym} in the variables contained
in the list @var{lvar}.  The function @code{explose} performs the
inverse operation.  The function @code{tcontract} tests the symmetry of
the polynomial.

@c GENERATED FROM THE FOLLOWING
@c psym: explose (2*a^3*b*x^4*y, [x, y, z]);
@c contract (psym, [x, y, z]);
@example
@group
(%i1) psym: explose (2*a^3*b*x^4*y, [x, y, z]);
         3      4      3      4      3    4        3    4
(%o1) 2 a  b y z  + 2 a  b x z  + 2 a  b y  z + 2 a  b x  z

                                           3      4      3    4
                                      + 2 a  b x y  + 2 a  b x  y
@end group
@group
(%i2) contract (psym, [x, y, z]);
                              3    4
(%o2)                      2 a  b x  y
@end group
@end example

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{explose}
@c @deffn {Function} explose (@var{pc}, @var{lvar})
m4_deffn({Function}, explose, <<<(@var{pc}, @var{lvar})>>>)
returns the symmetric polynomial associated with the contracted form
@var{pc}. The list @var{lvar} contains the variables.

@c GENERATED FROM THE FOLLOWING
@c explose (a*x + 1, [x, y, z]);
@example
@group
(%i1) explose (a*x + 1, [x, y, z]);
(%o1)                  a z + a y + a x + 1
@end group
@end example

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{part2cont}
@c @deffn {Function} part2cont (@var{ppart}, @var{lvar})
m4_deffn({Function}, part2cont, <<<(@var{ppart}, @var{lvar})>>>)
goes from the partitioned form to the contracted form of a symmetric polynomial.
The contracted form is rendered with the variables in @var{lvar}.

@c GENERATED FROM THE FOLLOWING
@c part2cont ([[2*a^3*b, 4, 1]], [x, y]);
@example
@group
(%i1) part2cont ([[2*a^3*b, 4, 1]], [x, y]);
                              3    4
(%o1)                      2 a  b x  y
@end group
@end example

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{partpol}
@c @deffn {Function} partpol (@var{psym}, @var{lvar})
m4_deffn({Function}, partpol, <<<(@var{psym}, @var{lvar})>>>)
@var{psym} is a symmetric polynomial in the variables of the list
@var{lvar}. This function retturns its partitioned representation.

@c GENERATED FROM THE FOLLOWING
@c partpol (-a*(x + y) + 3*x*y, [x, y]);
@example
@group
(%i1) partpol (-a*(x + y) + 3*x*y, [x, y]);
(%o1)               [[3, 1, 1], [- a, 1, 0]]
@end group
@end example

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{tcontract}
@c @deffn {Function} tcontract (@var{pol}, @var{lvar})
m4_deffn({Function}, tcontract, <<<(@var{pol}, @var{lvar})>>>)
tests if the polynomial @var{pol} is symmetric in the variables of the
list @var{lvar}.  If so, it returns a contracted representation like the
function @code{contract}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{tpartpol}
@c @deffn {Function} tpartpol (@var{pol}, @var{lvar})
m4_deffn({Function}, tpartpol, <<<(@var{pol}, @var{lvar})>>>)
tests if the polynomial @var{pol} is symmetric in the variables of the
list @var{lvar}.  If so, it returns its partitioned representation like
the function @code{partpol}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()




@subsection Groups and orbits

@anchor{direct}
@c @deffn {Function} direct ([@var{p_1}, ..., @var{p_n}], @var{y}, @var{f}, [@var{lvar_1}, ..., @var{lvar_n}])
m4_deffn({Function}, direct, <<<([@var{p_1}, ..., @var{p_n}], @var{y}, @var{f}, [@var{lvar_1}, ..., @var{lvar_n}])>>>)
calculates the direct image (see M. Giusti, D. Lazard et A. Valibouze,
ISSAC 1988, Rome) associated to the function @var{f}, in the lists of
variables @var{lvar_1}, ..., @var{lvar_n}, and in the polynomials
@var{p_1}, ..., @var{p_n} in a variable @var{y}.  The arity of the
function @var{f} is important for the calulation.  Thus, if the
expression for @var{f} does not depend on some variable, it is useless
to include this variable, and not including it will also considerably
reduce the amount of computation.

@c GENERATED FROM THE FOLLOWING
@c direct ([z^2  - e1* z + e2, z^2  - f1* z + f2],
@c               z, b*v + a*u, [[u, v], [a, b]]);
@c ratsimp (%);
@c ratsimp (direct ([z^3-e1*z^2+e2*z-e3,z^2  - f1* z + f2],
@c               z, b*v + a*u, [[u, v], [a, b]]));
@example
@group
(%i1) direct ([z^2  - e1* z + e2, z^2  - f1* z + f2],
              z, b*v + a*u, [[u, v], [a, b]]);
       2
(%o1) y  - e1 f1 y

                                 2            2             2   2
                  - 4 e2 f2 - (e1  - 2 e2) (f1  - 2 f2) + e1  f1
                + -----------------------------------------------
                                         2
@end group
@group
(%i2) ratsimp (%);
              2                2                   2
(%o2)        y  - e1 f1 y + (e1  - 4 e2) f2 + e2 f1
@end group
@group
(%i3) ratsimp (direct ([z^3-e1*z^2+e2*z-e3,z^2  - f1* z + f2],
              z, b*v + a*u, [[u, v], [a, b]]));
       6            5         2                        2    2   4
(%o3) y  - 2 e1 f1 y  + ((2 e1  - 6 e2) f2 + (2 e2 + e1 ) f1 ) y

                          3                               3   3
 + ((9 e3 + 5 e1 e2 - 2 e1 ) f1 f2 + (- 2 e3 - 2 e1 e2) f1 ) y

         2       2        4    2
 + ((9 e2  - 6 e1  e2 + e1 ) f2

                    2       2       2                   2    4
 + (- 9 e1 e3 - 6 e2  + 3 e1  e2) f1  f2 + (2 e1 e3 + e2 ) f1 )

  2          2                      2     3          2
 y  + (((9 e1  - 27 e2) e3 + 3 e1 e2  - e1  e2) f1 f2

                 2            2    3                5
 + ((15 e2 - 2 e1 ) e3 - e1 e2 ) f1  f2 - 2 e2 e3 f1 ) y

           2                   3           3     2   2    3
 + (- 27 e3  + (18 e1 e2 - 4 e1 ) e3 - 4 e2  + e1  e2 ) f2

         2      3                   3    2   2
 + (27 e3  + (e1  - 9 e1 e2) e3 + e2 ) f1  f2

                   2    4        2   6
 + (e1 e2 e3 - 9 e3 ) f1  f2 + e3  f1
@end group
@end example

Finding the polynomial whose roots are the sums @math{a+u} where @math{a}
is a root of @math{z^2 - e_1 z + e_2} and @math{u} is a root of @math{z^2 -
f_1 z + f_2}.

@c GENERATED FROM THE FOLLOWING
@c ratsimp (direct ([z^2 - e1* z + e2, z^2 - f1* z + f2],
@c                           z, a + u, [[u], [a]]));
@example
@group
(%i1) ratsimp (direct ([z^2 - e1* z + e2, z^2 - f1* z + f2],
                          z, a + u, [[u], [a]]));
       4                    3             2
(%o1) y  + (- 2 f1 - 2 e1) y  + (2 f2 + f1  + 3 e1 f1 + 2 e2

     2   2                              2               2
 + e1 ) y  + ((- 2 f1 - 2 e1) f2 - e1 f1  + (- 2 e2 - e1 ) f1

                  2                     2            2
 - 2 e1 e2) y + f2  + (e1 f1 - 2 e2 + e1 ) f2 + e2 f1  + e1 e2 f1

     2
 + e2
@end group
@end example

@code{direct} accepts two flags: @code{elementaires} and
@code{puissances} (default) which allow decomposing the symmetric
polynomials appearing in the calculation into elementary symmetric
functions, or power functions, respectively.

Functions of @code{sym} used in this function:

@code{multi_orbit} (so @code{orbit}), @code{pui_direct}, @code{multi_elem}
(so @code{elem}), @code{multi_pui} (so @code{pui}), @code{pui2ele}, @code{ele2pui}
(if the flag @code{direct} is in @code{puissances}).

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()



@anchor{multi_orbit}
@c @deffn {Function} multi_orbit (@var{P}, [@var{lvar_1}, @var{lvar_2},..., @var{lvar_p}])
m4_deffn({Function}, multi_orbit, <<<(@var{P}, [@var{lvar_1}, @var{lvar_2},..., @var{lvar_p}])>>>)

@var{P} is a polynomial in the set of variables contained in the lists
@var{lvar_1}, @var{lvar_2}, ..., @var{lvar_p}. This function returns the
orbit of the polynomial @var{P} under the action of the product of the
symmetric groups of the sets of variables represented in these @var{p}
lists.

@c GENERATED FROM THE FOLLOWING
@c multi_orbit (a*x + b*y, [[x, y], [a, b]]);
@c multi_orbit (x + y + 2*a, [[x, y], [a, b, c]]);
@example
@group
(%i1) multi_orbit (a*x + b*y, [[x, y], [a, b]]);
(%o1)                [b y + a x, a y + b x]
@end group
@group
(%i2) multi_orbit (x + y + 2*a, [[x, y], [a, b, c]]);
(%o2)        [y + x + 2 c, y + x + 2 b, y + x + 2 a]
@end group
@end example
@noindent
Also see: @code{orbit} for the action of a single symmetric group.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()



@anchor{multsym}
@c @deffn {Function} multsym (@var{ppart_1}, @var{ppart_2}, @var{n})
m4_deffn({Function}, multsym, <<<(@var{ppart_1}, @var{ppart_2}, @var{n})>>>)
returns the product of the two symmetric polynomials in @var{n}
variables by working only modulo the action of the symmetric group of
order @var{n}. The polynomials are in their partitioned form.

Given the 2 symmetric polynomials in @var{x}, @var{y}:  @code{3*(x + y)
+ 2*x*y} and @code{5*(x^2 + y^2)} whose partitioned forms are @code{[[3,
1], [2, 1, 1]]} and @code{[[5, 2]]}, their product will be

@c GENERATED FROM THE FOLLOWING
@c multsym ([[3, 1], [2, 1, 1]], [[5, 2]], 2);
@example
@group
(%i1) multsym ([[3, 1], [2, 1, 1]], [[5, 2]], 2);
(%o1)         [[10, 3, 1], [15, 3, 0], [15, 2, 1]]
@end group
@end example
@noindent
that is @code{10*(x^3*y + y^3*x) + 15*(x^2*y + y^2*x) + 15*(x^3 + y^3)}.

Functions for changing the representations of a symmetric polynomial:

@code{contract}, @code{cont2part}, @code{explose}, @code{part2cont},
@code{partpol}, @code{tcontract}, @code{tpartpol}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{orbit}
@c @deffn {Function} orbit (@var{P}, @var{lvar})
m4_deffn({Function}, orbit, <<<(@var{P}, @var{lvar})>>>)
computes the orbit of the polynomial @var{P} in the variables in the list
@var{lvar} under the action of the symmetric group of the set of
variables in the list @var{lvar}.

@c GENERATED FROM THE FOLLOWING
@c orbit (a*x + b*y, [x, y]);
@c orbit (2*x + x^2, [x, y]);
@example
@group
(%i1) orbit (a*x + b*y, [x, y]);
(%o1)                [a y + b x, b y + a x]
@end group
@group
(%i2) orbit (2*x + x^2, [x, y]);
                        2         2
(%o2)                 [y  + 2 y, x  + 2 x]
@end group
@end example
@noindent
See also @mref{multi_orbit} for the action of a product of symmetric
groups on a polynomial.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{pui_direct}
@c @deffn {Function} pui_direct (@var{orbite}, [@var{lvar_1}, ..., @var{lvar_n}], [@var{d_1}, @var{d_2}, ..., @var{d_n}])
m4_deffn({Function}, pui_direct, <<<(@var{orbite}, [@var{lvar_1}, ..., @var{lvar_n}], [@var{d_1}, @var{d_2}, ..., @var{d_n}])>>>)

Let @var{f} be a polynomial in @var{n} blocks of variables @var{lvar_1},
..., @var{lvar_n}.  Let @var{c_i} be the number of variables in
@var{lvar_i}, and @var{SC} be the product of @var{n} symmetric groups of
degree @var{c_1}, ..., @var{c_n}. This group acts naturally on @var{f}.
The list @var{orbite} is the orbit, denoted @code{@var{SC}(@var{f})}, of
the function @var{f} under the action of @var{SC}. (This list may be
obtained by the function @code{multi_orbit}.)  The @var{di} are integers
s.t.
@iftex
$c_1 \le d_1, c_2 \le d_2, \ldots, c_n \le d_n$.
@end iftex
@ifnottex
@math{c_1 <= d_1, c_2 <= d_2, ..., c_n <= d_n}.
@end ifnottex

@iftex
Let @var{SD} be the product of the symmetric groups @math{S_{d_1} \times
S_{d_2} \times \cdots \times S_{d_n}}.
@end iftex
@ifnottex
Let @var{SD} be the product of the symmetric groups @math{S_[d_1] x
S_[d_2] x ... x S_[d_n]}.
@end ifnottex
The function @code{pui_direct} returns
the first @var{n} power functions of @code{@var{SD}(@var{f})} deduced
from the power functions of @code{@var{SC}(@var{f})}, where @var{n} is
the size of @code{@var{SD}(@var{f})}.

The result is in multi-contracted form w.r.t. @var{SD}, i.e. only one
element is kept per orbit, under the action of @var{SD}.

@c GENERATED FROM THE FOLLOWING
@c l: [[x, y], [a, b]];
@c pui_direct (multi_orbit (a*x + b*y, l), l, [2, 2]);
@c pui_direct (multi_orbit (a*x + b*y, l), l, [3, 2]);
@c pui_direct ([y + x + 2*c, y + x + 2*b, y + x + 2*a], [[x, y], [a, b, c]], [2, 3]);
@example
@group
(%i1) l: [[x, y], [a, b]];
(%o1)                   [[x, y], [a, b]]
@end group
@group
(%i2) pui_direct (multi_orbit (a*x + b*y, l), l, [2, 2]);
                                       2  2
(%o2)               [a x, 4 a b x y + a  x ]
@end group
@group
(%i3) pui_direct (multi_orbit (a*x + b*y, l), l, [3, 2]);
                             2  2     2    2        3  3
(%o3) [2 a x, 4 a b x y + 2 a  x , 3 a  b x  y + 2 a  x , 

    2  2  2  2      3    3        4  4
12 a  b  x  y  + 4 a  b x  y + 2 a  x , 

    3  2  3  2      4    4        5  5
10 a  b  x  y  + 5 a  b x  y + 2 a  x , 

    3  3  3  3       4  2  4  2      5    5        6  6
40 a  b  x  y  + 15 a  b  x  y  + 6 a  b x  y + 2 a  x ]
@end group
@group
(%i4) pui_direct ([y + x + 2*c, y + x + 2*b, y + x + 2*a],
      [[x, y], [a, b, c]], [2, 3]);
                             2              2
(%o4) [3 x + 2 a, 6 x y + 3 x  + 4 a x + 4 a , 

                 2                   3        2       2        3
              9 x  y + 12 a x y + 3 x  + 6 a x  + 12 a  x + 8 a ]
@end group
@end example
@c THIS NEXT FUNCTION CALL TAKES A VERY LONG TIME (SEVERAL MINUTES)
@c SO LEAVE IT OUT TIL PROCESSORS GET A LITTLE FASTER ...
@c pui_direct ([y + x + 2*c, y + x + 2*b, y + x + 2*a], [[x, y], [a, b, c]], [3, 4]);

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()






@subsection Partitions
@anchor{kostka}
@c @deffn {Function} kostka (@var{part_1}, @var{part_2})
m4_deffn({Function}, kostka, <<<(@var{part_1}, @var{part_2})>>>)
written by P. Esperet, calculates the Kostka number of the partition
@var{part_1} and @var{part_2}.

@c GENERATED FROM THE FOLLOWING
@c kostka ([3, 3, 3], [2, 2, 2, 1, 1, 1]);
@example
@group
(%i1) kostka ([3, 3, 3], [2, 2, 2, 1, 1, 1]);
(%o1)                           6
@end group
@end example

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{lgtreillis}
@c @deffn {Function} lgtreillis (@var{n}, @var{m})
m4_deffn({Function}, lgtreillis, <<<(@var{n}, @var{m})>>>)
returns the list of partitions of weight @var{n} and length @var{m}.

@c GENERATED FROM THE FOLLOWING
@c lgtreillis (4, 2);
@example
@group
(%i1) lgtreillis (4, 2);
(%o1)                   [[3, 1], [2, 2]]
@end group
@end example
@noindent
Also see: @mrefcomma{ltreillis} @mref{treillis} and @mrefdot{treinat}

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{ltreillis}
@c @deffn {Function} ltreillis (@var{n}, @var{m})
m4_deffn({Function}, ltreillis, <<<(@var{n}, @var{m})>>>)
returns the list of partitions of weight @var{n} and length less than or
equal to @var{m}.

@c GENERATED FROM THE FOLLOWING
@c ltreillis (4, 2);
@example
@group
(%i1) ltreillis (4, 2);
(%o1)               [[4, 0], [3, 1], [2, 2]]
@end group
@end example
@noindent
Also see: @mrefcomma{lgtreillis} @mref{treillis} and @mrefdot{treinat}

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{treillis}
@c @deffn {Function} treillis (@var{n})
m4_deffn({Function}, treillis, <<<(@var{n})>>>)
returns all partitions of weight @var{n}.

@c GENERATED FROM THE FOLLOWING
@c treillis (4);
@example
@group
(%i1) treillis (4);
(%o1)    [[4], [3, 1], [2, 2], [2, 1, 1], [1, 1, 1, 1]]
@end group
@end example

See also: @mrefcomma{lgtreillis} @mref{ltreillis} and @mrefdot{treinat}

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{treinat}
@c @deffn {Function} treinat (@var{part})
m4_deffn({Function}, treinat, <<<(@var{part})>>>)
retruns the list of partitions inferior to the partition @var{part} w.r.t.
the natural order.

@c GENERATED FROM THE FOLLOWING
@c treinat ([5]);
@c treinat ([1, 1, 1, 1, 1]);
@c treinat ([3, 2]);
@example
@group
(%i1) treinat ([5]);
(%o1)                         [[5]]
@end group
@group
(%i2) treinat ([1, 1, 1, 1, 1]);
(%o2) [[5], [4, 1], [3, 2], [3, 1, 1], [2, 2, 1], [2, 1, 1, 1], 

                                                 [1, 1, 1, 1, 1]]
@end group
@group
(%i3) treinat ([3, 2]);
(%o3)                 [[5], [4, 1], [3, 2]]
@end group
@end example

See also: @mrefcomma{lgtreillis} @mref{ltreillis} and @mrefdot{treillis}

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()





@subsection Polynomials and their roots

@anchor{ele2polynome}
@c @deffn {Function} ele2polynome (@var{L}, @var{z})
m4_deffn({Function}, ele2polynome, <<<(@var{L}, @var{z})>>>)
returns the polynomial in @var{z} s.t. the elementary symmetric
functions of its roots are in the list @code{@var{L} = [@var{n},
@var{e_1}, ..., @var{e_n}]}, where @var{n} is the degree of the
polynomial and @var{e_i} the @var{i}-th elementary symmetric function.

@c GENERATED FROM THE FOLLOWING
@c ele2polynome ([2, e1, e2], z);
@c polynome2ele (x^7 - 14*x^5 + 56*x^3  - 56*x + 22, x);
@c ele2polynome ([7, 0, -14, 0, 56, 0, -56, -22], x);
@example
@group
(%i1) ele2polynome ([2, e1, e2], z);
                          2
(%o1)                    z  - e1 z + e2
@end group
@group
(%i2) polynome2ele (x^7 - 14*x^5 + 56*x^3  - 56*x + 22, x);
(%o2)          [7, 0, - 14, 0, 56, 0, - 56, - 22]
@end group
@group
(%i3) ele2polynome ([7, 0, -14, 0, 56, 0, -56, -22], x);
                  7       5       3
(%o3)            x  - 14 x  + 56 x  - 56 x + 22
@end group
@end example
@noindent
The inverse: @code{polynome2ele (@var{P}, @var{z})}.

Also see:
@code{polynome2ele}, @code{pui2polynome}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{polynome2ele}
@c @deffn {Function} polynome2ele (@var{P}, @var{x})
m4_deffn({Function}, polynome2ele, <<<(@var{P}, @var{x})>>>)
gives the list @code{@var{l} = [@var{n}, @var{e_1}, ..., @var{e_n}]}
where @var{n} is the degree of the polynomial @var{P} in the variable
@var{x} and @var{e_i} is the @var{i}-the elementary symmetric function
of the roots of @var{P}.

@c GENERATED FROM THE FOLLOWING
@c polynome2ele (x^7 - 14*x^5 + 56*x^3 - 56*x + 22, x);
@c ele2polynome ([7, 0, -14, 0, 56, 0, -56, -22], x);
@example
@group
(%i1) polynome2ele (x^7 - 14*x^5 + 56*x^3 - 56*x + 22, x);
(%o1)          [7, 0, - 14, 0, 56, 0, - 56, - 22]
@end group
@group
(%i2) ele2polynome ([7, 0, -14, 0, 56, 0, -56, -22], x);
                  7       5       3
(%o2)            x  - 14 x  + 56 x  - 56 x + 22
@end group
@end example
@noindent
The inverse: @code{ele2polynome (@var{l}, @var{x})}

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{prodrac}
@c @deffn {Function} prodrac (@var{L}, @var{k})
m4_deffn({Function}, prodrac, <<<(@var{L}, @var{k})>>>)
@var{L} is a list containing the elementary symmetric functions 
on a set @var{A}. @code{prodrac} returns the polynomial whose roots
are the @var{k} by @var{k} products of the elements of @var{A}.

Also see @code{somrac}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{pui2polynome}
@c @deffn {Function} pui2polynome (@var{x}, @var{lpui})
m4_deffn({Function}, pui2polynome, <<<(@var{x}, @var{lpui})>>>)
calculates the polynomial in @var{x} whose power functions of the roots
are given in the list @var{lpui}.

@c GENERATED FROM THE FOLLOWING
@c polynome2ele (x^3 - 4*x^2 + 5*x - 1, x);
@c ele2pui (3, %);
@c pui2polynome (x, %);
@example
@group
(%i1) pui;
(%o1)                           1
@end group
@group
(%i2) kill(labels);
(%o0)                         done
@end group
@group
(%i1) polynome2ele (x^3 - 4*x^2 + 5*x - 1, x);
(%o1)                     [3, 4, 5, 1]
@end group
@group
(%i2) ele2pui (3, %);
(%o2)                     [3, 4, 6, 7]
@end group
@group
(%i3) pui2polynome (x, %);
                        3      2
(%o3)                  x  - 4 x  + 5 x - 1
@end group
@end example
@noindent
See also:
@mrefcomma{polynome2ele} @mrefdot{ele2polynome}

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{somrac}
@c @deffn {Function} somrac (@var{L}, @var{k})
m4_deffn({Function}, somrac, <<<(@var{L}, @var{k})>>>)
The list @var{L} contains elementary symmetric functions of a polynomial
@var{P} . The function computes the polynomial whose roots are the 
@var{k} by @var{k} distinct sums of the roots of @var{P}. 

Also see @code{prodrac}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()





@subsection Resolvents

@anchor{resolvante}
@c @deffn {Function} resolvante (@var{P}, @var{x}, @var{f}, [@var{x_1},..., @var{x_d}]) 
m4_deffn({Function}, resolvante, <<<(@var{P}, @var{x}, @var{f}, [@var{x_1},..., @var{x_d}]) >>>)
calculates the resolvent of the polynomial @var{P} in @var{x} of degree
@var{n} >= @var{d} by the function @var{f} expressed in the variables
@var{x_1}, ..., @var{x_d}.  For efficiency of computation it is
important to not include in the list @code{[@var{x_1}, ..., @var{x_d}]}
variables which do not appear in the transformation function @var{f}.

To increase the efficiency of the computation one may set flags in
@code{resolvante} so as to use appropriate algorithms:

If the function @var{f} is unitary:
@itemize @bullet
@item
A polynomial in a single variable,
@item
  linear,
@item
  alternating,
@item
  a sum,
@item
  symmetric,
@item
  a product,
@item
the function of the Cayley resolvent (usable up to degree 5)

@c WHAT IS THIS ILLUSTRATING EXACTLY ??
@example
@group
(x1*x2 + x2*x3 + x3*x4 + x4*x5 + x5*x1 -
     (x1*x3 + x3*x5 + x5*x2 + x2*x4 + x4*x1))^2
@end group
@end example

general,
@end itemize
the flag of @code{resolvante} may be, respectively:
@itemize @bullet
@item
  unitaire,
@item
  lineaire,
@item
  alternee,
@item
  somme,
@item
  produit,
@item
  cayley,
@item
  generale.
@end itemize

@c GENERATED FROM THE FOLLOWING
@c resolvante: unitaire$
@c resolvante (x^7 - 14*x^5 + 56*x^3 - 56*x + 22, x, x^3 - 1, [x]);
@c resolvante: lineaire$
@c resolvante (x^4 - 1, x, x1 + 2*x2 + 3*x3, [x1, x2, x3]);
@c resolvante: general$
@c resolvante (x^4 - 1, x, x1 + 2*x2 + 3*x3, [x1, x2, x3]);
@c resolvante (x^4 - 1, x, x1 + 2*x2 + 3*x3, [x1, x2, x3, x4]);
@c direct ([x^4 - 1], x, x1 + 2*x2 + 3*x3, [[x1, x2, x3]]);
@c resolvante :lineaire$
@c resolvante (x^4 - 1, x, x1 + x2 + x3, [x1, x2, x3]);
@c resolvante: symetrique$
@c resolvante (x^4 - 1, x, x1 + x2 + x3, [x1, x2, x3]);
@c resolvante (x^4 + x + 1, x, x1 - x2, [x1, x2]);
@c resolvante: alternee$
@c resolvante (x^4 + x + 1, x, x1 - x2, [x1, x2]);
@c resolvante: produit$
@c resolvante (x^7 - 7*x + 3, x, x1*x2*x3, [x1, x2, x3]);
@c resolvante: symetrique$
@c resolvante (x^7 - 7*x + 3, x, x1*x2*x3, [x1, x2, x3]);
@c resolvante: cayley$
@c resolvante (x^5 - 4*x^2 + x + 1, x, a, []);
@example
(%i1) resolvante: unitaire$
@group
(%i2) resolvante (x^7 - 14*x^5 + 56*x^3 - 56*x + 22, x, x^3 - 1,
      [x]);

" resolvante unitaire " [7, 0, 28, 0, 168, 0, 1120, - 154, 7840,
                         - 2772, 56448, - 33880, 

413952, - 352352, 3076668, - 3363360, 23114112, - 30494464, 

175230832, - 267412992, 1338886528, - 2292126760] 
  3       6      3       9      6      3
[x  - 1, x  - 2 x  + 1, x  - 3 x  + 3 x  - 1, 

 12      9      6      3       15      12       9       6      3
x   - 4 x  + 6 x  - 4 x  + 1, x   - 5 x   + 10 x  - 10 x  + 5 x

       18      15       12       9       6      3
 - 1, x   - 6 x   + 15 x   - 20 x  + 15 x  - 6 x  + 1, 

 21      18       15       12       9       6      3
x   - 7 x   + 21 x   - 35 x   + 35 x  - 21 x  + 7 x  - 1] 
[- 7, 1127, - 6139, 431767, - 5472047, 201692519, - 3603982011] 
       7      6        5         4          3           2
(%o2) y  + 7 y  - 539 y  - 1841 y  + 51443 y  + 315133 y

                                              + 376999 y + 125253
@end group
(%i3) resolvante: lineaire$
@group
(%i4) resolvante (x^4 - 1, x, x1 + 2*x2 + 3*x3, [x1, x2, x3]);

" resolvante lineaire " 
       24       20         16            12             8
(%o4) y   + 80 y   + 7520 y   + 1107200 y   + 49475840 y

                                                    4
                                       + 344489984 y  + 655360000
@end group
(%i5) resolvante: general$
@group
(%i6) resolvante (x^4 - 1, x, x1 + 2*x2 + 3*x3, [x1, x2, x3]);

" resolvante generale " 
       24       20         16            12             8
(%o6) y   + 80 y   + 7520 y   + 1107200 y   + 49475840 y

                                                    4
                                       + 344489984 y  + 655360000
@end group
@group
(%i7) resolvante (x^4 - 1, x, x1 + 2*x2 + 3*x3, [x1, x2, x3, x4]);

" resolvante generale " 
       24       20         16            12             8
(%o7) y   + 80 y   + 7520 y   + 1107200 y   + 49475840 y

                                                    4
                                       + 344489984 y  + 655360000
@end group
@group
(%i8) direct ([x^4 - 1], x, x1 + 2*x2 + 3*x3, [[x1, x2, x3]]);
       24       20         16            12             8
(%o8) y   + 80 y   + 7520 y   + 1107200 y   + 49475840 y

                                                    4
                                       + 344489984 y  + 655360000
@end group
(%i9) resolvante :lineaire$
@group
(%i10) resolvante (x^4 - 1, x, x1 + x2 + x3, [x1, x2, x3]);

" resolvante lineaire " 
                              4
(%o10)                       y  - 1
@end group
(%i11) resolvante: symetrique$
@group
(%i12) resolvante (x^4 - 1, x, x1 + x2 + x3, [x1, x2, x3]);

" resolvante symetrique " 
                              4
(%o12)                       y  - 1
@end group
@group
(%i13) resolvante (x^4 + x + 1, x, x1 - x2, [x1, x2]);

" resolvante symetrique " 
                           6      2
(%o13)                    y  - 4 y  - 1
@end group
(%i14) resolvante: alternee$
@group
(%i15) resolvante (x^4 + x + 1, x, x1 - x2, [x1, x2]);

" resolvante alternee " 
            12      8       6        4        2
(%o15)     y   + 8 y  + 26 y  - 112 y  + 216 y  + 229
@end group
(%i16) resolvante: produit$
@group
(%i17) resolvante (x^7 - 7*x + 3, x, x1*x2*x3, [x1, x2, x3]);

" resolvante produit "
        35      33         29        28         27        26
(%o17) y   - 7 y   - 1029 y   + 135 y   + 7203 y   - 756 y

         24           23          22            21           20
 + 1323 y   + 352947 y   - 46305 y   - 2463339 y   + 324135 y

          19           18             17              15
 - 30618 y   - 453789 y   - 40246444 y   + 282225202 y

             14              12             11            10
 - 44274492 y   + 155098503 y   + 12252303 y   + 2893401 y

              9            8            7             6
 - 171532242 y  + 6751269 y  + 2657205 y  - 94517766 y

            5             3
 - 3720087 y  + 26040609 y  + 14348907
@end group
(%i18) resolvante: symetrique$
@group
(%i19) resolvante (x^7 - 7*x + 3, x, x1*x2*x3, [x1, x2, x3]);

" resolvante symetrique " 
        35      33         29        28         27        26
(%o19) y   - 7 y   - 1029 y   + 135 y   + 7203 y   - 756 y

         24           23          22            21           20
 + 1323 y   + 352947 y   - 46305 y   - 2463339 y   + 324135 y

          19           18             17              15
 - 30618 y   - 453789 y   - 40246444 y   + 282225202 y

             14              12             11            10
 - 44274492 y   + 155098503 y   + 12252303 y   + 2893401 y

              9            8            7             6
 - 171532242 y  + 6751269 y  + 2657205 y  - 94517766 y

            5             3
 - 3720087 y  + 26040609 y  + 14348907
@end group
(%i20) resolvante: cayley$
@group
(%i21) resolvante (x^5 - 4*x^2 + x + 1, x, a, []);

" resolvante de Cayley "
        6       5         4          3            2
(%o21) x  - 40 x  + 4080 x  - 92928 x  + 3772160 x  + 37880832 x

                                                       + 93392896
@end group
@end example

For the Cayley resolvent, the 2 last arguments are neutral and the input
polynomial must necessarily be of degree 5.

See also:
@flushleft
@mrefcomma{resolvante_bipartite} @mrefcomma{resolvante_produit_sym}
@mrefcomma{resolvante_unitaire} @mrefcomma{resolvante_alternee1} @mrefcomma{resolvante_klein} 
@mrefcomma{resolvante_klein3} @mrefcomma{resolvante_vierer} @mrefdot{resolvante_diedrale}
@end flushleft

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{resolvante_alternee1}
@c @deffn {Function} resolvante_alternee1 (@var{P}, @var{x})
m4_deffn({Function}, resolvante_alternee1, <<<(@var{P}, @var{x})>>>)
calculates the transformation
@code{@var{P}(@var{x})} of degree @var{n} by the function
@iftex
@math{\prod_{1\leq i<j\leq n-1} (x_i-x_j)}.
@end iftex
@ifnottex
@math{product(x_i - x_j, 1 <= i < j <= n - 1)}.
@end ifnottex

See also:
@flushleft
@mrefcomma{resolvante_produit_sym} @mrefcomma{resolvante_unitaire}
@mrefcomma{resolvante}  @mrefcomma{resolvante_klein} @mrefcomma{resolvante_klein3}
@mrefcomma{resolvante_vierer} @mrefcomma{resolvante_diedrale} @mrefdot{resolvante_bipartite}
@end flushleft

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{resolvante_bipartite}
@c @deffn {Function} resolvante_bipartite (@var{P}, @var{x})
m4_deffn({Function}, resolvante_bipartite, <<<(@var{P}, @var{x})>>>)
calculates the transformation of
@code{@var{P}(@var{x})} of even degree @var{n} by the function 
@iftex
@math{x_1 x_2 \cdots x_{n/2} + x_{n/2+1}\cdots x_n}.
@end iftex
@c UNFORTUNATELY TEXINFO DOES NOT HAVE A NOTION OF "@ELSE"
@c SO IT IS NECESSARY TO REPEAT THE FOLLOWING NON-TEX STUFF FOR INFO AND FOR HTML ... SIGH
@ifnottex
@math{x_1 x_2 ... x_[n/2] + x_[n/2 + 1] ... x_n}.
@end ifnottex

@c GENERATED FROM THE FOLLOWING
@c resolvante_bipartite (x^6 + 108, x);
@example
@group
(%i1) resolvante_bipartite (x^6 + 108, x);
              10        8           6             4
(%o1)        y   - 972 y  + 314928 y  - 34012224 y
@end group
@end example

See also:
@flushleft
@mrefcomma{resolvante_produit_sym} @mrefcomma{resolvante_unitaire}
@mrefcomma{resolvante} @mrefcomma{resolvante_klein} @mrefcomma{resolvante_klein3}
@mrefcomma{resolvante_vierer} @mrefcomma{resolvante_diedrale} @mrefdot{resolvante_alternee1}
@end flushleft

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{resolvante_diedrale}
@c @deffn {Function} resolvante_diedrale (@var{P}, @var{x})
m4_deffn({Function}, resolvante_diedrale, <<<(@var{P}, @var{x})>>>)
calculates the transformation of @code{@var{P}(@var{x})} by the function
@code{@var{x_1} @var{x_2} + @var{x_3} @var{x_4}}.

@c GENERATED FROM THE FOLLOWING
@c resolvante_diedrale (x^5 - 3*x^4 + 1, x);
@example
@group
(%i1) resolvante_diedrale (x^5 - 3*x^4 + 1, x);
       15       12       11       10        9         8         7
(%o1) x   - 21 x   - 81 x   - 21 x   + 207 x  + 1134 x  + 2331 x

        6         5          4          3          2
 - 945 x  - 4970 x  - 18333 x  - 29079 x  - 20745 x  - 25326 x

 - 697
@end group
@end example

See also:
@flushleft
@mrefcomma{resolvante_produit_sym} @mrefcomma{resolvante_unitaire}
@mrefcomma{resolvante_alternee1} @mrefcomma{resolvante_klein} @mrefcomma{resolvante_klein3}
@mrefcomma{resolvante_vierer} @mrefdot{resolvante}
@end flushleft

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{resolvante_klein}
@c @deffn {Function} resolvante_klein (@var{P}, @var{x})
m4_deffn({Function}, resolvante_klein, <<<(@var{P}, @var{x})>>>)
calculates the transformation of @code{@var{P}(@var{x})} by the function
@code{@var{x_1} @var{x_2} @var{x_4} + @var{x_4}}.

See also:
@flushleft
@mrefcomma{resolvante_produit_sym} @mrefcomma{resolvante_unitaire}
@mrefcomma{resolvante_alternee1} @mrefcomma{resolvante} @mrefcomma{resolvante_klein3}
@mrefcomma{resolvante_vierer} @mrefdot{resolvante_diedrale}
@end flushleft

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{resolvante_klein3}
@c @deffn {Function} resolvante_klein3 (@var{P}, @var{x})
m4_deffn({Function}, resolvante_klein3, <<<(@var{P}, @var{x})>>>)
calculates the transformation of @code{@var{P}(@var{x})} by the function
@code{@var{x_1} @var{x_2} @var{x_4} + @var{x_4}}.

See also:
@flushleft
@mrefcomma{resolvante_produit_sym} @mrefcomma{resolvante_unitaire}
@mrefcomma{resolvante_alternee1} @mrefcomma{resolvante_klein} @mrefcomma{resolvante}
@mrefcomma{resolvante_vierer} @mrefdot{resolvante_diedrale}
@end flushleft

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{resolvante_produit_sym}
@c @deffn {Function} resolvante_produit_sym (@var{P}, @var{x})
m4_deffn({Function}, resolvante_produit_sym, <<<(@var{P}, @var{x})>>>)
calculates the list of all product resolvents of the polynomial
@code{@var{P}(@var{x})}.

@c GENERATED FROM THE FOLLOWING
@c resolvante_produit_sym (x^5 + 3*x^4 + 2*x - 1, x);
@c resolvante: produit$
@c resolvante (x^5 + 3*x^4 + 2*x - 1, x, a*b*c, [a, b, c]);
@example
@group
(%i1) resolvante_produit_sym (x^5 + 3*x^4 + 2*x - 1, x);
        5      4             10      8       7       6       5
(%o1) [y  + 3 y  + 2 y - 1, y   - 2 y  - 21 y  - 31 y  - 14 y

    4       3      2       10      8       7    6       5       4
 - y  + 14 y  + 3 y  + 1, y   + 3 y  + 14 y  - y  - 14 y  - 31 y

       3      2       5      4
 - 21 y  - 2 y  + 1, y  - 2 y  - 3 y - 1, y - 1]
@end group
(%i2) resolvante: produit$
@group
(%i3) resolvante (x^5 + 3*x^4 + 2*x - 1, x, a*b*c, [a, b, c]);

" resolvante produit "
       10      8       7    6        5       4       3     2
(%o3) y   + 3 y  + 14 y  - y  - 14 y  - 31 y  - 21 y  - 2 y  + 1
@end group
@end example
@c INPUT %i3 TICKLES A MINOR BUG IN resolvante: 
@c " resolvante produit " IS PRINTED FROM SOMEWHERE IN THE BOWELS OF resolvante
@c AND IT GOOFS UP THE DISPLAY OF THE EXPONENTS OF %o3 -- I THREW IN A LINE BREAK TO ADJUST

See also:
@flushleft
@mrefcomma{resolvante} @mrefcomma{resolvante_unitaire}
@mrefcomma{resolvante_alternee1} @mrefcomma{resolvante_klein}
@mrefcomma{resolvante_klein3} @mrefcomma{resolvante_vierer}
@mrefdot{resolvante_diedrale}
@end flushleft

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@anchor{resolvante_unitaire}
@c @deffn {Function} resolvante_unitaire (@var{P}, @var{Q}, @var{x})
m4_deffn({Function}, resolvante_unitaire, <<<(@var{P}, @var{Q}, @var{x})>>>)
computes the resolvent of the polynomial @code{@var{P}(@var{x})} by the
polynomial @code{@var{Q}(@var{x})}. 

See also:
@flushleft
@mrefcomma{resolvante_produit_sym} @mrefcomma{resolvante}
@mrefcomma{resolvante_alternee1} @mrefcomma{resolvante_klein} @mrefcomma{resolvante_klein3}
@mrefcomma{resolvante_vierer} @mrefdot{resolvante_diedrale}
@end flushleft

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{resolvante_vierer}
@c @deffn {Function} resolvante_vierer (@var{P}, @var{x})
m4_deffn({Function}, resolvante_vierer, <<<(@var{P}, @var{x})>>>)
computes the transformation of
@code{@var{P}(@var{x})} by the function @code{@var{x_1} @var{x_2} -
@var{x_3} @var{x_4}}.

See also:
@flushleft
@mrefcomma{resolvante_produit_sym} @mrefcomma{resolvante_unitaire}
@mrefcomma{resolvante_alternee1} @mrefcomma{resolvante_klein} @mrefcomma{resolvante_klein3}
@mrefcomma{resolvante} @mrefdot{resolvante_diedrale}
@end flushleft

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()




@subsection Miscellaneous

@anchor{multinomial}
@c @deffn {Function} multinomial (@var{r}, @var{part})
m4_deffn({Function}, multinomial, <<<(@var{r}, @var{part})>>>)
where @var{r} is the weight of the partition @var{part}.  This function
returns the associate multinomial coefficient: if the parts of
@var{part} are @var{i_1}, @var{i_2}, ..., @var{i_k}, the result is
@code{@var{r}!/(@var{i_1}! @var{i_2}! ... @var{i_k}!)}.

@c @opencatbox
@c @category{Package sym}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_setcat(Package sym, Lists)
@anchor{permut}
@c @deffn {Function} permut (@var{L})
m4_deffn({Function}, permut, <<<(@var{L})>>>)
returns the list of permutations of the list @var{L}.

@c @opencatbox
@c @category{Package sym}
@c @category{Lists}
@c @closecatbox
@c @end deffn
m4_end_deffn()