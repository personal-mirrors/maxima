@c -*- Mode: texinfo -*-
@menu
* Introduction to ctensor::
* Functions and Variables for ctensor::
@end menu

@node Introduction to ctensor, Functions and Variables for ctensor, ctensor, ctensor
@section Introduction to ctensor

@code{ctensor} is a component tensor manipulation package.  To use the @code{ctensor}
package, type @code{load("ctensor")}.
To begin an interactive session with @code{ctensor}, type @code{csetup()}.  You are
first asked to specify the dimension of the manifold. If the dimension
is 2, 3 or 4 then the list of coordinates defaults to @code{[x,y]}, @code{[x,y,z]}
or @code{[x,y,z,t]} respectively.
These names may be changed by assigning a new list of coordinates to
the variable @code{ct_coords} (described below) and the user is queried about
this. Care must be taken to avoid the coordinate names conflicting
with other object definitions.

Next, the user enters the metric either directly or from a file by
specifying its ordinal position.
@c NO SUCH FILE !
@c As an example of a file of common metrics, see @code{share/tensor/metrics.mac}.
The metric is stored in the matrix
@code{lg}. Finally, the metric inverse is computed and stored in the matrix
@code{ug}. One has the option of carrying out all calculations in a power
series.

A sample protocol is begun below for the static, spherically symmetric
metric (standard coordinates) which will be applied to the problem of
deriving Einstein's vacuum equations (which lead to the Schwarzschild
solution) as an example. Many of the functions in @code{ctensor} will be
displayed for the standard metric as examples.

@example
(%i1) load("ctensor");
(%o1)      /share/tensor/ctensor.mac
(%i2) csetup();
Enter the dimension of the coordinate system:
4;
Do you wish to change the coordinate names?
n;
Do you want to
1. Enter a new metric?

2. Enter a metric from a file?

3. Approximate a metric with a Taylor series?
1;

Is the matrix  1. Diagonal  2. Symmetric  3. Antisymmetric  4. General
Answer 1, 2, 3 or 4
1;
Row 1 Column 1:
a;
Row 2 Column 2:
x^2;
Row 3 Column 3:
x^2*sin(y)^2;
Row 4 Column 4:
-d;

Matrix entered.
Enter functional dependencies with the DEPENDS function or 'N' if none
depends([a,d],x);
Do you wish to see the metric?
y;
                          [ a  0       0        0  ]
                          [                        ]
                          [     2                  ]
                          [ 0  x       0        0  ]
                          [                        ]
                          [         2    2         ]
                          [ 0  0   x  sin (y)   0  ]
                          [                        ]
                          [ 0  0       0       - d ]
(%o2)                                done
(%i3) christof(mcs);
                                            a
                                             x
(%t3)                          mcs        = ---
                                  1, 1, 1   2 a

                                             1
(%t4)                           mcs        = -
                                   1, 2, 2   x

                                             1
(%t5)                           mcs        = -
                                   1, 3, 3   x

                                            d
                                             x
(%t6)                          mcs        = ---
                                  1, 4, 4   2 d

                                              x
(%t7)                          mcs        = - -
                                  2, 2, 1     a

                                           cos(y)
(%t8)                         mcs        = ------
                                 2, 3, 3   sin(y)

                                               2
                                          x sin (y)
(%t9)                      mcs        = - ---------
                              3, 3, 1         a

(%t10)                   mcs        = - cos(y) sin(y)
                            3, 3, 2

                                            d
                                             x
(%t11)                         mcs        = ---
                                  4, 4, 1   2 a
(%o11)                               done

@end example

@opencatbox
@category{Tensors}
@category{Share packages}
@category{Package ctensor}
@closecatbox

@c end concepts ctensor
@node Functions and Variables for ctensor,  , Introduction to ctensor, ctensor

@section Functions and Variables for ctensor

@subsection Initialization and setup
m4_setcat(Package ctensor)
@anchor{csetup}
@c @deffn {Function} csetup ()
m4_deffn({Function}, csetup, <<<()>>>)
A function in the @code{ctensor} (component tensor) package
which initializes the package and allows the user to enter a metric
interactively. See @code{ctensor} for more details.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{cmetric}
@deffn {Function} cmetric @
@fname{cmetric} (@var{dis}) @
@fname{cmetric} ()

A function in the @code{ctensor} (component tensor) package
that computes the metric inverse and sets up the package for
further calculations.

If @code{cframe_flag} is @code{false}, the function computes the inverse metric
@code{ug} from the (user-defined) matrix @code{lg}. The metric determinant is
also computed and stored in the variable @code{gdet}. Furthermore, the
package determines if the metric is diagonal and sets the value
of @code{diagmetric} accordingly. If the optional argument @var{dis}
is present and not equal to @code{false}, the user is prompted to see
the metric inverse.

If @code{cframe_flag} is @code{true}, the function expects that the values of
@code{fri} (the inverse frame matrix) and @code{lfg} (the frame metric) are
defined. From these, the frame matrix @code{fr} and the inverse frame
metric @code{ufg} are computed.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{ct}
@deffn {Function} ct_coordsys @
@fname{ct_coordsys} (@var{coordinate_system}, @var{extra_arg}) @
@fname{ct_coordsys} (@var{coordinate_system})

Sets up a predefined coordinate system and metric. The argument
@var{coordinate_system} can be one of the following symbols:

@example

 SYMBOL             Dim Coordinates     Description/comments
 ------------------------------------------------------------------
 cartesian2d           2  [x,y]             Cartesian 2D coordinate
                                            system
 polar                 2  [r,phi]           Polar coordinate system
 elliptic              2  [u,v]             Elliptic coord. system
 confocalelliptic      2  [u,v]             Confocal elliptic
                                            coordinates
 bipolar               2  [u,v]             Bipolar coord. system
 parabolic             2  [u,v]             Parabolic coord. system
 cartesian3d           3  [x,y,z]           Cartesian 3D coordinate
                                            system
 polarcylindrical      3  [r,theta,z]       Polar 2D with
                                            cylindrical z
 ellipticcylindrical   3  [u,v,z]           Elliptic 2D with
                                            cylindrical z
 confocalellipsoidal   3  [u,v,w]           Confocal ellipsoidal
 bipolarcylindrical    3  [u,v,z]           Bipolar 2D with
                                            cylindrical z
 paraboliccylindrical  3  [u,v,z]           Parabolic 2D with
                                            cylindrical z
 paraboloidal          3  [u,v,phi]         Paraboloidal coords.
 conical               3  [u,v,w]           Conical coordinates
 toroidal              3  [phi,u,v]         Toroidal coordinates
 spherical             3  [r,theta,phi]     Spherical coord. system
 oblatespheroidal      3  [u,v,phi]         Oblate spheroidal
                                            coordinates
 oblatespheroidalsqrt  3  [u,v,phi]
 prolatespheroidal     3  [u,v,phi]         Prolate spheroidal
                                            coordinates
 prolatespheroidalsqrt 3  [u,v,phi]
 ellipsoidal           3  [r,theta,phi]     Ellipsoidal coordinates
 cartesian4d           4  [x,y,z,t]         Cartesian 4D coordinate
                                            system
 spherical4d           4  [r,theta,eta,phi] Spherical 4D coordinate
                                            system
 exteriorschwarzschild 4  [t,r,theta,phi]   Schwarzschild metric
 interiorschwarzschild 4  [t,z,u,v]         Interior Schwarzschild
                                            metric
 kerr_newman           4  [t,r,theta,phi]   Charged axially
                                            symmetric metric
@end example

@code{coordinate_system} can also be a list of transformation functions,
followed by a list containing the coordinate variables. For instance,
you can specify a spherical metric as follows:

@c ===beg===
@c load("ctensor");
@c ct_coordsys([r*cos(theta)*cos(phi),r*cos(theta)*sin(phi),
@c             r*sin(theta),[r,theta,phi]]);
@c lg:trigsimp(lg);
@c ct_coords;
@c dim;
@c ===end===
@example

(%i1) load("ctensor");
(%o1)       /share/tensor/ctensor.mac
(%i2) ct_coordsys([r*cos(theta)*cos(phi),r*cos(theta)*sin(phi),
      r*sin(theta),[r,theta,phi]]);
(%o2)                                done
(%i3) lg:trigsimp(lg);
                           [ 1  0         0        ]
                           [                       ]
                           [     2                 ]
(%o3)                      [ 0  r         0        ]
                           [                       ]
                           [         2    2        ]
                           [ 0  0   r  cos (theta) ]
(%i4) ct_coords;
(%o4)                           [r, theta, phi]
(%i5) dim;
(%o5)                                  3

@end example

Transformation functions can also be used when @code{cframe_flag} is @code{true}:

@c ===beg===
@c load("ctensor");
@c cframe_flag:true;
@c ct_coordsys([r*cos(theta)*cos(phi),r*cos(theta)*sin(phi),
@c             r*sin(theta),[r,theta,phi]]);
@c fri;
@c cmetric();
@c lg:trigsimp(lg);
@c ===end===
@example

(%i1) load("ctensor");
(%o1)       /share/tensor/ctensor.mac
(%i2) cframe_flag:true;
(%o2)                                true
(%i3) ct_coordsys([r*cos(theta)*cos(phi),r*cos(theta)*sin(phi),
      r*sin(theta),[r,theta,phi]]);
(%o3)                                done
(%i4) fri;
(%o4)
 [cos(phi)cos(theta) -cos(phi) r sin(theta) -sin(phi) r cos(theta)]
 [                                                                ]
 [sin(phi)cos(theta) -sin(phi) r sin(theta)  cos(phi) r cos(theta)]
 [                                                                ]
 [    sin(theta)           r cos(theta)                0          ]

(%i5) cmetric();
(%o5)                                false
(%i6) lg:trigsimp(lg);
                           [ 1  0         0        ]
                           [                       ]
                           [     2                 ]
(%o6)                      [ 0  r         0        ]
                           [                       ]
                           [         2    2        ]
                           [ 0  0   r  cos (theta) ]

@end example

The optional argument @var{extra_arg} can be any one of the following:
@c LOOKING AT share/tensor/ctensor.mac CIRCA LINE 837, misner IS RECOGNIZED ALSO; WHAT EFFECT DOES IT HAVE ??

@code{cylindrical} tells @code{ct_coordsys} to attach an additional cylindrical coordinate.

@code{minkowski} tells @code{ct_coordsys} to attach an additional coordinate with negative metric signature.

@code{all} tells @code{ct_coordsys} to call @code{cmetric} and @code{christof(false)} after setting up the metric.

@c GLOBAL VARIABLE verbose IS USED IN ctensor.mac IN JUST THIS ONE CONTEXT
If the global variable @code{verbose} is set to @code{true}, @code{ct_coordsys} displays the values of @code{dim}, @code{ct_coords}, and either @code{lg} or @code{lfg} and @code{fri}, depending on the value of @code{cframe_flag}.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{init}
@c @deffn {Function} init_ctensor ()
m4_deffn({Function}, init_ctensor, <<<()>>>)
Initializes the @code{ctensor} package.

The @code{init_ctensor} function reinitializes the @code{ctensor} package. It removes all arrays and matrices used by @code{ctensor}, resets all flags, resets @code{dim} to 4, and resets the frame metric to the Lorentz-frame.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@subsection The tensors of curved space

The main purpose of the @code{ctensor} package is to compute the tensors
of curved space(time), most notably the tensors used in general
relativity.

When a metric base is used, @code{ctensor} can compute the following tensors:

@example

 lg  -- ug
   \      \
    lcs -- mcs -- ric -- uric
              \      \       \
               \      tracer - ein -- lein
                \
                 riem -- lriem -- weyl
                     \
                      uriem


@end example

@code{ctensor} can also work using moving frames. When @code{cframe_flag} is
set to @code{true}, the following tensors can be calculated:

@example

 lfg -- ufg
     \
 fri -- fr -- lcs -- mcs -- lriem -- ric -- uric
      \                       |  \      \       \
       lg -- ug               |   weyl   tracer - ein -- lein
                              |\
                              | riem
                              |
                              \uriem

@end example
@anchor{christof}
@c @deffn {Function} christof (@var{dis})
m4_deffn({Function}, christof, <<<(@var{dis})>>>)
A function in the @code{ctensor} (component tensor)
package.  It computes the Christoffel symbols of both
kinds.  The argument @var{dis} determines which results are to be immediately
displayed.  The Christoffel symbols of the first and second kinds are
stored in the arrays @code{lcs[i,j,k]} and @code{mcs[i,j,k]} respectively and
defined to be symmetric in the first two indices. If the argument to
@code{christof} is @code{lcs} or @code{mcs} then the unique non-zero values of @code{lcs[i,j,k]}
or @code{mcs[i,j,k]}, respectively, will be displayed. If the argument is @code{all}
then the unique non-zero values of @code{lcs[i,j,k]} and @code{mcs[i,j,k]} will be
displayed.  If the argument is @code{false} then the display of the elements
will not occur. The array elements @code{mcs[i,j,k]} are defined in such a
manner that the final index is contravariant.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{ricci}
@c @deffn {Function} ricci (@var{dis})
m4_deffn({Function}, ricci, <<<(@var{dis})>>>)
A function in the @code{ctensor} (component tensor)
package. @code{ricci} computes the covariant (symmetric)
components @code{ric[i,j]} of the Ricci tensor.  If the argument @var{dis} is @code{true},
then the non-zero components are displayed.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{uricci}
@c @deffn {Function} uricci (@var{dis})
m4_deffn({Function}, uricci, <<<(@var{dis})>>>)
This function first computes the
covariant components @code{ric[i,j]} of the Ricci tensor.
Then the mixed Ricci tensor is computed using the
contravariant metric tensor.  If the value of the argument @var{dis}
is @code{true}, then these mixed components, @code{uric[i,j]} (the
index @code{i} is covariant and the index @code{j} is contravariant), will be displayed
directly.  Otherwise, @code{ricci(false)} will simply compute the entries
of the array @code{uric[i,j]} without displaying the results.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{scurvature}
@c @deffn {Function} scurvature ()
m4_deffn({Function}, scurvature, <<<()>>>)
Returns the scalar curvature (obtained by contracting
the Ricci tensor) of the Riemannian manifold with the given metric.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{einstein}
@c @deffn {Function} einstein (@var{dis})
m4_deffn({Function}, einstein, <<<(@var{dis})>>>)
A function in the @code{ctensor} (component tensor)
package.  @code{einstein} computes the mixed Einstein tensor
after the Christoffel symbols and Ricci tensor have been obtained
(with the functions @code{christof} and @code{ricci}).  If the argument @var{dis} is
@code{true}, then the non-zero values of the mixed Einstein tensor @code{ein[i,j]}
will be displayed where @code{j} is the contravariant index.
The variable @code{rateinstein} will cause the rational simplification on
these components. If @code{ratfac} is @code{true} then the components will
also be factored.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{leinstein}
@c @deffn {Function} leinstein (@var{dis})
m4_deffn({Function}, leinstein, <<<(@var{dis})>>>)
Covariant Einstein-tensor. @code{leinstein} stores the values of the covariant Einstein tensor in the array @code{lein}. The covariant Einstein-tensor is computed from the mixed Einstein tensor @code{ein} by multiplying it with the metric tensor. If the argument @var{dis} is @code{true}, then the non-zero values of the covariant Einstein tensor are displayed.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{riemann}
@c @deffn {Function} riemann (@var{dis})
m4_deffn({Function}, riemann, <<<(@var{dis})>>>)
A function in the @code{ctensor} (component tensor)
package.  @code{riemann} computes the Riemann curvature tensor
from the given metric and the corresponding Christoffel symbols. The following
index conventions are used:

@example
                l      _l       _l       _l   _m    _l   _m
 R[i,j,k,l] =  R    = |      - |      + |    |   - |    |
                ijk     ij,k     ik,j     mk   ij    mj   ik
@end example

This notation is consistent with the notation used by the @code{itensor}
package and its @code{icurvature} function.
If the optional argument @var{dis} is @code{true},
the unique non-zero components @code{riem[i,j,k,l]} will be displayed.
As with the Einstein tensor, various switches set by the user
control the simplification of the components of the Riemann tensor.
If @code{ratriemann} is @code{true}, then
rational simplification will be done. If @code{ratfac}
is @code{true} then
each of the components will also be factored.

If the variable @code{cframe_flag} is @code{false}, the Riemann tensor is
computed directly from the Christoffel-symbols. If @code{cframe_flag} is
@code{true}, the covariant Riemann-tensor is computed first from the
frame field coefficients.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{lriemann}
@c @deffn {Function} lriemann (@var{dis})
m4_deffn({Function}, lriemann, <<<(@var{dis})>>>)
Covariant Riemann-tensor (@code{lriem[]}).

Computes the covariant Riemann-tensor as the array @code{lriem}. If the
argument @var{dis} is @code{true}, unique non-zero values are displayed.

If the variable @code{cframe_flag} is @code{true}, the covariant Riemann
tensor is computed directly from the frame field coefficients. Otherwise,
the (3,1) Riemann tensor is computed first.

For information on index ordering, see @code{riemann}.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{uriemann}
@c @deffn {Function} uriemann (@var{dis})
m4_deffn({Function}, uriemann, <<<(@var{dis})>>>)
Computes the contravariant components of the Riemann
curvature tensor as array elements @code{uriem[i,j,k,l]}.  These are displayed
if @var{dis} is @code{true}.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{rinvariant}
@c @deffn {Function} rinvariant ()
m4_deffn({Function}, rinvariant, <<<()>>>)
Forms the Kretchmann-invariant (@code{kinvariant}) obtained by
contracting the tensors

@example
lriem[i,j,k,l]*uriem[i,j,k,l].
@end example

This object is not automatically simplified since it can be very large.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{weyl}
@c @deffn {Function} weyl (@var{dis})
m4_deffn({Function}, weyl, <<<(@var{dis})>>>)
Computes the Weyl conformal tensor.  If the argument @var{dis} is
@code{true}, the non-zero components @code{weyl[i,j,k,l]} will be displayed to the
user.  Otherwise, these components will simply be computed and stored.
If the switch @code{ratweyl} is set to @code{true}, then the components will be
rationally simplified; if @code{ratfac} is @code{true} then the results will be
factored as well.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn

@subsection Taylor series expansion

The @code{ctensor} package has the ability to truncate results by assuming
that they are Taylor-series approximations. This behavior is controlled by
the @code{ctayswitch} variable; when set to true, @code{ctensor} makes use
internally of the function @code{ctaylor} when simplifying results.

The @code{ctaylor} function is invoked by the following @code{ctensor} functions:

@example

    Function     Comments
    ---------------------------------
    christof()   For mcs only
    ricci()
    uricci()
    einstein()
    riemann()
    weyl()
    checkdiv()
@end example
@anchor{ctaylor}
@c @deffn {Function} ctaylor ()
m4_deffn({Function}, ctaylor, <<<()>>>)

The @code{ctaylor} function truncates its argument by converting
it to a Taylor-series using @code{taylor}, and then calling
@code{ratdisrep}. This has the combined effect of dropping terms
higher order in the expansion variable @code{ctayvar}. The order
of terms that should be dropped is defined by @code{ctaypov}; the
point around which the series expansion is carried out is specified
in @code{ctaypt}.

As an example, consider a simple metric that is a perturbation of
the Minkowski metric. Without further restrictions, even a diagonal
metric produces expressions for the Einstein tensor that are far too
complex:

@example

(%i1) load("ctensor");
(%o1)       /share/tensor/ctensor.mac
(%i2) ratfac:true;
(%o2)                                true
(%i3) derivabbrev:true;
(%o3)                                true
(%i4) ct_coords:[t,r,theta,phi];
(%o4)                         [t, r, theta, phi]
(%i5) lg:matrix([-1,0,0,0],[0,1,0,0],[0,0,r^2,0],
                [0,0,0,r^2*sin(theta)^2]);
                        [ - 1  0  0         0        ]
                        [                            ]
                        [  0   1  0         0        ]
                        [                            ]
(%o5)                   [          2                 ]
                        [  0   0  r         0        ]
                        [                            ]
                        [              2    2        ]
                        [  0   0  0   r  sin (theta) ]
(%i6) h:matrix([h11,0,0,0],[0,h22,0,0],[0,0,h33,0],[0,0,0,h44]);
                            [ h11   0    0    0  ]
                            [                    ]
                            [  0   h22   0    0  ]
(%o6)                       [                    ]
                            [  0    0   h33   0  ]
                            [                    ]
                            [  0    0    0   h44 ]
(%i7) depends(l,r);
(%o7)                               [l(r)]
(%i8) lg:lg+l*h;
      [ h11 l - 1      0          0                 0            ]
      [                                                          ]
      [     0      h22 l + 1      0                 0            ]
      [                                                          ]
(%o8) [                        2                                 ]
      [     0          0      r  + h33 l            0            ]
      [                                                          ]
      [                                    2    2                ]
      [     0          0          0       r  sin (theta) + h44 l ]
(%i9) cmetric(false);
(%o9)                                done
(%i10) einstein(false);
(%o10)                               done
(%i11) ntermst(ein);
[[1, 1], 62]
[[1, 2], 0]
[[1, 3], 0]
[[1, 4], 0]
[[2, 1], 0]
[[2, 2], 24]
[[2, 3], 0]
[[2, 4], 0]
[[3, 1], 0]
[[3, 2], 0]
[[3, 3], 46]
[[3, 4], 0]
[[4, 1], 0]
[[4, 2], 0]
[[4, 3], 0]
[[4, 4], 46]
(%o12)                               done

@end example

However, if we recompute this example as an approximation that is
linear in the variable @code{l}, we get much simpler expressions:

@example

(%i14) ctayswitch:true;
(%o14)                               true
(%i15) ctayvar:l;
(%o15)                                 l
(%i16) ctaypov:1;
(%o16)                                 1
(%i17) ctaypt:0;
(%o17)                                 0
(%i18) christof(false);
(%o18)                               done
(%i19) ricci(false);
(%o19)                               done
(%i20) einstein(false);
(%o20)                               done
(%i21) ntermst(ein);
[[1, 1], 6]
[[1, 2], 0]
[[1, 3], 0]
[[1, 4], 0]
[[2, 1], 0]
[[2, 2], 13]
[[2, 3], 2]
[[2, 4], 0]
[[3, 1], 0]
[[3, 2], 2]
[[3, 3], 9]
[[3, 4], 0]
[[4, 1], 0]
[[4, 2], 0]
[[4, 3], 0]
[[4, 4], 9]
(%o21)                               done
(%i22) ratsimp(ein[1,1]);
                         2      2  4               2     2
(%o22) - (((h11 h22 - h11 ) (l )  r  - 2 h33 l    r ) sin (theta)
                              r               r r

                            2               2      4    2
              - 2 h44 l    r  - h33 h44 (l ) )/(4 r  sin (theta))
                       r r                r



@end example

This capability can be useful, for instance, when working in the weak
field limit far from a gravitational source.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn


@subsection Frame fields

When the variable @code{cframe_flag} is set to true, the @code{ctensor} package
performs its calculations using a moving frame.
@anchor{frame}
@c @deffn {Function} frame_bracket (@var{fr}, @var{fri}, @var{diagframe})
m4_deffn({Function}, frame_bracket, <<<(@var{fr}, @var{fri}, @var{diagframe})>>>)
The frame bracket (@code{fb[]}).

Computes the frame bracket according to the following definition:

@example
   c          c         c        d     e
ifb   = ( ifri    - ifri    ) ifr   ifr
   ab         d,e       e,d      a     b
@end example

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn

@subsection Algebraic classification

A new feature (as of November, 2004) of @code{ctensor} is its ability to
compute the Petrov classification of a 4-dimensional spacetime metric.
For a demonstration of this capability, see the file
@code{share/tensor/petrov.dem}.
@anchor{nptetrad}
@c @deffn {Function} nptetrad ()
m4_deffn({Function}, nptetrad, <<<()>>>)
Computes a Newman-Penrose null tetrad (@code{np}) and its raised-index
counterpart (@code{npi}). See @code{petrov} for an example.

The null tetrad is constructed on the assumption that a four-dimensional
orthonormal frame metric with metric signature (-,+,+,+) is being used.
The components of the null tetrad are related to the inverse frame matrix
as follows:

@example

np  = (fri  + fri ) / sqrt(2)
  1       1      2

np  = (fri  - fri ) / sqrt(2)
  2       1      2

np  = (fri  + %i fri ) / sqrt(2)
  3       3         4

np  = (fri  - %i fri ) / sqrt(2)
  4       3         4

@end example

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{psi}
@c @deffn {Function} psi (@var{dis})
m4_deffn({Function}, psi, <<<(@var{dis})>>>)
Computes the five Newman-Penrose coefficients @code{psi[0]}...@code{psi[4]}.
If @code{dis} is set to @code{true}, the coefficients are displayed.
See @code{petrov} for an example.

These coefficients are computed from the Weyl-tensor in a coordinate base.
If a frame base is used, the Weyl-tensor is first converted to a coordinate
base, which can be a computationally expensive procedure. For this reason,
in some cases it may be more advantageous to use a coordinate base in the
first place before the Weyl tensor is computed. Note however, that
constructing a Newman-Penrose null tetrad requires a frame base. Therefore,
a meaningful computation sequence may begin with a frame base, which
is then used to compute @code{lg} (computed automatically by @code{cmetric})
and then @code{ug}. See @code{petrov} for an example. At this point, you can switch back to a coordinate base
by setting @code{cframe_flag} to false before beginning to compute the
Christoffel symbols. Changing to a frame base at a later stage could yield
inconsistent results, as you may end up with a mixed bag of tensors, some
computed in a frame base, some in a coordinate base, with no means to
distinguish between the two.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{petrov}
@c @deffn {Function} petrov ()
m4_deffn({Function}, petrov, <<<()>>>)
Computes the Petrov classification of the metric characterized by @code{psi[0]}...@code{psi[4]}.

For example, the following demonstrates how to obtain the Petrov-classification
of the Kerr metric:

@example
(%i1) load("ctensor");
(%o1)       /share/tensor/ctensor.mac
(%i2) (cframe_flag:true,gcd:spmod,ctrgsimp:true,ratfac:true);
(%o2)                                true
(%i3) ct_coordsys(exteriorschwarzschild,all);
(%o3)                                done
(%i4) ug:invert(lg)$
(%i5) weyl(false);
(%o5)                                done
(%i6) nptetrad(true);
(%t6) np =

[ sqrt(r - 2 m)           sqrt(r)                                 ]
[---------------   ---------------------    0            0        ]
[sqrt(2) sqrt(r)   sqrt(2) sqrt(r - 2 m)                          ]
[                                                                 ]
[ sqrt(r - 2 m)            sqrt(r)                                ]
[---------------  - ---------------------   0            0        ]
[sqrt(2) sqrt(r)    sqrt(2) sqrt(r - 2 m)                         ]
[                                                                 ]
[                                          r      %i r sin(theta) ]
[       0                    0          -------   --------------- ]
[                                       sqrt(2)       sqrt(2)     ]
[                                                                 ]
[                                          r       %i r sin(theta)]
[       0                    0          -------  - ---------------]
[                                       sqrt(2)        sqrt(2)    ]

                             sqrt(r)         sqrt(r - 2 m)
(%t7) npi = matrix([- ---------------------,---------------, 0, 0],
                      sqrt(2) sqrt(r - 2 m) sqrt(2) sqrt(r)

          sqrt(r)            sqrt(r - 2 m)
[- ---------------------, - ---------------, 0, 0],
   sqrt(2) sqrt(r - 2 m)    sqrt(2) sqrt(r)

           1               %i
[0, 0, ---------, --------------------],
       sqrt(2) r  sqrt(2) r sin(theta)

           1                 %i
[0, 0, ---------, - --------------------])
       sqrt(2) r    sqrt(2) r sin(theta)

(%o7)                                done
(%i7) psi(true);
(%t8)                              psi  = 0
                                      0

(%t9)                              psi  = 0
                                      1

                                          m
(%t10)                             psi  = --
                                      2    3
                                          r

(%t11)                             psi  = 0
                                      3

(%t12)                             psi  = 0
                                      4
(%o12)                               done
(%i12) petrov();
(%o12)                                 D

@end example

The Petrov classification function is based on the algorithm published in
"Classifying geometries in general relativity: III Classification in practice"
by Pollney, Skea, and d'Inverno, Class. Quant. Grav. 17 2885-2902 (2000).
Except for some simple test cases, the implementation is untested as of
December 19, 2004, and is likely to contain errors.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn


@subsection Torsion and nonmetricity

@code{ctensor} has the ability to compute and include torsion and nonmetricity
coefficients in the connection coefficients.

The torsion coefficients are calculated from an user-supplied tensor
@code{tr}, which should be a rank (2,1) tensor. From this, the torsion
coefficients @code{kt} are computed according to the following formulae:

@example

              m          m      m
       - g  tr   - g   tr   - tr   g
          im  kj    jm   ki     ij  km
kt   = -------------------------------
  ijk                 2


  k     km
kt   = g   kt
  ij         ijm

@end example

Note that only the mixed-index tensor is calculated and stored in the
array @code{kt}.

The nonmetricity coefficients are calculated from the user-supplied
nonmetricity vector @code{nm}. From this, the nonmetricity coefficients
@code{nmc} are computed as follows:

@example

             k    k        km
       -nm  D  - D  nm  + g   nm  g
   k      i  j    i   j         m  ij
nmc  = ------------------------------
   ij                2

@end example

where D stands for the Kronecker-delta.

When @code{ctorsion_flag} is set to @code{true}, the values of @code{kt}
are subtracted from the mixed-indexed connection coefficients computed by
@code{christof} and stored in @code{mcs}. Similarly, if @code{cnonmet_flag}
is set to @code{true}, the values of @code{nmc} are subtracted from the
mixed-indexed connection coefficients.

If necessary, @code{christof} calls the functions @code{contortion} and
@code{nonmetricity} in order to compute @code{kt} and @code{nm}.
@anchor{contortion}
@c @deffn {Function} contortion (@var{tr})
m4_deffn({Function}, contortion, <<<(@var{tr})>>>)

Computes the (2,1) contortion coefficients from the torsion tensor @var{tr}.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{nonmetricity}
@c @deffn {Function} nonmetricity (@var{nm})
m4_deffn({Function}, nonmetricity, <<<(@var{nm})>>>)

Computes the (2,1) nonmetricity coefficients from the nonmetricity
vector @var{nm}.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn



@subsection Miscellaneous features
@anchor{ctransform}
@c @deffn {Function} ctransform (@var{M})
m4_deffn({Function}, ctransform, <<<(@var{M})>>>)
A function in the @code{ctensor} (component tensor)
package which will perform a coordinate transformation
upon an arbitrary square symmetric matrix @var{M}. The user must input the
functions which define the transformation.  (Formerly called @code{transform}.)

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{findde}
@c @deffn {Function} findde (@var{A}, @var{n})
m4_deffn({Function}, findde, <<<(@var{A}, @var{n})>>>)

returns a list of the unique differential equations (expressions)
corresponding to the elements of the @var{n} dimensional square
array @var{A}. Presently, @var{n} may be 2 or 3. @code{deindex} is a global list
containing the indices of @var{A} corresponding to these unique
differential equations. For the Einstein tensor (@code{ein}), which
is a two dimensional array, if computed for the metric in the example
below, @code{findde} gives the following independent differential equations:


@c ===beg===
@c load("ctensor");
@c derivabbrev:true;
@c dim:4;
@c lg:matrix([a, 0, 0, 0], [ 0, x^2, 0, 0],
@c                         [0, 0, x^2*sin(y)^2, 0], [0,0,0,-d]);
@c depends([a,d],x);
@c ct_coords:[x,y,z,t];
@c cmetric();
@c einstein(false);
@c findde(ein,2);
@c deindex;
@c ===end===
@example
(%i1) load("ctensor");
(%o1)       /share/tensor/ctensor.mac
(%i2) derivabbrev:true;
(%o2)                                true
(%i3) dim:4;
(%o3)                                  4
(%i4) lg:matrix([a, 0, 0, 0], [ 0, x^2, 0, 0],
                              [0, 0, x^2*sin(y)^2, 0], [0,0,0,-d]);
                          [ a  0       0        0  ]
                          [                        ]
                          [     2                  ]
                          [ 0  x       0        0  ]
(%o4)                     [                        ]
                          [         2    2         ]
                          [ 0  0   x  sin (y)   0  ]
                          [                        ]
                          [ 0  0       0       - d ]
(%i5) depends([a,d],x);
(%o5)                            [a(x), d(x)]
(%i6) ct_coords:[x,y,z,t];
(%o6)                            [x, y, z, t]
(%i7) cmetric();
(%o7)                                done
(%i8) einstein(false);
(%o8)                                done
(%i9) findde(ein,2);
                                            2
(%o9) [d  x - a d + d, 2 a d d    x - a (d )  x - a  d d  x
        x                     x x         x        x    x

                                              2          2
                          + 2 a d d   - 2 a  d , a  x + a  - a]
                                   x       x      x
(%i10) deindex;
(%o10)                     [[1, 1], [2, 2], [4, 4]]
@end example

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{cograd}
@c @deffn {Function} cograd ()
m4_deffn({Function}, cograd, <<<()>>>)
Computes the covariant gradient of a scalar function allowing the
user to choose the corresponding vector name as the example under
@code{contragrad} illustrates.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{contragrad}
@c @deffn {Function} contragrad ()
m4_deffn({Function}, contragrad, <<<()>>>)

Computes the contravariant gradient of a scalar function allowing
@c "vector^F2name^F*" LOOKS LIKE IT NEEDS TO BE FIXED UP, NOT SURE HOW THOUGH
the user to choose the corresponding vector name as the example
below for the Schwarzschild metric illustrates:

@c ===beg===
@c load("ctensor");
@c derivabbrev:true;
@c ct_coordsys(exteriorschwarzschild,all);
@c depends(f,r);
@c cograd(f,g1);
@c listarray(g1);
@c contragrad(f,g2);
@c listarray(g2);
@c ===end===
@example
(%i1) load("ctensor");
(%o1)       /share/tensor/ctensor.mac
(%i2) derivabbrev:true;
(%o2)                                true
(%i3) ct_coordsys(exteriorschwarzschild,all);
(%o3)                                done
(%i4) depends(f,r);
(%o4)                               [f(r)]
(%i5) cograd(f,g1);
(%o5)                                done
(%i6) listarray(g1);
(%o6)                            [0, f , 0, 0]
                                      r
(%i7) contragrad(f,g2);
(%o7)                                done
(%i8) listarray(g2);
                               f  r - 2 f  m
                                r        r
(%o8)                      [0, -------------, 0, 0]
                                     r
@end example

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{ctensor_dscalar}
@c @deffn {Function} dscalar ()
m4_deffn({Function}, dscalar, <<<()>>>)
computes the tensor d'Alembertian of the scalar function once
dependencies have been declared upon the function. For example:

@c ===beg===
@c load("ctensor");
@c derivabbrev:true;
@c ct_coordsys(exteriorschwarzschild,all);
@c depends(p,r);
@c factor(dscalar(p));
@c ===end===
@example
(%i1) load("ctensor");
(%o1)       /share/tensor/ctensor.mac
(%i2) derivabbrev:true;
(%o2)                                true
(%i3) ct_coordsys(exteriorschwarzschild,all);
(%o3)                                done
(%i4) depends(p,r);
(%o4)                               [p(r)]
(%i5) factor(dscalar(p));
@group
                          2
                    p    r  - 2 m p    r + 2 p  r - 2 m p
                     r r           r r        r          r
(%o5)               --------------------------------------
                                       2
                                      r
@end group
@end example

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{checkdiv}
@c @deffn {Function} checkdiv ()
m4_deffn({Function}, checkdiv, <<<()>>>)

computes the covariant divergence of the mixed second rank tensor
(whose first index must be covariant) by printing the
corresponding n components of the vector field (the divergence) where
n = @code{dim}. If the argument to the function is @code{g} then the
divergence of the Einstein tensor will be formed and must be zero.
In addition, the divergence (vector) is given the array name @code{div}.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{cgeodesic}
@c @deffn {Function} cgeodesic (@var{dis})
m4_deffn({Function}, cgeodesic, <<<(@var{dis})>>>)
A function in the @code{ctensor} (component tensor)
package.  @code{cgeodesic} computes the geodesic equations of
motion for a given metric.  They are stored in the array @code{geod[i]}.  If
the argument @var{dis} is @code{true} then these equations are displayed.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn

@anchor{bdvac}
@c @deffn {Function} bdvac (@var{f})
m4_deffn({Function}, bdvac, <<<(@var{f})>>>)

generates the covariant components of the vacuum field equations of
the Brans- Dicke gravitational theory. The scalar field is specified
by the argument @var{f}, which should be a (quoted) function name
with functional dependencies, e.g., @code{'p(x)}.

The components of the second rank covariant field tensor are
represented by the array @code{bd}.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{invariant1}
@c @deffn {Function} invariant1 ()
m4_deffn({Function}, invariant1, <<<()>>>)

generates the mixed Euler- Lagrange tensor (field equations) for the
invariant density of R^2. The field equations are the components of an
array named @code{inv1}.

@opencatbox
@category{Package ctensor}
@closecatbox
@end deffn
@anchor{invariant2}
@c @deffn {Function} invariant2 ()
m4_deffn({Function}, invariant2, <<<()>>>)

*** NOT YET IMPLEMENTED ***

generates the mixed Euler- Lagrange tensor (field equations) for the
invariant density of @code{ric[i,j]*uriem[i,j]}. The field equations are the
components of an array named @code{inv2}.


@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{bimetric}
@c @deffn {Function} bimetric ()
m4_deffn({Function}, bimetric, <<<()>>>)

*** NOT YET IMPLEMENTED ***

generates the field equations of Rosen's bimetric theory. The field
equations are the components of an array named @code{rosen}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@subsection Utility functions
@anchor{diagmatrixp}
@c @deffn {Function} diagmatrixp (@var{M},@var{n})
m4_deffn({Function}, diagmatrixp, <<<(@var{M},@var{n})>>>)

Returns @code{true} if the first @var{n} rows and @var{n} columns of @var{M}
form a diagonal matrix or (2D) array.

@opencatbox
@category{Package ctensor}
@category{Predicate functions}
@closecatbox

@end deffn
m4_setcat(Package ctensor, Predicate functions)
@anchor{symmetricp}
@c @deffn {Function} symmetricp (@var{M}, @var{n})
m4_deffn({Function}, symmetricp, <<<(@var{M}, @var{n})>>>)

Returns @code{true} if @var{M} is a @var{n} by @var{n} symmetric matrix or two-dimensional array,
otherwise @code{false}.

If @var{n} is less than the size of @var{M},
@code{symmetricp} considers only the @var{n} by @var{n} submatrix (respectively, subarray)
comprising rows 1 through @var{n} and columns 1 through @var{n}.

@opencatbox
@category{Package ctensor}
@category{Predicate functions}
@closecatbox

@end deffn
m4_setcat(Package ctensor)
@anchor{ntermst}
@c @deffn {Function} ntermst (@var{f})
m4_deffn({Function}, ntermst, <<<(@var{f})>>>)
gives the user a quick picture of the "size" of the doubly subscripted
tensor (array) @var{f}.  It prints two element lists where the second
element corresponds to NTERMS of the components specified by the first
elements.  In this way, it is possible to quickly find the non-zero
expressions and attempt simplification.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{cdisplay}
@c @deffn {Function} cdisplay (@var{ten})
m4_deffn({Function}, cdisplay, <<<(@var{ten})>>>)
displays all the elements of the tensor @var{ten}, as represented by
a multidimensional array. Tensors of rank 0 and 1, as well as other types
of variables, are displayed as with @code{ldisplay}. Tensors of rank 2 are
displayed as 2-dimensional matrices, while tensors of higher rank are displayed
as a list of 2-dimensional matrices. For instance, the Riemann-tensor of
the Schwarzschild metric can be viewed as:

@example
(%i1) load("ctensor");
(%o1)       /share/tensor/ctensor.mac
(%i2) ratfac:true;
(%o2)                                true
(%i3) ct_coordsys(exteriorschwarzschild,all);
(%o3)                                done
(%i4) riemann(false);
(%o4)                                done
(%i5) cdisplay(riem);
          [ 0               0                   0           0     ]
          [                                                       ]
          [                              2                        ]
          [      3 m (r - 2 m)   m    2 m                         ]
          [ 0  - ------------- + -- - ----      0           0     ]
          [            4          3     4                         ]
          [           r          r     r                          ]
          [                                                       ]
riem    = [                                m (r - 2 m)            ]
    1, 1  [ 0               0              -----------      0     ]
          [                                     4                 ]
          [                                    r                  ]
          [                                                       ]
          [                                           m (r - 2 m) ]
          [ 0               0                   0     ----------- ]
          [                                                4      ]
          [                                               r       ]

                                [    2 m (r - 2 m)       ]
                                [ 0  -------------  0  0 ]
                                [          4             ]
                                [         r              ]
                     riem     = [                        ]
                         1, 2   [ 0        0        0  0 ]
                                [                        ]
                                [ 0        0        0  0 ]
                                [                        ]
                                [ 0        0        0  0 ]

                                [         m (r - 2 m)    ]
                                [ 0  0  - -----------  0 ]
                                [              4         ]
                                [             r          ]
                     riem     = [                        ]
                         1, 3   [ 0  0        0        0 ]
                                [                        ]
                                [ 0  0        0        0 ]
                                [                        ]
                                [ 0  0        0        0 ]

                                [            m (r - 2 m) ]
                                [ 0  0  0  - ----------- ]
                                [                 4      ]
                                [                r       ]
                     riem     = [                        ]
                         1, 4   [ 0  0  0        0       ]
                                [                        ]
                                [ 0  0  0        0       ]
                                [                        ]
                                [ 0  0  0        0       ]

                               [       0         0  0  0 ]
                               [                         ]
                               [       2 m               ]
                               [ - ------------  0  0  0 ]
                    riem     = [    2                    ]
                        2, 1   [   r  (r - 2 m)          ]
                               [                         ]
                               [       0         0  0  0 ]
                               [                         ]
                               [       0         0  0  0 ]

             [     2 m                                         ]
             [ ------------  0        0               0        ]
             [  2                                              ]
             [ r  (r - 2 m)                                    ]
             [                                                 ]
             [      0        0        0               0        ]
             [                                                 ]
  riem     = [                         m                       ]
      2, 2   [      0        0  - ------------        0        ]
             [                     2                           ]
             [                    r  (r - 2 m)                 ]
             [                                                 ]
             [                                         m       ]
             [      0        0        0         - ------------ ]
             [                                     2           ]
             [                                    r  (r - 2 m) ]

                                [ 0  0       0        0 ]
                                [                       ]
                                [            m          ]
                                [ 0  0  ------------  0 ]
                     riem     = [        2              ]
                         2, 3   [       r  (r - 2 m)    ]
                                [                       ]
                                [ 0  0       0        0 ]
                                [                       ]
                                [ 0  0       0        0 ]

                                [ 0  0  0       0       ]
                                [                       ]
                                [               m       ]
                                [ 0  0  0  ------------ ]
                     riem     = [           2           ]
                         2, 4   [          r  (r - 2 m) ]
                                [                       ]
                                [ 0  0  0       0       ]
                                [                       ]
                                [ 0  0  0       0       ]

                                      [ 0  0  0  0 ]
                                      [            ]
                                      [ 0  0  0  0 ]
                                      [            ]
                           riem     = [ m          ]
                               3, 1   [ -  0  0  0 ]
                                      [ r          ]
                                      [            ]
                                      [ 0  0  0  0 ]

                                      [ 0  0  0  0 ]
                                      [            ]
                                      [ 0  0  0  0 ]
                                      [            ]
                           riem     = [    m       ]
                               3, 2   [ 0  -  0  0 ]
                                      [    r       ]
                                      [            ]
                                      [ 0  0  0  0 ]

                               [   m                      ]
                               [ - -   0   0       0      ]
                               [   r                      ]
                               [                          ]
                               [        m                 ]
                               [  0   - -  0       0      ]
                    riem     = [        r                 ]
                        3, 3   [                          ]
                               [  0    0   0       0      ]
                               [                          ]
                               [              2 m - r     ]
                               [  0    0   0  ------- + 1 ]
                               [                 r        ]

                                    [ 0  0  0    0   ]
                                    [                ]
                                    [ 0  0  0    0   ]
                                    [                ]
                         riem     = [            2 m ]
                             3, 4   [ 0  0  0  - --- ]
                                    [             r  ]
                                    [                ]
                                    [ 0  0  0    0   ]

                                [       0        0  0  0 ]
                                [                        ]
                                [       0        0  0  0 ]
                                [                        ]
                     riem     = [       0        0  0  0 ]
                         4, 1   [                        ]
                                [      2                 ]
                                [ m sin (theta)          ]
                                [ -------------  0  0  0 ]
                                [       r                ]

                                [ 0        0        0  0 ]
                                [                        ]
                                [ 0        0        0  0 ]
                                [                        ]
                     riem     = [ 0        0        0  0 ]
                         4, 2   [                        ]
                                [         2              ]
                                [    m sin (theta)       ]
                                [ 0  -------------  0  0 ]
                                [          r             ]

                              [ 0  0          0          0 ]
                              [                            ]
                              [ 0  0          0          0 ]
                              [                            ]
                   riem     = [ 0  0          0          0 ]
                       4, 3   [                            ]
                              [                2           ]
                              [         2 m sin (theta)    ]
                              [ 0  0  - ---------------  0 ]
                              [                r           ]

           [        2                                             ]
           [   m sin (theta)                                      ]
           [ - -------------         0                0         0 ]
           [         r                                            ]
           [                                                      ]
           [                         2                            ]
           [                    m sin (theta)                     ]
riem     = [        0         - -------------         0         0 ]
    4, 4   [                          r                           ]
           [                                                      ]
           [                                          2           ]
           [                                   2 m sin (theta)    ]
           [        0                0         ---------------  0 ]
           [                                          r           ]
           [                                                      ]
           [        0                0                0         0 ]

(%o5)                                done

@end example

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{deleten}
@c @deffn {Function} deleten (@var{L}, @var{n})
m4_deffn({Function}, deleten, (@var{L}, @var{n}))
Returns a new list consisting of @var{L} with the @var{n}'th element
deleted.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@subsection Variables used by @code{ctensor}


@anchor{dim}
@c @defvr {Option variable} dim
m4_defvr({Option variable}, dim)
Default value: 4

An option in the @code{ctensor} (component tensor)
package.  @code{dim} is the dimension of the manifold with the
default 4. The command @code{dim: n} will reset the dimension to any other
value @code{n}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{diagmetric}
@c @defvr {Option variable} diagmetric
m4_defvr({Option variable}, diagmetric)
Default value: @code{false}

An option in the @code{ctensor} (component tensor)
package.  If @code{diagmetric} is @code{true} special routines compute
all geometrical objects (which contain the metric tensor explicitly)
by taking into consideration the diagonality of the metric. Reduced
run times will, of course, result. Note: this option is set
automatically by @code{csetup} if a diagonal metric is specified.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

m4_setcat(Package ctensor, Simplification flags and variables)
@anchor{ctrgsimp}
@c @defvr {Option variable} ctrgsimp
m4_defvr({Option variable}, ctrgsimp)

Causes trigonometric simplifications to be used when tensors are computed. Presently,
@code{ctrgsimp} affects only computations involving a moving frame.

@c @opencatbox
@c @category{Package ctensor}
@c @category{Simplification flags and variables}
@c @closecatbox

@c @end defvr
m4_end_defvr()

m4_setcat(Package ctensor)
@anchor{cframe_flag}
@c @defvr {Option variable} cframe_flag
m4_defvr({Option variable}, cframe_flag)

Causes computations to be performed relative to a moving frame as opposed to
a holonomic metric. The frame is defined by the inverse frame array @code{fri}
and the frame metric @code{lfg}. For computations using a Cartesian frame,
@code{lfg} should be the unit matrix of the appropriate dimension; for
computations in a Lorentz frame, @code{lfg} should have the appropriate
signature.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{ctorsion_flag}
@c @defvr {Option variable} ctorsion_flag
m4_defvr({Option variable}, ctorsion_flag)

Causes the contortion tensor to be included in the computation of the
connection coefficients. The contortion tensor itself is computed by
@code{contortion} from the user-supplied tensor @code{tr}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{cnonmet_flag}
@c @defvr {Option variable} cnonmet_flag
m4_defvr({Option variable}, cnonmet_flag)

Causes the nonmetricity coefficients to be included in the computation of
the connection coefficients. The nonmetricity coefficients are computed
from the user-supplied nonmetricity vector @code{nm} by the function
@code{nonmetricity}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{ctayswitch}
@c @defvr {Option variable} ctayswitch
m4_defvr({Option variable}, ctayswitch)

If set to @code{true}, causes some @code{ctensor} computations to be carried out using
Taylor-series expansions. Presently, @code{christof}, @code{ricci},
@code{uricci}, @code{einstein}, and @code{weyl} take into account this
setting.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{ctayvar}
@c @defvr {Option variable} ctayvar
m4_defvr({Option variable}, ctayvar)

Variable used for Taylor-series expansion if @code{ctayswitch} is set to
@code{true}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{ctaypov}
@c @defvr {Option variable} ctaypov
m4_defvr({Option variable}, ctaypov)

Maximum power used in Taylor-series expansion when @code{ctayswitch} is
set to @code{true}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{ctaypt}
@c @defvr {Option variable} ctaypt
m4_defvr({Option variable}, ctaypt)

Point around which Taylor-series expansion is carried out when
@code{ctayswitch} is set to @code{true}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{gdet}
@c @defvr {System variable} gdet
m4_defvr({System variable}, gdet)

The determinant of the metric tensor @code{lg}. Computed by @code{cmetric} when
@code{cframe_flag} is set to @code{false}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{ratchristof}
@c @defvr {Option variable} ratchristof
m4_defvr({Option variable}, ratchristof)

Causes rational simplification to be applied by @code{christof}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{rateinstein}
@c @defvr {Option variable} rateinstein
m4_defvr({Option variable}, rateinstein)
Default value: @code{true}

If @code{true} rational simplification will be
performed on the non-zero components of Einstein tensors; if
@code{ratfac} is @code{true} then the components will also be factored.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{ratriemann}
@c @defvr {Option variable} ratriemann
m4_defvr({Option variable}, ratriemann)
Default value: @code{true}

One of the switches which controls
simplification of Riemann tensors; if @code{true}, then rational
simplification will be done; if @code{ratfac} is @code{true} then each of the
components will also be factored.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{ratweyl}
@c @defvr {Option variable} ratweyl
m4_defvr({Option variable}, ratweyl)
Default value: @code{true}

If @code{true}, this switch causes the @code{weyl} function
to apply rational simplification to the values of the Weyl tensor. If
@code{ratfac} is @code{true}, then the components will also be factored.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{lfg}
@c @defvr {Variable} lfg
m4_defvr({Variable}, lfg)
The covariant frame metric. By default, it is initialized to the 4-dimensional Lorentz frame with signature (+,+,+,-). Used when @code{cframe_flag} is @code{true}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{ufg}
@c @defvr {Variable} ufg
m4_defvr({Variable}, ufg)
The inverse frame metric. Computed from @code{lfg} when @code{cmetric} is called while @code{cframe_flag} is set to @code{true}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{riem}
@c @defvr {Variable} riem
m4_defvr({Variable}, riem)
The (3,1) Riemann tensor. Computed when the function @code{riemann} is invoked. For information about index ordering, see the description of @code{riemann}.

If @code{cframe_flag} is @code{true}, @code{riem} is computed from the covariant Riemann-tensor @code{lriem}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{lriem}
@c @defvr {Variable} lriem
m4_defvr({Variable}, lriem)

The covariant Riemann tensor. Computed by @code{lriemann}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{uriem}
@c @defvr {Variable} uriem
m4_defvr({Variable}, uriem)

The contravariant Riemann tensor. Computed by @code{uriemann}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{ric}
@c @defvr {Variable} ric
m4_defvr({Variable}, ric)

The covariant Ricci-tensor. Computed by @code{ricci}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{uric}
@c @defvr {Variable} uric
m4_defvr({Variable}, uric)

The mixed-index Ricci-tensor. Computed by @code{uricci}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{lg}
@c @defvr {Variable} lg
m4_defvr({Variable}, lg)

The metric tensor. This tensor must be specified (as a @code{dim} by @code{dim} matrix)
before other computations can be performed.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{ug}
@c @defvr {Variable} ug
m4_defvr({Variable}, ug)

The inverse of the metric tensor. Computed by @code{cmetric}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{weyl_variable}
@c @defvr {Variable} weyl
m4_defvr({Variable}, weyl)

The Weyl tensor. Computed by @code{weyl}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{fb}
@c @defvr {Variable} fb
m4_defvr({Variable}, fb)

Frame bracket coefficients, as computed by @code{frame_bracket}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{kinvariant}
@c @defvr {Variable} kinvariant
m4_defvr({Variable}, kinvariant)

The Kretschmann invariant. Computed by @code{rinvariant}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{np}
@c @defvr {Variable} np
m4_defvr({Variable}, np)

A Newman-Penrose null tetrad. Computed by @code{nptetrad}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{npi}
@c @defvr {Variable} npi
m4_defvr({Variable}, npi)

The raised-index Newman-Penrose null tetrad. Computed by @code{nptetrad}.
Defined as @code{ug.np}. The product @code{np.transpose(npi)} is constant:

@example
(%i39) trigsimp(np.transpose(npi));
                              [  0   - 1  0  0 ]
                              [                ]
                              [ - 1   0   0  0 ]
(%o39)                        [                ]
                              [  0    0   0  1 ]
                              [                ]
                              [  0    0   1  0 ]
@end example

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{tr}
@c @defvr {Variable} tr
m4_defvr({Variable}, tr)

User-supplied rank-3 tensor representing torsion. Used by @code{contortion}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{kt}
@c @defvr {Variable} kt
m4_defvr({Variable}, kt)

The contortion tensor, computed from @code{tr} by @code{contortion}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{nm}
@c @defvr {Variable} nm
m4_defvr({Variable}, nm)

User-supplied nonmetricity vector. Used by @code{nonmetricity}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{nmc}
@c @defvr {Variable} nmc
m4_defvr({Variable}, nmc)

The nonmetricity coefficients, computed from @code{nm} by @code{nonmetricity}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{tensorkill}
@c @defvr {System variable} tensorkill
m4_defvr({System variable}, tensorkill)

Variable indicating if the tensor package has been initialized. Set and used by
@code{csetup}, reset by @code{init_ctensor}.

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@anchor{ct_coords}
@c @defvr {Option variable} ct_coords
m4_defvr({Option variable}, ct_coords)
Default value: @code{[]}

An option in the @code{ctensor} (component tensor)
package.  @code{ct_coords} contains a list of coordinates.
While normally defined when the function @code{csetup} is called,
one may redefine the coordinates with the assignment
@code{ct_coords: [j1, j2, ..., jn]} where the j's are the new coordinate names.
See also @mrefdot{csetup}

@c @opencatbox
@c @category{Package ctensor}
@c @closecatbox

@c @end defvr
m4_end_defvr()

@subsection Reserved names

The following names are used internally by the @code{ctensor} package and
should not be redefined:

@example
  Name         Description
  ---------------------------------------------------------------------
  _lg()        Evaluates to lfg if frame metric used, lg otherwise
  _ug()        Evaluates to ufg if frame metric used, ug otherwise
  cleanup()    Removes items drom the deindex list
  contract4()  Used by psi()
  filemet()    Used by csetup() when reading the metric from a file
  findde1()    Used by findde()
  findde2()    Used by findde()
  findde3()    Used by findde()
  kdelt()      Kronecker-delta (not generalized)
  newmet()     Used by csetup() for setting up a metric interactively
  setflags()   Used by init_ctensor()
  readvalue()
  resimp()
  sermet()     Used by csetup() for entering a metric as Taylor-series
  txyzsum()
  tmetric()    Frame metric, used by cmetric() when cframe_flag:true
  triemann()   Riemann-tensor in frame base, used when cframe_flag:true
  tricci()     Ricci-tensor in frame base, used when cframe_flag:true
  trrc()       Ricci rotation coefficients, used by christof()
  yesp()
@end example


@subsection Changes

In November, 2004, the @code{ctensor} package was extensively rewritten.
Many functions and variables have been renamed in order to make the
package compatible with the commercial version of Macsyma.


@example
  New Name     Old Name        Description
  ---------------------------------------------------------------------
  ctaylor()    DLGTAYLOR()     Taylor-series expansion of an expression
  lgeod[]      EM              Geodesic equations
  ein[]        G[]             Mixed Einstein-tensor
  ric[]        LR[]            Mixed Ricci-tensor
  ricci()      LRICCICOM()     Compute the mixed Ricci-tensor
  ctaypov      MINP            Maximum power in Taylor-series expansion
  cgeodesic()  MOTION          Compute geodesic equations
  ct_coords    OMEGA           Metric coordinates
  ctayvar      PARAM           Taylor-series expansion variable
  lriem[]      R[]             Covariant Riemann-tensor
  uriemann()   RAISERIEMANN()  Compute the contravariant Riemann-tensor
  ratriemann   RATRIEMAN       Rational simplif. of the Riemann-tensor
  uric[]       RICCI[]         Contravariant Ricci-tensor
  uricci()     RICCICOM()      Compute the contravariant Ricci-tensor
  cmetric()    SETMETRIC()     Set up the metric
  ctaypt       TAYPT           Point for Taylor-series expansion
  ctayswitch   TAYSWITCH       Taylor-series setting switch
  csetup()     TSETUP()        Start interactive setup session
  ctransform() TTRANSFORM()    Interactive coordinate transformation
  uriem[]      UR[]            Contravariant Riemann-tensor
  weyl[]       W[]             (3,1) Weyl-tensor

@end example

