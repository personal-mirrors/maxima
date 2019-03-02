@c -*- Mode: texinfo -*-
@menu
* Functions and Variables for clebsch_gordan::
@end menu

@node Functions and Variables for clebsch_gordan, , clebsch_gordan-pkg, clebsch_gordan-pkg
@section Functions and Variables for clebsch_gordan

m4_setcat(clebsch_gordan)
@c @deffn {Function} clebsch_gordan (@var{j1}, @var{j2}, @var{m1}, @var{m2}, @var{j}, @var{m})
m4_deffn({Function}, clebsch_gordan, <<<(@var{j1}, @var{j2}, @var{m1}, @var{m2}, @var{j}, @var{m})>>>)

Compute the Clebsch-Gordan coefficient @verb{|<j1, j2, m1, m2 | j, m>|}.

@c @opencatbox
@c @category{clebsch_gordan}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c @deffn {Function} racah_v (@var{a}, @var{b}, @var{c}, @var{a1}, @var{b1}, @var{c1})
m4_deffn({Function}, racah_v, <<<(@var{a}, @var{b}, @var{c}, @var{a1}, @var{b1}, @var{c1})>>>)

Compute Racah's V coefficient (computed in terms of a related
Clebsch-Gordan coefficient).

@c @opencatbox
@c @category{clebsch_gordan}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c @deffn {Function} racah_w (@var{j1}, @var{j2}, @var{j5}, @var{j4}, @var{j3}, @var{j6})
m4_deffn({Function}, racah_w, <<<(@var{j1}, @var{j2}, @var{j5}, @var{j4}, @var{j3}, @var{j6})>>>)

Compute Racah's W coefficient (computed in terms of a Wigner @verb{|6j|} symbol)

@c @opencatbox
@c @category{clebsch_gordan}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c @deffn {Function} wigner_3j (@var{j1}, @var{j2}, @var{j3}, @var{m1}, @var{m2}, @var{m3})
m4_deffn({Function}, wigner_3j, <<<(@var{j1}, @var{j2}, @var{j3}, @var{m1}, @var{m2}, @var{m3})>>>)

Compute Wigner's @verb{|3j|} symbol (computed in terms of a related
Clebsch-Gordan coefficient).

@c @opencatbox
@c @category{clebsch_gordan}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c @deffn {Function} wigner_6j (@var{j1}, @var{j2}, @var{j3}, @var{j4}, @var{j5}, @var{j6})
m4_deffn({Function}, wigner_6j, <<<(@var{j1}, @var{j2}, @var{j3}, @var{j4}, @var{j5}, @var{j6})>>>)

Compute Wigner's @verb{|6j|} symbol.

@c @opencatbox
@c @category{clebsch_gordan}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@c @deffn {Function} wigner_9j (@var{a}, @var{b}, @var{c}, @var{d}, @var{e}, @var{f}, @var{g}, @var{h}, @var{i}, @var{j},)
m4_deffn({Function}, wigner_9j, <<<(@var{a}, @var{b}, @var{c}, @var{d}, @var{e}, @var{f}, @var{g}, @var{h}, @var{i}, @var{j},)>>>)

Compute Wigner's @verb{|9j|} symbol.

@c @opencatbox
@c @category{clebsch_gordan}
@c @closecatbox
@c @end deffn
m4_end_deffn()
