@c -*- Mode: texinfo -*-

@menu
* Introduction to Elliptic Functions and Integrals::
* Functions and Variables for Elliptic Functions::
* Functions and Variables for Elliptic Integrals::
@end menu



@node Introduction to Elliptic Functions and Integrals, Functions and Variables for Elliptic Functions, , Top
@comment  node-name,  next,  previous,  up

@section Introduction to Elliptic Functions and Integrals

Maxima includes support for Jacobian elliptic functions and for
complete and incomplete elliptic integrals.  This includes symbolic
manipulation of these functions and numerical evaluation as well.
Definitions of these functions and many of their properties can by
found in Abramowitz and Stegun, Chapter 16--17.  As much as possible,
we use the definitions and relationships given there.

In particular, all elliptic functions and integrals use the parameter
@math{m} instead of the modulus @math{k} or the modular angle
m4_mathjax(
<<<\(\alpha\).>>>,
@math{\alpha}.,
<<<$\alpha$.>>>)
This is one area where we differ from Abramowitz and
Stegun who use the modular angle for the elliptic functions.  The
following relationships are true:
m4_mathjax(
<<<$$m = k^2$$ and $$k = \sin \alpha$$>>>,
<<<@math{m = k^2} and @math{k = \sin(\alpha)}>>>)

The elliptic functions and integrals are primarily intended to support
symbolic computation.  Therefore, most of derivatives of the functions
and integrals are known.  However, if floating-point values are given,
a floating-point result is returned.

Support for most of the other properties of elliptic functions and
integrals other than derivatives has not yet been written.

Some examples of elliptic functions:
@c ===beg===
@c jacobi_sn (u, m);
@c jacobi_sn (u, 1);
@c jacobi_sn (u, 0);
@c diff (jacobi_sn (u, m), u);
@c diff (jacobi_sn (u, m), m);
@c ===end===
@example
(%i1) jacobi_sn (u, m);
(%o1)                    jacobi_sn(u, m)
(%i2) jacobi_sn (u, 1);
(%o2)                        tanh(u)
(%i3) jacobi_sn (u, 0);
(%o3)                        sin(u)
(%i4) diff (jacobi_sn (u, m), u);
(%o4)            jacobi_cn(u, m) jacobi_dn(u, m)
(%i5) diff (jacobi_sn (u, m), m);
(%o5) jacobi_cn(u, m) jacobi_dn(u, m)

      elliptic_e(asin(jacobi_sn(u, m)), m)
 (u - ------------------------------------)/(2 m)
                     1 - m

            2
   jacobi_cn (u, m) jacobi_sn(u, m)
 + --------------------------------
              2 (1 - m)
@end example

Some examples of elliptic integrals:
@c ===beg===
@c elliptic_f (phi, m);
@c elliptic_f (phi, 0);
@c elliptic_f (phi, 1);
@c elliptic_e (phi, 1);
@c elliptic_e (phi, 0);
@c elliptic_kc (1/2);
@c makegamma (%);
@c diff (elliptic_f (phi, m), phi);
@c diff (elliptic_f (phi, m), m);
@c ===end===
@example
(%i1) elliptic_f (phi, m);
(%o1)                  elliptic_f(phi, m)
(%i2) elliptic_f (phi, 0);
(%o2)                          phi
(%i3) elliptic_f (phi, 1);
                               phi   %pi
(%o3)                  log(tan(--- + ---))
                                2     4
(%i4) elliptic_e (phi, 1);
(%o4)                       sin(phi)
(%i5) elliptic_e (phi, 0);
(%o5)                          phi
(%i6) elliptic_kc (1/2);
                                     1
(%o6)                    elliptic_kc(-)
                                     2
(%i7) makegamma (%);
                                 2 1
                            gamma (-)
                                   4
(%o7)                      -----------
                           4 sqrt(%pi)
(%i8) diff (elliptic_f (phi, m), phi);
                                1
(%o8)                 ---------------------
                                    2
                      sqrt(1 - m sin (phi))
(%i9) diff (elliptic_f (phi, m), m);
       elliptic_e(phi, m) - (1 - m) elliptic_f(phi, m)
(%o9) (-----------------------------------------------
                              m

                                 cos(phi) sin(phi)
                             - ---------------------)/(2 (1 - m))
                                             2
                               sqrt(1 - m sin (phi))
@end example

Support for elliptic functions and integrals was written by Raymond
Toy.  It is placed under the terms of the General Public License (GPL)
that governs the distribution of Maxima.

@opencatbox
@category{Elliptic functions}
@closecatbox

@node Functions and Variables for Elliptic Functions, Functions and Variables for Elliptic Integrals, Introduction to Elliptic Functions and Integrals, Top
@comment  node-name,  next,  previous,  up

@section Functions and Variables for Elliptic Functions

Elliptic functions can be defined in many different ways, including as
inverses of elliptic functions and ratios of theta functions,  In the
following, we express the elliptic functions as Fourier series where
m4_mathjax(
<<<$$q = e^{-\pi K'(m)/K(m)}$$>>>,
<<<@math{q = exp(-\pi K'(m)/K(m))}>>>)
and
m4_mathjax(
<<<$$v = {{\pi u}\over{2 K(m)}}$$>>>,
<<<@math{v = \pi u / (2 K(m))}>>>)
with @math{K(m)} being @ref{elliptic_kc, @code{elliptic_kc(m)}} and
@math{K'(m)} being @code{elliptic_kc(1-m)}.

m4_setcat(Elliptic functions)
m4_deffn(<<<{Function}>>>, <<<jacobi_sn>>>, <<<(@var{u}, @var{m})>>>)
The Jacobian elliptic function @math{sn(u,m)}:
m4_mathjax(
<<<$${\rm sn}(u|m) = \frac{2\pi}{\sqrt{m} K} \sum_{n=0}^{\infty} \frac{q^{n+1/2}}{1-q^{2n+1}}\sin[(2n+1)v]$$>>>,
<<<@math{sn(u|m) = 2 %pi/(sqrt(m)*K) sum(q^(n+1/2)/(1-q^(2*n+1)) sin((2*n+1)*v),n,0,inf)}>>>,
<<<$${\rm sn}(u|m) = {{2\pi}\over{\sqrt{m} K}} \sum_{n=0}^{\infty} {q^{n+1/2}\over{1-q^{2n+1}}} \sin[(2n+1)v]$$>>>)

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<jacobi_cn>>>, <<<(@var{u}, @var{m})>>>)
The Jacobian elliptic function @math{cn(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<jacobi_dn>>>, <<<(@var{u}, @var{m})>>>)
The Jacobian elliptic function @math{dn(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<jacobi_ns>>>, <<<(@var{u}, @var{m})>>>)
The Jacobian elliptic function @math{ns(u,m) = 1/sn(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<jacobi_sc>>>, <<<(@var{u}, @var{m})>>>)
The Jacobian elliptic function @math{sc(u,m) = sn(u,m)/cn(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<jacobi_sd>>>, <<<(@var{u}, @var{m})>>>)
The Jacobian elliptic function @math{sd(u,m) = sn(u,m)/dn(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<jacobi_nc>>>, <<<(@var{u}, @var{m})>>>)
The Jacobian elliptic function @math{nc(u,m) = 1/cn(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<jacobi_cs>>>, <<<(@var{u}, @var{m})>>>)
The Jacobian elliptic function @math{cs(u,m) = cn(u,m)/sn(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<jacobi_cd>>>, <<<(@var{u}, @var{m})>>>)
The Jacobian elliptic function @math{cd(u,m) = cn(u,m)/dn(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<jacobi_nd>>>, <<<(@var{u}, @var{m})>>>)
The Jacobian elliptic function @math{nd(u,m) = 1/dn(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<jacobi_ds>>>, <<<(@var{u}, @var{m})>>>)
The Jacobian elliptic function @math{ds(u,m) = dn(u,m)/sn(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<jacobi_dc>>>, <<<(@var{u}, @var{m})>>>)
The Jacobian elliptic function @math{dc(u,m) = dn(u,m)/cn(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<inverse_jacobi_sn>>>, <<<(@var{u}, @var{m})>>>)
The inverse of the Jacobian elliptic function
m4_mathjax(
<<<\({\rm sn}(u,m)\).>>>,
<<<@math{sn(u,m)}.>>>,
<<<${\rm sn}(u,m)$.>>>)
This can also be represented by
m4_mathjax(
<<<$${\rm sn}^{-1}(u, m) = \int_0^u {dt\over\sqrt{(1-t^2)(1-mt^2)}}$$>>>,
<<<@math{sn(u,m) = integral(1/sqrt((1-t^2)(1-m*t^2)), t, 0, u)}>>>)

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<inverse_jacobi_cn>>>, <<<(@var{u}, @var{m})>>>)
The inverse of the Jacobian elliptic function @math{cn(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<inverse_jacobi_dn>>>, <<<(@var{u}, @var{m})>>>)
The inverse of the Jacobian elliptic function @math{dn(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<inverse_jacobi_ns>>>, <<<(@var{u}, @var{m})>>>)
The inverse of the Jacobian elliptic function @math{ns(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<inverse_jacobi_sc>>>, <<<(@var{u}, @var{m})>>>)
The inverse of the Jacobian elliptic function @math{sc(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<inverse_jacobi_sd>>>, <<<(@var{u}, @var{m})>>>)
The inverse of the Jacobian elliptic function @math{sd(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<inverse_jacobi_nc>>>, <<<(@var{u}, @var{m})>>>)
The inverse of the Jacobian elliptic function @math{nc(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<inverse_jacobi_cs>>>, <<<(@var{u}, @var{m})>>>)
The inverse of the Jacobian elliptic function @math{cs(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<inverse_jacobi_cd>>>, <<<(@var{u}, @var{m})>>>)
The inverse of the Jacobian elliptic function @math{cd(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<inverse_jacobi_nd>>>, <<<(@var{u}, @var{m})>>>)
The inverse of the Jacobian elliptic function @math{nd(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<inverse_jacobi_ds>>>, <<<(@var{u}, @var{m})>>>)
The inverse of the Jacobian elliptic function @math{ds(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<inverse_jacobi_dc>>>, <<<(@var{u}, @var{m})>>>)
The inverse of the Jacobian elliptic function @math{dc(u,m)}.

@c @opencatbox
@c @category{Elliptic functions}
@c @closecatbox
@c @end deffn
m4_end_deffn()


@node Functions and Variables for Elliptic Integrals, , Functions and Variables for Elliptic Functions, Top
@comment  node-name,  next,  previous,  up

@section Functions and Variables for Elliptic Integrals

m4_setcat(Elliptic integrals)
@anchor{elliptic_f}
m4_deffn(<<<{Function}>>>, <<<elliptic_f>>>, <<<(@var{phi}, @var{m})>>>)
The incomplete elliptic integral of the first kind, defined as

m4_mathjax(
<<<$$F(\phi|m) = \int_0^\phi {{d\theta}\over{\sqrt{1 - m\sin^2\theta}}}$$>>>,
<<<@math{integrate(1/sqrt(1 - m*sin(x)^2), x, 0, phi)}>>>)

See also @ref{elliptic_e} and @ref{elliptic_kc}.

@c @opencatbox
@c @category{Elliptic integrals}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{elliptic_e}
m4_deffn(<<<{Function}>>>, <<<elliptic_e>>>, <<<(@var{phi}, @var{m})>>>)
The incomplete elliptic integral of the second kind, defined as

m4_mathjax(
<<<$$E(\phi|m) = \int_0^\phi \sqrt{1 - m\sin^2\theta}\,d\theta$$>>>,
<<<@math{elliptic_e(phi, m) = integrate(sqrt(1 - m*sin(x)^2), x, 0, phi)}>>>)

This is Legendre's form for the elliptic integral of the second kind.
See also @ref{elliptic_f} and @ref{elliptic_ec}.

@c @opencatbox
@c @category{Elliptic integrals}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<elliptic_eu>>>, <<<(@var{u}, @var{m})>>>)
The incomplete elliptic integral of the second kind, defined as

m4_mathjax(
<<<$$Eu(u,m) = \int_0^u {\rm dn}(v, m)\,dv  = \int_0^\tau \sqrt{{1-m t^2}\over{1-t^2}}\,dt$$>>>,
<<<@math{Eu(u,m) = integrate(dn(v,m)^2,v,0,u) = integrate(sqrt(1-m*t^2)/sqrt(1-t^2), t, 0, tau)}>>>)

where
m4_mathjax(
<<<\(\tau = {\rm sn}(u, m)\).>>>,
<<<@math{tau = sn(u,m).}>>>,
<<<$\tau = {\rm sn}(u, m)$.>>>)
This is Jacobi's epsilon function.

This is related to @code{elliptic_e} by

m4_mathjax(
<<<$$\text{Eu}(u,m) = E(\sin^{-1} \text{sn}(u,m), m)$$>>>,
<<<@math{@code{elliptic_eu}(u, m) = @code{elliptic_e}(asin(sn(u,m)),m)}>>>,
<<<$${\rm Eu}(u,m) = E(\sin^{-1} {\rm sn}(u, m), m)$$>>>)

See also @ref{elliptic_e}.
@c @opencatbox
@c @category{Elliptic integrals}
@c @closecatbox
@c @end deffn
m4_end_deffn()

m4_deffn(<<<{Function}>>>, <<<elliptic_pi>>>, <<<(@var{n}, @var{phi}, @var{m})>>>)
The incomplete elliptic integral of the third kind, defined as

m4_mathjax(
<<<$$\Pi(n; \phi|m) = \int_0^\phi {{d\theta}\over{(1-n\sin^2 \theta)\sqrt{1 - m\sin^2\theta}}}$$>>>,
<<<@math{integrate(1/(1-n*sin(x)^2)/sqrt(1 - m*sin(x)^2), x, 0, phi)}>>>)

@c @opencatbox
@c @category{Elliptic integrals}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{elliptic_kc}
m4_deffn(<<<{Function}>>>, <<<elliptic_kc>>>, <<<(@var{m})>>>)
The complete elliptic integral of the first kind, defined as

m4_mathjax(
<<<$$K(m) = \int_0^{{\pi}\over{2}} {{d\theta}\over{\sqrt{1 - m\sin^2\theta}}}$$>>>,
<<<@math{integrate(1/sqrt(1 - m*sin(x)^2), x, 0, %pi/2)}>>>)

For certain values of @math{m}, the value of the integral is known in
terms of @math{Gamma} functions.  Use @code{makegamma} to evaluate them.

@c @opencatbox
@c @category{Elliptic integrals}
@c @closecatbox
@c @end deffn
m4_end_deffn()

@anchor{elliptic_ec}
m4_deffn(<<<{Function}>>>, <<<elliptic_ec>>>, <<<(@var{m})>>>)
The complete elliptic integral of the second kind, defined as

m4_mathjax(
<<<$$E(m) = \int_0^{{\pi}\over{2}} \sqrt{1 - m\sin^2\theta}\,d\theta$$>>>,
<<<@math{E(m) = integrate(sqrt(1 - m*sin(x)^2), x, 0, %pi/2)}>>>)

For certain values of @math{m}, the value of the integral is known in
terms of @math{Gamma} functions.  Use @code{makegamma} to evaluate them.

@c @opencatbox
@c @category{Elliptic integrals}
@c @closecatbox
@c @end deffn
m4_end_deffn()

