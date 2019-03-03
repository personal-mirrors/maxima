@c -*- Mode: texinfo -*-
Engineering-format changes the way maxima outputs floating-point numbers
to the notation engineers are used to: @code{a*10^b} with @code{b} dividable by
three.
@menu
* Functions and Variables for engineering-format::
* Known bugs in engineering-format::
@end menu

@node Functions and Variables for engineering-format, Known bugs in engineering-format, engineering-format-pkg, engineering-format-pkg
@section Functions and Variables for engineering-format

@c -----------------------------------------------------------------------------
m4_setcat(Display functions, Global flags, Share packages)
@anchor{engineering_format_floats}
@c @defvr {Option variable} engineering_format_floats
m4_defvr({Option variable}, engineering_format_floats)
Default value: @code{true} 


This variable allows to temporarily switch off engineering-format.
@c ===beg===
@c load("engineering-format");
@c float(sin(10)/10000);
@c engineering_format_floats:false$
@c float(sin(10)/10000);
@c ===end===
@example
@group
(%i1) load("engineering-format");
(%o1) /home/gunter/src/maxima-code/share/contrib/engineering-for\
mat.lisp
@end group
@group
(%i2) float(sin(10)/10000);
(%o2)                - 54.40211108893698e-6
@end group
(%i3) engineering_format_floats:false$
@group
(%i4) float(sin(10)/10000);
(%o4)                - 5.440211108893698e-5
@end group
@end example

See also @mref{fpprintprec} and @mrefdot{float}

@c @opencatbox
@c @category{Display functions}
@c @category{Global flags}
@c @category{Share packages}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{engineering_format_min}
@c @defvr {Option variable} engineering_format_min
m4_defvr({Option variable}, engineering_format_min)
Default value: @code{0.0} 

The minimum absolute value that isn't automatically converted to the engineering format.
See also @mref{engineering_format_max} and @mrefdot{engineering_format_floats}

@c ===beg===
@c lst: float([.05,.5,5,500,5000,500000]);
@c load("engineering-format");
@c lst;
@c engineering_format_min:.1$
@c engineering_format_max:1000$
@c lst;
@c ===end===
@example
@group
(%i1) lst: float([.05,.5,5,500,5000,500000]);
(%o1)       [0.05, 0.5, 5.0, 500.0, 5000.0, 500000.0]
@end group
@group
(%i2) load("engineering-format");
(%o2) /home/gunter/src/maxima-code/share/contrib/engineering-for\
mat.lisp
@end group
@group
(%i3) lst;
(%o3) [50.0e-3, 500.0e-3, 5.0e+0, 500.0e+0, 5.0e+3, 500.0e+3]
@end group
(%i4) engineering_format_min:.1$
(%i5) engineering_format_max:1000$
@group
(%i6) lst;
(%o6)     [50.0e-3, 0.5, 5.0, 500.0, 5.0e+3, 500.0e+3]
@end group
@end example

@c @opencatbox
@c @category{Display functions}
@c @category{Global flags}
@c @category{Share packages}
@c @closecatbox
@c @end defvr
m4_end_defvr()


@anchor{engineering_format_max}
@c @defvr {Option variable} engineering_format_max
m4_defvr({Option variable}, engineering_format_max)
Default value: @code{0.0} 

The maximum absolute value that isn't automatically converted to the engineering format.
See also @mref{engineering_format_min} and @mrefdot{engineering_format_floats}

@c @opencatbox
@c @category{Display functions}
@c @category{Global flags}
@c @category{Share packages}
@c @closecatbox
@c @end defvr
m4_end_defvr()


@c -----------------------------------------------------------------------------
@node Known bugs in engineering-format, , Functions and Variables for engineering-format, engineering-format-pkg
@section Known Bugs

The output routine of SBCL 1.3.0 has a bug that sometimes causes the exponent not
to be dividable by three. The value of the displayed number is still valid in
this case.
