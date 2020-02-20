@c -*- Mode: texinfo -*-
@menu
* The dynamics package::
* Graphical analysis of discrete dynamical systems::
* Visualization with VTK::
@end menu

@node The dynamics package, Graphical analysis of discrete dynamical systems, dynamics-pkg, dynamics-pkg
@section The dynamics package

Package @code{dynamics} includes functions for 3D visualization,
animations, graphical analysis of differential and difference equations
and numerical solution of differential equations. The functions for
differential equations are described in the section on @mxref{Numerical,
Numerical Methods} and the functions to plot the Mandelbrot and Julia
sets are described in the section on @mrefdot{Plotting}

All the functions in this package will be loaded automatically the first
time they are used.

@opencatbox
@category{Dynamical systems}
@category{Share packages}
@category{Package dynamics}
@closecatbox

@node Graphical analysis of discrete dynamical systems,  Visualization with VTK, The dynamics package, dynamics-pkg
@section Graphical analysis of discrete dynamical systems

m4_setcat(Package dynamics, Plotting)
@anchor{chaosgame}
@c @deffn {Function} chaosgame ([[@var{x1}, @var{y1}]@dots{}[@var{xm}, @var{ym}]], [@var{x0}, @var{y0}], @var{b}, @var{n}, @var{options}, @dots{});
m4_deffn({Function}, chaosgame, ([[@var{x1}, @var{y1}]@dots{}[@var{xm}, @var{ym}]], [@var{x0}, @var{y0}], @var{b}, @var{n}, @var{options}, @dots{}))

Implements the so-called chaos game: the initial point (@var{x0},
@var{y0}) is plotted and then one of the @var{m} points
[@var{x1}, @var{y1}]@dots{}@var{xm}, @var{ym}]
will be selected at random. The next point plotted will be on the
segment from the previous point plotted to the point chosen randomly, at a
distance from the random point which will be @var{b} times that segment's
length. The procedure is repeated @var{n} times. The options are the
same as for @mrefdot{plot2d}

@strong{Example}. A plot of Sierpinsky's triangle:

@example
(%i1) chaosgame([[0, 0], [1, 0], [0.5, sqrt(3)/2]], [0.1, 0.1], 1/2,
                 30000, [style, dots]);
@end example

@ifnotinfo
@figure{dynamics7}
@end ifnotinfo

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox

@c @end deffn
m4_end_deffn()

@anchor{evolution}
@c @deffn {Function} evolution (@var{F}, @var{y0}, @var{n},  @dots{}, @var{options}, @dots{});
m4_deffn({Function}, evolution, (@var{F}, @var{y0}, @var{n},  @dots{}, @var{options}, @dots{}))

Draws @var{n+1} points in a two-dimensional graph, where the horizontal
coordinates of the points are the integers 0, 1, 2, ..., @var{n}, and
the vertical coordinates are the corresponding values @var{y(n)} of the
sequence defined by the recurrence relation
@ifnottex
@example
        y(n+1) = F(y(n))
@end example
@end ifnottex
@tex
$$y_{n+1} = F(y_n)$$
@end tex

With initial value @var{y(0)} equal to @var{y0}. @var{F} must be an
expression that depends only on one variable (in the example, it
depend on @var{y}, but any other variable can be used),
@var{y0} must be a real number and @var{n} must be a positive integer.
 This function accepts the same options as @mrefdot{plot2d}

@strong{Example}.

@example
(%i1) evolution(cos(y), 2, 11);
@end example
@ifnotinfo
@figure{dynamics1}
@end ifnotinfo


@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox

@c @end deffn
m4_end_deffn()

@anchor{evolution2d}
@c @deffn {Function} evolution2d ([@var{F}, @var{G}], [@var{u}, @var{v}], [@var{u0}, @var{y0}], @var{n}, @var{options}, @dots{});
m4_deffn({Function}, evolution2d, ([@var{F}, @var{G}], [@var{u}, @var{v}], [@var{u0}, @var{y0}], @var{n}, @var{options}, @dots{}))

Shows, in a two-dimensional plot, the first @var{n+1} points in the
sequence of points defined by the two-dimensional discrete dynamical
system with recurrence relations
@ifnottex
@example
        u(n+1) = F(u(n), v(n))    v(n+1) = G(u(n), v(n))
@end example
@end ifnottex
@tex
$$\cases{u_{n+1} = F(u_n, v_n) &\cr v_{n+1} = G(u_n, v_n)}$$
@end tex

With initial values @var{u0} and @var{v0}. @var{F} and @var{G} must be
two expressions that depend only on two variables, @var{u} and
@var{v}, which must be named explicitly in a list. The options are the
same as for @mrefdot{plot2d}

@strong{Example}. Evolution of a two-dimensional discrete dynamical system:

@example
(%i1) f: 0.6*x*(1+2*x)+0.8*y*(x-1)-y^2-0.9$
(%i2) g: 0.1*x*(1-6*x+4*y)+0.1*y*(1+9*y)-0.4$
(%i3) evolution2d([f,g], [x,y], [-0.5,0], 50000, [style,dots]);
@end example

@ifnotinfo
@figure{dynamics5}
@end ifnotinfo

And an enlargement of a small region in that fractal:

@example
(%i9) evolution2d([f,g], [x,y], [-0.5,0], 300000, [x,-0.8,-0.6],
                  [y,-0.4,-0.2], [style, dots]);
@end example

@ifnotinfo
@figure{dynamics6}
@end ifnotinfo

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox

@c @end deffn
m4_end_deffn()

@anchor{ifs}
@c @deffn {Function} ifs ([@var{r1}, @dots{}, @var{rm}], [@var{A1},@dots{}, @var{Am}], [[@var{x1}, @var{y1}], @dots{}, [@var{xm}, @var{ym}]], [@var{x0}, @var{y0}], @var{n}, @var{options}, @dots{});
m4_deffn({Function}, ifs, ([@var{r1}, @dots{}, @var{rm}], [@var{A1},@dots{}, @var{Am}], [[@var{x1}, @var{y1}], @dots{}, [@var{xm}, @var{ym}]], [@var{x0}, @var{y0}], @var{n}, @var{options}, @dots{}))

Implements the Iterated Function System method. This method is similar
to the method described in the function @mrefdot{chaosgame} but instead of
shrinking the segment from the current point to the randomly chosen
point, the 2 components of that segment will be multiplied by the 2 by 2
matrix @var{Ai} that corresponds to the point chosen randomly.

The random choice of one of the @var{m} attractive points can be made
with a non-uniform probability distribution defined by the weights
@var{r1},...,@var{rm}. Those weights are given in cumulative form; for
instance if there are 3 points with probabilities 0.2, 0.5 and 0.3, the
weights @var{r1}, @var{r2} and @var{r3} could be 2, 7 and 10. The
options are the same as for @mrefdot{plot2d}

@strong{Example}. Barnsley's fern, obtained with 4 matrices and 4 points:

@example
(%i1) a1: matrix([0.85,0.04],[-0.04,0.85])$
(%i2) a2: matrix([0.2,-0.26],[0.23,0.22])$
(%i3) a3: matrix([-0.15,0.28],[0.26,0.24])$
(%i4) a4: matrix([0,0],[0,0.16])$
(%i5) p1: [0,1.6]$
(%i6) p2: [0,1.6]$
(%i7) p3: [0,0.44]$
(%i8) p4: [0,0]$
(%i9) w: [85,92,99,100]$
(%i10) ifs(w, [a1,a2,a3,a4], [p1,p2,p3,p4], [5,0], 50000, [style,dots]);
@end example

@ifnotinfo
@figure{dynamics8}
@end ifnotinfo

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox

@c @end deffn
m4_end_deffn()

@anchor{orbits}
@c @deffn {Function} orbits (@var{F}, @var{y0}, @var{n1}, @var{n2}, [@var{x}, @var{x0}, @var{xf}, @var{xstep}], @var{options}, @dots{});
m4_deffn({Function}, orbits, (@var{F}, @var{y0}, @var{n1}, @var{n2}, [@var{x}, @var{x0}, @var{xf}, @var{xstep}], @var{options}, @dots{}))

Draws the orbits diagram for a family of one-dimensional
discrete dynamical systems, with one parameter @var{x}; that kind of
diagram is used to study the bifurcations of a one-dimensional discrete
system.

The function @var{F(y)} defines a sequence with a starting value of
@var{y0}, as in the case of the function @code{evolution}, but in this
case that function will also depend on a parameter @var{x} that will
take values in the interval from @var{x0} to @var{xf} with increments of
@var{xstep}. Each value used for the parameter @var{x} is shown on the
horizontal axis. The vertical axis will show the @var{n2} values
of the sequence @var{y(n1+1)},..., @var{y(n1+n2+1)} obtained after letting
the sequence evolve @var{n1} iterations.  In addition to the options
accepted by @mrefcomma{plot2d} it accepts an option @var{pixels} that
sets up the maximum number of different points that will be represented
in the vertical direction.

@strong{Example}. Orbits diagram of the quadratic map, with a parameter
@var{a}:

@example
(%i1) orbits(x^2+a, 0, 50, 200, [a, -2, 0.25], [style, dots]);
@end example

@ifnotinfo
@figure{dynamics3}
@end ifnotinfo

To enlarge the region around the lower bifurcation near x @code{=} -1.25 use:
@example
(%i2) orbits(x^2+a, 0, 100, 400, [a,-1,-1.53], [x,-1.6,-0.8],
             [nticks, 400], [style,dots]);
@end example

@ifnotinfo
@figure{dynamics4}
@end ifnotinfo

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox

@c @end deffn
m4_end_deffn()

@anchor{staircase}
@c @deffn {Function} staircase (@var{F}, @var{y0}, @var{n},@var{options},@dots{});
m4_deffn({Function}, staircase, (@var{F}, @var{y0}, @var{n},@var{options},@dots{}))

Draws a staircase diagram for the sequence defined by the recurrence
relation
@ifnottex
@example
        y(n+1) = F(y(n))
@end example
@end ifnottex
@tex
$$y_{n+1} = F(y_n)$$
@end tex

The interpretation and allowed values of the input parameters is the
same as for the function @mrefdot{evolution} A staircase diagram consists
of a plot of the function @var{F(y)}, together with the line @var{G(y)}
@code{=} @var{y}. A vertical segment is drawn from the point (@var{y0},
@var{y0}) on that line until the point where it intersects the function
@var{F}. From that point a horizontal segment is drawn until it reaches
the point (@var{y1}, @var{y1}) on the line, and the procedure is
repeated @var{n} times until the point (@var{yn}, @var{yn}) is
reached. The options are the same as for @mrefdot{plot2d}

@strong{Example}.

@example
(%i1) staircase(cos(y), 1, 11, [y, 0, 1.2]);
@end example
@ifnotinfo
@figure{dynamics2}
@end ifnotinfo

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox

@c @end deffn
m4_end_deffn()

@node Visualization with VTK, , Graphical analysis of discrete dynamical systems, dynamics-pkg
@section Visualization with VTK

Function scene creates 3D images and animations using the @emph{Visualization
ToolKit} (VTK) software. In order to use that function, Xmaxima and VTK should be
installed in your system (including the TCL bindings of VTK, which in
some system might come in a separate package).

@anchor{scene}
@c @deffn {Function} scene (@var{objects}, @dots{}, @var{options}, @dots{});
m4_deffn({Function}, scene, (@var{objects}, @dots{}, @var{options}, @dots{}))

Accepts an empty list or a list of several @mxref{scene_objects,objects}
and @mxrefdot{scene_options,options} The program launches Xmaxima, which
opens an external window representing the given objects in a
3-dimensional space and applying the options given. Each object must
belong to one of the following 4 classes: sphere, cube, cylinder or cone
(see @mxref{scene_objects,Scene objects}). Objects are identified by
giving their name or by a list in which the first element is the class
name and the following elements are options for that object.
 
@strong{Example}. A hexagonal pyramid with a blue background:
@c ===beg===
@c scene(cone, [background,"#9980e5"])$
@c ===end===
@example
(%i1) scene(cone, [background,"#9980e5"])$
@end example
@ifnotinfo
@figure{scene1}
@end ifnotinfo

By holding down the left button of the mouse while it is moved on the
graphics window, the camera can be rotated showing different views of
the pyramid. The two plot options @mxref{scene_elevation,elevation} and
@mxref{scene_azimuth,azimuth} can also be used to change the initial
orientation of the viewing camera. The camera can be moved by holding
the middle mouse button while moving it and holding the right-side mouse
button while moving it up or down will zoom in or out.

Each object option should be a list starting with the option name,
followed by its value. The list of allowed options can be found in the
@mxref{object_options,Scene object's options} section.

@strong{Example}. This will show a sphere falling to the ground and
bouncing off without losing any energy. To start or pause the
animation, press the play/pause button.
@c ===beg===
@c p: makelist ([0,0,2.1- 9.8*t^2/2], t, 0, 0.64, 0.01)$
@c p: append (p, reverse(p))$
@c ball: [sphere, [radius,0.1], [thetaresolution,20],
@c   [phiresolution,20],[position,0,0,2.1], [color,red],
@c   [animate,position,p]]$
@c ground: [cube, [xlength,2], [ylength,2], [zlength,0.2],
@c   [position,0,0,-0.1],[color,violet]]$
@c scene (ball, ground, restart);
@c ===end===

@example
(%i1) p: makelist ([0,0,2.1- 9.8*t^2/2], t, 0, 0.64, 0.01)$

(%i2) p: append (p, reverse(p))$

(%i3) ball: [sphere, [radius,0.1], [thetaresolution,20],
  [phiresolution,20], [position,0,0,2.1], [color,red],
  [animate,position,p]]$

(%i4) ground: [cube, [xlength,2], [ylength,2], [zlength,0.2],
  [position,0,0,-0.1],[color,violet]]$

(%i5) scene (ball, ground, restart)$
@end example
@ifnotinfo
@figure{scene2}
@end ifnotinfo

The @var{restart} option was used to make the animation restart
automatically every time the last point in the position list is reached.
The accepted values for the colors are the same as for the @mref{color}
option of plot2d. 

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox

@c @end deffn
m4_end_deffn()

@anchor{scene_options}
@subsection Scene options

@anchor{scene_azimuth}
@c @defvr {Scene option} azimuth [azimuth, @var{angle}]
m4_defvr({Scene option}, azimuth) @
@fname{[azimuth, @var{angle}]}
Default value: @code{135}

The rotation of the camera on the horizontal (x, y) plane. @var{angle}
must be a real number; an angle of 0 means that the camera points in the
direction of the y axis and the x axis will appear on the right.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{scene_background}
@c @defvr {Scene option} background [background, @var{color}]
m4_defvr({Scene option}, background) @
@fname{[background, @var{color}]}
Default value: @code{black}

The color of the graphics window's background. It accepts color names or
hexadecimal red-green-blue strings (see the @mref{color} option of plot2d).

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{scene_elevation}
@c @defvr {Scene option} elevation [elevation, @var{angle}]
m4_defvr({Scene option}, elevation) @
@fname{[elevation, @var{angle}]}
Default value: @code{30}

The vertical rotation of the camera. The @var{angle} must be a real
number; an angle of 0 means that the camera points on the horizontal,
and the default angle of 30 means that the camera is pointing 30 degrees
down from the horizontal.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{scene_height}
@c @defvr {Scene option} height [height, @var{pixels}]
m4_defvr({Scene option}, height) @
@fname{[height, @var{pixels}]}
Default value: @code{500}

The height, in pixels, of the graphics window. @var{pixels} must be a
positive integer number.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{scene_restart}
@c @defvr {Scene option} restart [restart, @var{value}]
m4_defvr({Scene option}, restart) @
@fname{[restart, @var{value}]}
Default value: @code{false}

A true value means that animations will restart automatically when the
end of the list is reached. Writing just ``restart'' is equivalent to
[restart, @var{true}].

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{scene_tstep}
@c @defvr {Scene option} tstep [tstep, @var{time}]
m4_defvr({Scene option}, tstep) @
@fname{[tstep, @var{time}]}
Default value: @code{10}

The amount of time, in mili-seconds, between iterations among
consecutive animation frames. @var{time} must be a real number.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{scene_width}
@c @defvr {Scene option} width [width, @var{pixels}]
m4_defvr({Scene option}, width) @
@fname{[width, @var{pixels}]}
Default value: @code{500}

The width, in pixels, of the graphics window. @var{pixels} must be a
positive integer number.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{scene_windowname}
@c @defvr {Scene option} windowname [windowtitle, @var{name}]
m4_defvr({Scene option}, windowname) @
@fname{[windowtitle, @var{name}]}
Default value: @code{.scene}

@var{name} must be a string that can be used as the name of the Tk
window created by Xmaxima for the @code{scene} graphics. The default
value @code{.scene} implies that a new top level window will be created.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{scene_windowtitle}
@c @defvr {Scene option} windowtitle [windowtitle, @var{name}]
m4_defvr({Scene option}, windowtitle) @
@fname{[windowtitle, @var{name}]}
Default value: @code{Xmaxima: scene}

@var{name} must be a string that will be written in the title of the
window created by @code{scene}.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{scene_objects}
@subsection Scene objects

@anchor{cone}
@c @defvr {Scene object} cone [cone, @var{options}]
m4_defvr({Scene object}, cone) @
@fname{ [cone, @var{options}]}

Creates a regular pyramid with height equal to 1 and a hexagonal base
with vertices 0.5 units away from the axis. Options
@mxref{object_height,height} and @mxref{object_radius,radius} can be used
to change those defaults and option @mxref{object_resolution, resolution}
can be used to change the number of edges of the base; higher values
will make it look like a cone. By default, the axis will be along the x
axis, the middle point of the axis will be at the origin and the vertex
on the positive side of the x axis; use options
@mxref{object_orientation,orientation} and @mxref{object_center,center} to
change those defaults.

@strong{Example}. This shows a pyramid that starts rotating around the z
axis when the play button is pressed.
 
@example
(%i1) scene([cone, [orientation,0,30,0], [tstep,100],
   [animate,orientation,makelist([0,30,i],i,5,360,5)]], restart)$
@end example

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{cube}
@c @defvr {Scene object} cube [cube, @var{options}]
m4_defvr({Scene object}, cube) @
@fname{[cube, @var{options}]}

A cube with edges of 1 unit and faces parallel to the xy, xz and yz
planes. The lengths of the three edges can be changed with options
@mxref{object_xlength,xlength}, @mxref{object_ylength,ylength} and
@mxref{object_zlength,zlength}, turning it into a rectangular box and
the faces can be rotated with option @mxrefdot{object_orientation,orientation}

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{cylinder}
@c @defvr {Scene object} cylinder [cylinder, @var{options}]
m4_defvr({Scene object}, cylinder) @
@fname{[cylinder, @var{options}]}

Creates a regular prism with height equal to 1 and a hexagonal base with
vertices 0.5 units away from the axis. Options
@mxref{object_height,height} and @mxref{object_radius,radius} can be
used to change those defaults and option @mxref{object_resolution,
resolution} can be used to change the number of edges of the base;
higher values will make it look like a cylinder. The default height can
be changed with the option @mxrefdot{object_height,height} By default,
the axis will be along the x axis and the middle point of the axis will
be at the origin; use options @mxref{object_orientation,orientation} and
@mxref{object_center,center} to change those defaults.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{sphere}
@c @defvr {Scene object} sphere [sphere, @var{options}]
m4_defvr({Scene object}, sphere) @
@fname{[sphere, @var{options}]}

A sphere with default radius of 0.5 units and center at the origin. 

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_options}
@subsection Scene object's options

@anchor{object_animation}
@c @defvr {Object option} animation [animation, @var{property}, @var{positions}]
m4_defvr({Object option}, animation) @
@fname{[animation, @var{property}, @var{positions}]}

@var{property} should be one of the following 4 object's properties:
@mxrefcomma{object_origin,origin} @mxrefcomma{object_scale,scale}
@mxref{object_position,position} or
@mxref{object_orientation,orientation} and @var{positions} should be a
list of points. When the play button is pressed, the object property
will be changed sequentially through all the values in the list, at
intervals of time given by the option @mxrefdot{scene_tstep,tstep} The
rewind button can be used to point at the start of the sequence making
the animation restart after the play button is pressed again.

See also @mxrefdot{object_track,track}

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_capping}
@c @defvr {Object option} capping [capping, @var{number}]
m4_defvr({Object option}, capping) @
@fname{[capping, @var{number}]}
Default value: @code{1}

In a cone or a cylinder, it defines whether the base (or bases) will be
shown. A value of 1 for @var{number} makes the base visible and a value
of 0 makes it invisible.
 
@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_center}
@c @defvr {Object option} center [center, @var{point}]
m4_defvr({Object option}, center) @
@fname{[center, @var{point}]}
Default value: @code{[0, 0, 0]}

The coordinates of the object's geometric center, with respect to its
@mxrefdot{object_position, position} @var{point} can be a list with 3
real numbers, or 3 real numbers separated by commas. In a cylinder, cone
or cube it will be at half its height and in a sphere at its center.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_color}
@c @defvr {Object option} color [color, @var{colorname}]
m4_defvr({Object option}, color) @
@fname{[color, @var{colorname}]}
Default value: @code{white}

The color of the object. It accepts color names or hexadecimal
red-green-blue strings (see the @mref{color} option of plot2d).

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_endphi}
@c @defvr {Object option} endphi [endphi, @var{angle}]
m4_defvr({Object option}, endphi) @
@fname{[endphi, @var{angle}]}
Default value: @code{180}

In a sphere phi is the angle on the vertical plane that passes through
the z axis, measured from the positive part of the z axis. @var{angle}
must be a number between 0 and 180 that sets the final value of phi at
which the surface will end. A value smaller than 180 will eliminate a
part of the sphere's surface.

See also @mxref{object_startphi,startphi} and
@mxrefdot{object_phiresolution,phiresolution}

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_endtheta}
@c @defvr {Object option} endtheta [endtheta, @var{angle}]
m4_defvr({Object option}, endtheta) @
@fname{[endtheta, @var{angle}]}
Default value: @code{360}

In a sphere theta is the angle on the horizontal plane (longitude),
measured from the positive part of the x axis. @var{angle} must be a
number between 0 and 360 that sets the final value of theta at which the
surface will end. A value smaller than 360 will eliminate a part of
the sphere's surface.

See also @mxref{object_starttheta,starttheta} and
@mxrefdot{object_thetaresolution,thetaresolution}

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_height}
@c @defvr {Object option} height [height, @var{value}]
m4_defvr({Object option}, height) @
@fname{[height, @var{value}]}
Default value: @code{1}

@var{value} must be a positive number which sets the height of a cone
or a cylinder.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_linewidth}
@c @defvr {Object option} linewidth [linewidth, @var{value}]
m4_defvr({Object option}, linewidth) @
@fname{[linewidth, @var{value}]}
Default value: @code{1}

The width of the lines, when option @mxref{object_wireframe,wireframe} is
used. @var{value} must be a positive number.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_opacity}
@c @defvr {Object option} opacity [opacity, @var{value}]
m4_defvr({Object option}, opacity) @
@fname{[opacity, @var{value}]}
Default value: @code{1}

@var{value} must be a number between 0 and 1. The lower the number, the
more transparent the object will become. The default value of 1 means a
completely opaque object.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_orientation}
@c @defvr {Object option} orientation [orientation, @var{angles}]
m4_defvr({Object option}, orientation) @
@fname{[orientation, @var{angles}]}
Default value: @code{[0, 0, 0]}

Three angles by which the object will be rotated with respect to the
three axis. @var{angles} can be a list with 3 real numbers, or 3 real
numbers separated by commas. @strong{Example}: @code{[0, 0, 90]} rotates
the x axis of the object to the y axis of the reference frame.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_origin}
@c @defvr {Object option} origin [origin, @var{point}]
m4_defvr({Object option}, origin) @
@fname{[origin, @var{point}]}
Default value: @code{[0, 0, 0]}

The coordinates of the object's origin, with respect to which its
other dimensions are defined. @var{point} can be a list with 3
real numbers, or 3 real numbers separated by commas.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_phiresolution}
@c @defvr {Object option} phiresolution [phiresolution, @var{num}]
m4_defvr({Object option}, phiresolution) @
@fname{[phiresolution, @var{num}]}
Default value: @code{}

The number of sub-intervals into which the phi angle interval from
@mxref{object_startphi,startphi} to @mxref{object_endphi,endphi}
will be divided. @var{num} must be a positive integer.

See also @mxref{object_startphi,startphi} and
@mxrefdot{object_endphi,endphi}

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_points}
@c @defvr {Object option} points  [points]
m4_defvr({Object option}, points) @
@fname{ [points]}

Only the vertices of the triangulation used to render the surface will
be shown. @strong{Example}: @code{[sphere, [points]]}

See also @mxref{object_surface,surface} and
@mxrefdot{object_wireframe,wireframe}

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_pointsize}
@c @defvr {Object option} pointsize [pointsize, @var{value}]
m4_defvr({Object option}, pointsize) @
@fname{[pointsize, @var{value}]}
Default value: @code{1}

The size of the points, when option @mxref{object_points,points} is
used. @var{value} must be a positive number.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_position}
@c @defvr {Object option} position [position, @var{point}]
m4_defvr({Object option}, position) @
@fname{[position, @var{point}]}
Default value: @code{[0, 0, 0]}

The coordinates of the object's position. @var{point} can be a list with 3
real numbers, or 3 real numbers separated by commas.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_radius}
@c @defvr {Object option} radius [radius, @var{value}]
m4_defvr({Object option}, radius) @
@fname{[radius, @var{value}]}
Default value: @code{0.5}

The radius or a sphere or the distance from the axis to the base's
vertices in a cylinder or a cone. @var{value} must be a positive number.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_resolution}
@c @defvr {Object option} resolution [resolution, @var{number}]
m4_defvr({Object option}, resolution) @
@fname{[resolution, @var{number}]}
Default value: @code{6}

@var{number} must be a integer greater than 2 that sets the number of
edges in the base of a cone or a cylinder.
 
@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_scale}
@c @defvr {Object option} scale [scale, @var{factors}]
m4_defvr({Object option}, scale) @
@fname{[scale, @var{factors}]}
Default value: @code{[1, 1, 1]}

Three numbers by which the object will be scaled with respect to the
three axis. @var{factors} can be a list with 3 real numbers, or 3 real
numbers separated by commas. @strong{Example}: @code{[2, 0.5, 1]}
enlarges the object to twice its size in the x direction, reduces the
dimensions in the y direction to half and leaves the z dimensions
unchanged.

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_startphi}
@c @defvr {Object option} startphi [startphi, @var{angle}]
m4_defvr({Object option}, startphi) @
@fname{[startphi, @var{angle}]}
Default value: @code{0}

In a sphere phi is the angle on the vertical plane that passes through
the z axis, measured from the positive part of the z axis. @var{angle}
must be a number between 0 and 180 that sets the initial value of phi at
which the surface will start. A value bigger than 0 will eliminate a
part of the sphere's surface.

See also @mxref{object_endphi,endphi} and
@mxrefdot{object_phiresolution,phiresolution}

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_starttheta}
@c @defvr {Object option} starttheta [starttheta, @var{angle}]
m4_defvr({Object option}, starttheta) @
@fname{[starttheta, @var{angle}]}
Default value: @code{0}

In a sphere theta is the angle on the horizontal plane (longitude),
measured from the positive part of the x axis. @var{angle} must be a
number between 0 and 360 that sets the initial value of theta at which
the surface will start. A value bigger than 0 will eliminate a part of
the sphere's surface.

See also @mxref{object_endtheta,endtheta} and
@mxrefdot{object_thetaresolution,thetaresolution}

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_surface}
@c @defvr {Object option} surface [surface]
m4_defvr({Object option}, surface) @
@fname{[surface]}

The surfaces of the object will be rendered and the lines and points of
the triangulation used to build the surface will not be shown. This is
the default behavior, which can be changed using either the option
@mxref{object_points,points} or @mxrefdot{object_wireframe,wireframe}

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_thetaresolution}
@c @defvr {Object option} thetaresolution [thetaresolution, @var{num}]
m4_defvr({Object option}, thetaresolution) @
@fname{[thetaresolution, @var{num}]}
Default value: @code{}

The number of sub-intervals into which the theta angle interval from
@mxref{object_starttheta,starttheta} to @mxref{object_endtheta,endtheta}
will be divided. @var{num} must be a positive integer.

See also @mxref{object_starttheta,starttheta} and
@mxrefdot{object_endtheta,endtheta}

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_track}
@c @defvr {Object option} track [track, @var{positions}]
m4_defvr({Object option}, track) @
@fname{[track, @var{positions}]}

@var{positions} should be a list of points. When the play button is
pressed, the object position will be changed sequentially through all
the points in the list, at intervals of time given by the option
@mxrefcomma{scene_tstep,tstep} leaving behind a track of the object's
trajectory. The rewind button can be used to point at the start of the
sequence making the animation restart after the play button is pressed
again.

@strong{Example}. This will show the trajectory of a ball thrown with
speed of 5 m/s, at an angle of 45 degrees, when the air resistance can
be neglected:

@example
(%i1) p: makelist ([0,4*t,4*t- 9.8*t^2/2], t, 0, 0.82, 0.01)$

(%i2) ball: [sphere, [radius,0.1], [color,red], [track,p]]$

(%i3) ground: [cube, [xlength,2], [ylength,4], [zlength,0.2],
      [position,0,1.5,-0.2],[color,green]]$

(%i4) scene (ball, ground)$
@end example

See also @mxrefdot{object_animation,animation}


@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_xlength}
@c @defvr {Object option} xlength [xlength, @var{length}]
m4_defvr({Object option}, xlength) @
@fname{[xlength, @var{length}]}
Default value: @code{1}

The height of a cube in the x direction. @var{length} must be a positive
number. See also @mxref{object_ylength,ylength} and
@mxrefdot{object_zlength,zlength}

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_ylength}
@c @defvr {Object option} ylength [ylength, @var{length}]
m4_defvr({Object option}, ylength) @
@fname{[ylength, @var{length}]}
Default value: @code{1}

The height of a cube in the y direction. @var{length} must be a positive
number. See also @mxref{object_xlength,xlength} and
@mxrefdot{object_zlength,zlength}

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_zlength}
@c @defvr {Object option} zlength [zlength, @var{length}]
m4_defvr({Object option}, zlength) @
@fname{[zlength, @var{length}]}
Default value: @code{1}

The height of a cube in z the direction. @var{length} must be a positive
 number.  See also @mxref{object_xlength,xlength} and
 @mxrefdot{object_ylength,ylength}

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()

@anchor{object_wireframe}
@c @defvr {Object option} wireframe  [wireframe]
m4_defvr({Object option}, wireframe) @
@fname{ [wireframe]}

Only the edges of the triangulation used to render the surface will be
shown. @strong{Example}: @code{[cube, [wireframe]]}

See also @mxref{object_surface,surface} and
@mxrefdot{object_points,points}

@c @opencatbox
@c @category{Package dynamics}
@c @category{Plotting}
@c @closecatbox
@c @end defvr
m4_end_defvr()