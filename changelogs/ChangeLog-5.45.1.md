Maxima 5.45.1 change log
========================

Changes in the Windows installer:
---------------------------------
 * Strip the included C programs (winkill, winkill_lib, maxima_longnames)
 * Downgrade the included Gnuplot version.
   The Gnuplot team announced, that version < 5.4 should be used with Windows.
   That solves a problem, that plots could not be rotated (bug #3796)
 * Compile the included TCL/TK statically
   Solves a strange error when TCL wanted to load the library tcl86.dll. (*)
 * Use draw_renderer:gnuplot as default for Windows/SBCL.
   gnuplot_pipes did not work with SBCL (*)
 * Downgrade the included wxWidgets to 3.1.4.
   version 3.1.5 caused a strange warning (about localization), 3.1.4
   does not have that problem. (*)

(*) These changes were already included in the binary Windows installer
    for 5.45.0, they were discovered, after the source code release was made.
