m4_dnl For writing formulas suitable for various output formats.  For
m4_dnl simplicity three arguments are required:
m4_dnl
m4_dnl 1:  HTML output with MathJAX enabled
m4_dnl 2:  HTML output without MathJax. Also used for info
m4_dnl 3:  TeX output
m4_dnl
m4_dnl If an arg contains a comma, you will need to quote the argument
m4_dnl using `'.
m4_define(`m4_mathjax', 
`@ifhtml
@ifset mathjax
@html
$1
@end html
@end ifset
@ifclear mathjax
$2
@end ifclear
@end ifhtml
@ifinfo
$2
@end ifinfo
@tex
$3
@end tex
')
m4_dnl Change the quote characters to something that isn't likely to
m4_dnl show up in the manual.
m4_changequote(`[{{[', `]}}]')