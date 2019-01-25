m4_dnl Change the quote characters to something that isn't likely to
m4_dnl show up in the manual.
m4_changequote(`<<<', `>>>')
m4_dnl
m4_dnl Set the category to be used for all the following deffn and
m4_dnl defvr entries.  We do this by adding the category to the DC
m4_dnl index and defining the macro m4_cat to be the given category.
m4_dnl This is used by m4_deffn/m4_defvr to produce the right entries.
m4_define(<<<m4_setcat>>>,
<<<@c setcat $@
m4_define(<<<m4_cat>>>, <<<$@>>>)>>>)m4_dnl
m4_dnl

m4_define(<<<m4_dcindex_entry>>>, <<<
m4_ifelse(<<<$#>>>, <<<1>>>,
<<<m4_ifelse(<<<$1>>>, <<<>>>, , @dcindex $1!m4_name)>>>,
@dcindex $1!m4_name <<<m4_dcindex_entry(m4_shift($@))>>>)
>>>)

m4_define(<<<m4_anchor_entry>>>, <<<
m4_ifelse(<<<$#>>>, <<<1>>>,
<<<m4_ifelse(<<<$1>>>, <<<>>>, , @anchor{$1-m4_name})>>>,
@anchor{$1-m4_name} <<<m4_anchor_entry(m4_shift($@))>>>)
>>>)

m4_define(<<<m4_catentry>>>,<<<
m4_ifelse(<<<$#>>>, <<<1>>>,
<<<m4_ifelse(<<<$1>>>, <<<>>>, , @category{$1})>>>,
@category{$1} <<<m4_catentry(m4_shift($@))>>>)
>>>)

m4_dnl Define a function entry.  Basically like @deffn, but we do
m4_dnl more.  First, define an anchor with the function name, another
m4_dnl with the label "m4_cat()-name" to provide an anchor for the
m4_dnl category index to reference.  Add it to the DC index. and
m4_dnl finally use @deffn to define the function for texinfo.
m4_define(<<<m4_deffn>>>,
<<<@c deffn m4_cat()
m4_define(<<<m4_name>>>, $2)m4_dnl
@anchor{$2}
m4_anchor_entry(m4_cat())
m4_dcindex_entry(m4_cat())
@deffn $1 $2 $3
>>>)
m4_dnl Like m4_deffn, but for @deffnx.
m4_define(<<<m4_deffnx>>>,
<<<@c deffnx
m4_define(<<<m4_name>>>, $2)m4_dnl
@anchor{$2}
m4_anchor_entry(m4_cat())
m4_dcindex_entry(m4_cat())
@deffnx $1 $2 $3
>>>)
m4_define(<<<m4_end_deffn>>>,<<<m4_dnl
@opencatbox
m4_catentry(m4_cat())
@closecatbox
@end deffn
>>>)

m4_define(<<<m4_dcindex_var>>>, <<<
m4_ifelse(<<<$#>>>, <<<1>>>,
<<<m4_ifelse(<<<$1>>>, <<<>>>, , @dcindex $1!m4_name)>>>,
@dcindex $1!m4_name <<<m4_dcindex_var(m4_shift($@))>>>)
>>>)

m4_define(<<<m4_anchor_var>>>, <<<
m4_ifelse(<<<$#>>>, <<<1>>>,
<<<m4_ifelse(<<<$1>>>, <<<>>>, , @anchor{$1-m4_name})>>>,
@anchor{$1-m4_name} <<<m4_anchor_var(m4_shift($@))>>>)
>>>)

m4_dnl Like deffn bug for @defvr
m4_define(<<<m4_defvr>>>,
<<<@c defvr m4_cat()
m4_define(<<<m4_name>>>, $2)m4_dnl
@anchor{$2}
m4_anchor_var(m4_cat())
m4_dcindex_var(m4_cat())
@defvr $1 $2
>>>)

m4_define(<<<m4_category>>>,
<<<m4_dnl
@category{$1}>>>)

m4_dnl For writing formulas suitable for various output formats.  For
m4_dnl simplicity two or three arguments are required:
m4_dnl
m4_dnl 1:  HTML output with MathJAX enabled
m4_dnl 2:  HTML output without MathJax. Also used for info
m4_dnl 3:  If given, this is for TeX output.  If not, then use arg 1.
m4_dnl
m4_dnl If an arg contains a comma, you will need to quote the argument
m4_dnl using `'.
m4_define(<<<m4_mathjax>>>, 
<<<@ifhtml
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
m4_ifelse(<<<$#>>>, <<<3>>>, <<<<<<$3>>>>>>, <<<$1>>>)
@end tex>>>)
