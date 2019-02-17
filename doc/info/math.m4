m4_dnl Change the quote characters to something that isn't likely to
m4_dnl show up in the manual.
m4_changequote(`<<<', `>>>')
m4_dnl
m4_dnl Set the categories to be used for all the following deffn and
m4_dnl defvr entries.  This accepts any number of arguments.  To
m4_dnl retrieve the categories, use m4_cat() which returns the comma-
m4_dnl separated list of categories.
m4_dnl
m4_define(<<<m4_setcat>>>,
<<<@c setcat $@
m4_define(<<<m4_cat>>>, <<<$@>>>)>>>)m4_dnl
m4_dnl

m4_dnl Create index entries given the name and the list of categories.
m4_dnl The call should look like
m4_dnl   m4_dfindex_entry(name, cat1, cat2, cat3, ...)
m4_dnl The various cat entries should have been set via m4_setcat(), and
m4_dnl is retrieved by m4_cat().
m4_dnl
m4_dnl This produces
m4_dnl   @dfindex cat1!name
m4_dnl   @dfindex cat2!name
m4_dnl   ...
m4_dnl 
m4_dnl The entry "cat!name" will get converted by texindex to a 4-arg
m4_dnl \entry item, suitable for the pdf document and will be
m4_dnl processed by an awk script to produce category.texi for the html
m4_dnl document.
m4_dnl
m4_define(<<<m4_dfindex_entry>>>, <<<
m4_ifelse(<<<$#>>>, <<<2>>>,
@dfindex $2!$1,
@dfindex $2!$1 <<<m4_dfindex_entry($1, m4_shift(m4_shift($@)))>>>)
>>>)
m4_dnl
m4_dnl Create anchor references given the name and the list of categories.
m4_dnl The call should look like
m4_dnl   m4_anchory_entry(name, cat1, cat2, cat3, ...)
m4_dnl This is very similar to m4_dfindex_entry, except we create @anchor
m4_dnl entries:
m4_dnl   @anchor{cat1-name}
m4_dnl   @anchor{cat2-name}
m4_dnl   ...
m4_dnl
m4_define(<<<m4_anchor_entry>>>, <<<
m4_ifelse(<<<$#>>>, <<<2>>>,
@anchor{$2-$1},
@anchor{$2-$1} <<<m4_anchor_entry($1, m4_shift(m4_shift($@)))>>>)
>>>)

m4_define(<<<m4_catentry>>>,<<<
m4_ifelse(<<<$#>>>, <<<2>>>,
<<<@catentries{$1,$2}>>>,
<<<@catentries{$1,$2}>>> <<<m4_catentry($1, m4_shift(m4_shift($@)))>>>)
>>>)

m4_dnl Define a function entry.  Basically like @deffn, but we do
m4_dnl more.  First, define an anchor with the function name, another
m4_dnl with the label "m4_cat()-name" to provide an anchor for the
m4_dnl category index to reference.  Add it to the DC index. and
m4_dnl finally use @deffn to define the function for texinfo.
m4_dnl
m4_dnl We also define a new macro, m4_deffn_name, which is set to the name
m4_dnl of the function, the second arg.  This is used by m4_deffnx
m4_define(<<<m4_deffn>>>,
<<<@c deffn m4_cat()
m4_define(<<<m4_deffn_name>>>, <<<$2>>>)m4_dnl
m4_anchor_entry(fn-$2, m4_cat())m4_dnl
m4_dfindex_entry($2, m4_cat())m4_dnl
@deffn $1 $2 $3 >>>)
m4_dnl Like m4_deffn, but for @deffnx.  If the function name is the same
m4_dnl as the previous m4_deffn, then we don't produce the anchor and
m4_dnl entries because we already have.
m4_define(<<<m4_deffnx>>>,
<<<@c deffnx
m4_ifelse(<<<m4_deffn_name()>>>, <<<$2>>>, <<<>>>, <<<
m4_anchor_entry(fn-$2, m4_cat())
m4_dfindex_entry($2, m4_cat())
>>>)
@deffnx $1 $2 $3
>>>)
m4_define(<<<m4_end_deffn>>>,<<<m4_dnl

@opencatbox
m4_catentry(<<<fn>>>, m4_cat())
@closecatbox
@end deffn
>>>)

m4_dnl m4_dvindex_var and m4_anchor_var are the same as 

m4_dnl m4_dcindex_entry and m4_anchor_entry, except that they're meant
m4_dnl to be used with defvr.  This is mostly in case we decide to do
m4_dnl something different from deffn for defvr.  Maybe have a
m4_dnl different index and anchor format?
m4_dnl
m4_define(<<<m4_dvindex_var>>>, <<<
m4_ifelse(<<<$#>>>, <<<2>>>,
@dvindex $2!$1,
@dvindex $2!$1 <<<m4_dvindex_var($1, m4_shift(m4_shift($@)))>>>)
>>>)

m4_define(<<<m4_anchor_var>>>, <<<
m4_ifelse(<<<$#>>>, <<<2>>>,
@anchor{$2-$1},
@anchor{$2-$1} <<<m4_anchor_var($1, m4_shift(m4_shift($@)))>>>)
>>>)

m4_dnl Like deffn bug for @defvr
m4_define(<<<m4_defvr>>>,
<<<@c defvr m4_cat()
m4_anchor_var(vr-$2, m4_cat())
m4_dvindex_var($2, m4_cat())
@defvr $1 $2
>>>)

m4_define(<<<m4_end_defvr>>>,<<<m4_dnl
@opencatbox
m4_catentry(<<<vr>>>, m4_cat())
@closecatbox
@end defvr
>>>)

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
