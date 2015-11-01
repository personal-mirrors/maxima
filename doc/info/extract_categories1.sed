s/^@deffn  *{\([^}]*\)}  *\([^[:blank:]]*\).*/items = ["fn_\2_\1"]/
s/^@defvr  *{\([^}]*\)}  *\([^[:blank:]]*\).*/items = ["vr_\2_\1"]/
s/^@deffnx  *{\([^}]*\)}  *\([^[:blank:]]*\).*/if not "fn_\1" in items: items.append ("fn_\2_\1")/
s/^@defvrx  *{\([^}]*\)}  *\([^[:blank:]]*\).*/if not "vr_\1" in items: items.append ("vr_\2_\1")/
s/^@end deffn/items = []/
s/^@end defvr/items = []/
s/^@node  *\([^,]*\).*/items = ["nd_\1"] # extracted from node/
s/@opencatbox//
s/@closecatbox//
s/@category{\([^}]*\)}\s*/\$foo = []\$for x in items: foo.append ([items[0], x])\$try: categories ["\1"] . extend (foo)\$except KeyError: categories ["\1"] = foo\$/g
