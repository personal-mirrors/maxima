include $(top_srcdir)/common.mk

genericdir = $(sharedir)/lisp-utils

genericdirDATA = defsystem.lisp

EXTRA_DIST = \
  defsystem.lisp make-depends.lisp make-proclaim.lisp makealbertdoc.lisp \
  parse-info.lisp parse-info.sh

all-local: compile-parse-info
clean-local: clean-pi-fasls

## Don't compile for clisp and ecl because they both make foo.fas and
## don't compile for acl and sbcl because they both make foo.fasl :-(
compile_for_these := $(filter-out clisp ecl acl sbcl,@BUILT_LISPS@)
compile-parse-info: parse-info.lisp
	for x in $(compile_for_these); do \
	  ../maxima-local --very-quiet -l $$x -r \
	    ':lisp (compile-file "parse-info.lisp")'; \
	done

clean-pi-fasls:
	rm -f parse-info.{sse2f,x86f,fasl,fas,o,lx32fsl}

.PHONY: clean-pi-fasls compile-parse-info
