include $(top_srcdir)/common.mk

genericdir = $(sharedir)/lisp-utils

genericdirDATA = defsystem.lisp

EXTRA_DIST = \
  defsystem.lisp make-depends.lisp make-proclaim.lisp makealbertdoc.lisp \
  parse-info.lisp parse-info.sh

all-local: compile-parse-info
clean-local: clean-pi-fasls

## This compiles parse-info.lisp for each configured lisp. Normally,
## just passing output file "parse-info-<foo>" works fine and the lisp
## implementation appends ".fasl" or whatever. ECL and ACL take us
## seriously and don't append anything. This isn't a problem for ACL,
## since it loads the result perfectly well. Unfortunately, if you ask
## ECL to load "parse-info-ecl", it doesn't realise that it's been
## compiled and you get a strange error, hence the nasty special
## casing below.
compile-parse-info: parse-info.lisp
	@for x in @BUILT_LISPS@; do \
	  echo "Compiling for lisp: "$$x; \
	  if echo $$x | grep ecl >/dev/null ; then \
	    of=parse-info-ecl.fas; \
	  else \
	    of=parse-info-$$x; \
	  fi; \
	  ../maxima-local --very-quiet -l $$x -r \
	    ':lisp (compile-file "parse-info.lisp" :output-file "'"$$of"'")'; \
	done

clean-pi-fasls:
	rm -f parse-info-*

.PHONY: clean-pi-fasls compile-parse-info
