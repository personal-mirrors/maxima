include $(top_srcdir)/common.mk

genericdir = $(sharedir)/lisp-utils

genericdirDATA = defsystem.lisp

EXTRA_DIST = \
  defsystem.lisp make-depends.lisp make-proclaim.lisp makealbertdoc.lisp \
  parse-info.lisp parse-info.sh

pi_stamps=$(foreach lisp,@BUILT_LISPS@,binary-$(lisp)/.pi-stamp)

all-local: compile-parse-info
clean-local: clean-binary-dirs

compile-parse-info: $(pi_stamps)

## This compiles parse-info.lisp for each configured lisp, storing the
## outputs in binary-<lispname>
binary-%/.pi-stamp: parse-info.lisp
	$(MAKE) bd_$*
	cp parse-info.lisp binary-$*
	../maxima-local --very-quiet -l $* -r \
	  ':lisp (compile-file "'binary-$*'/parse-info.lisp")'
	touch $@

clean-binary-dirs:
	rm -rf binary-*

.PHONY: clean-binary-dirs compile-parse-info
