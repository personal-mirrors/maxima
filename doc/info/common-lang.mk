## This file contains code that's common to just the translated
## documentation and is included by Makefile.am in each language's
## subdirectory.

include $(top_srcdir)/doc/info/common.mk

## See TODO note at top of common.mk
maxima.html: maxima.texi $(maxima_TEXINFOS)
	perl ../texi2html -split_chapter --lang=$(lang) --output=. \
          --css-include=../manual.css --init-file ../texi2html.init  \
          maxima.texi
