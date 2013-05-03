## This file contains code that's common to just the translated
## documentation and is included by Makefile.am in each language's
## subdirectory.

include $(top_srcdir)/doc/info/common.mk

## See TODO note at top of common.mk
maxima.html: maxima.texi $(maxima_TEXINFOS)
	perl ../texi2html -split_chapter --lang=$(lang) --output=. \
          --css-include=../manual.css --init-file ../texi2html.init  \
          maxima.texi

## A rule for actually making the info file. Ideally, we'd hook into
## Automake's machinery by adding info_TEXINFOS = maxima.texi to each
## subdirectory's Makefile.am.
##
## Unfortunately, there are two problems with this. Firstly it
## completely fails for the recoded files (the utf8 directories),
## because Automake tries to read maxima.texi when it's called (and
## fails, since it doesn't exist yet). It also fails somewhat for the
## other language files, since we want to install to
## $(infodir)/$(langsdir) and I can't work out how to force
## info_TEXINFOS to do something like that.

MAKEINFOFLAGS = --enable-encoding
maxima.info: maxima.texi
	$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) $<

INSTALL_INFO=install-info
install-info: maxima.info
	$(MKDIR_P) "$(lang_info_dir)"
	$(INSTALL_DATA) -t "$(lang_info_dir)" maxima.info*

UNINSTALL_INFO=uninstall-info
uninstall-info:
	rm -f "$(lang_info_dir)/"maxima.info*


.PHONY: install-info uninstall-info
