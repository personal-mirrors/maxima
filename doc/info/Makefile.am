if LANG_EN
  EN_DIR = en
endif

if LANG_ES
  ES_DIR = es
endif

if LANG_PT
  PT_DIR = pt
endif

if LANG_PT_BR
  PT_BR_DIR = pt_BR
endif

if LANG_DE
  DE_DIR = de
endif

if LANG_JA
  JA_DIR = ja
endif

if LANG_RU
  RU_DIR = ru
endif

SUBDIRS = $(DE_DIR) $(EN_DIR) $(ES_DIR) $(JA_DIR) $(PT_DIR) $(PT_BR_DIR) $(RU_DIR)
DIST_SUBDIRS = de en es fr ja pt pt_BR ru zh figures

INFOLANG ?= en

# We need the html docu to be generated on "make all" because if we fail
# to do so a "sudo make install" will generate the html docu as read-only
# fails conflicting with a subsequent "make html".
all-local: html

# MOVE THIS RULE TO SUBDIR EN
#maxima.epub: maxima_toc.html
#	ebook-convert maxima_singlepage.html maxima.epub --breadth-first "--title=Maxima @manual_version@ Manual" "--authors=Maxima Team" --language=en

# MOVE THIS RULE TO SUBDIR EN
#maxima.pdf: maxima.texi $(maxima_TEXINFOS) $(pdffigures)
#	$(TEXI2PDF) $(AM_V_texinfo) -o maxima.pdf $<
#	rm -f maxima.fns maxima.vr maxima.tp maxima.pg maxima.ky maxima.cp \
#	maxima.toc maxima.fn maxima.aux maxima.log

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)

htmlname = maxima
htmlinstdir = $(dochtmldir)
include $(top_srcdir)/common-html.mk

all-local: warn_texinfo
dist-hook: check_texinfo html
pdf-local: maxima.pdf

# ARE THESE RULES STILL NEEDED ??
#install-data-local: install-maxima-index
#uninstall-local: uninstall-maxima-index
#
#install-maxima-index: maxima-index.lisp
#	$(MKDIR_P) "$(DESTDIR)$(infodir)"
#	$(INSTALL_DATA) maxima-index.lisp "$(DESTDIR)$(infodir)/maxima-index.lisp"
#
#uninstall-maxima-index: maxima-index.lisp
#	rm -f "$(DESTDIR)$(infodir)/maxima-index.lisp"
#
#clean-local: clean-info clean-html
#
#clean-info:
#	rm -f $(wildcard maxima.info* maxima-index.lisp)
#	rm -f maxima.fns
#
#clean-html:
#	rm -f $(wildcard maxima*.html) \
#	      $(wildcard $(addprefix $(srcdir),maxima*.html))

# REVIEW THIS LIST AND EXCLUDE FILES WHICH NO LONGER EXIST IN DOC/INFO !!
EXTRA_DIST = manual.css texi2html.init \
    create_index build_index.pl docdraw.mac \
    extract_categories1.awk extract_categories1.sed check_tab fix_tab \
    maxima-init.lisp README.update_examples \
	category-macros.texi.in category-macros.texi include-maxima.texi

# CONSIDER UPDATING THIS RULE TO LOOK IN LANGUAGE SUBDIRS !!
check_texinfo:
	@pattern=`printf "\t"` ; \
	bad_files=`find . -name '*.texi' -print0 | xargs -0 $(EGREP) -l -e "$$pattern"` ; \
	[ -z  "$$bad_files" ] || ( echo "ERROR: The following files have unexpanded Tabs: $$bad_files" ; \
	echo "Run make distclean and then doc/info/fix_tab to fix the problem." ; exit 1 )

# CONSIDER UPDATING THIS RULE TO LOOK IN LANGUAGE SUBDIRS !!
warn_texinfo:
	@pattern=`printf "\t"` ; \
	bad_files=`find . -name '*.texi' -print0 | xargs -0 $(EGREP) -l -e "$$pattern"` ; \
	[ -z  "$$bad_files" ] || ( echo "WARNING: The following files have unexpanded Tabs: $$bad_files" ; \
	echo "Run doc/info/fix_tab to fix the problem." )

# IS THIS RULE STILL NEEDED ??
distclean-local:
	rm -f figures/Makefile

# IS THIS RULE STILL NEEDED ??
.PHONY: check_texinfo warn_texinfo
