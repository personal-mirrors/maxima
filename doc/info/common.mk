## Makefile snippets that are relevant, both in the top-level info
## dir, and in the translation subdirectories. This file is included
## via "include" by the relevant Makefile.am so we can use things like
## Automake conditionals and substitutions.
##
## TODO: The language-specific targets currently use an explicit call
##       to texi2html to generate the html files, whereas en has
##       extract_categories.sh. These should probably do the same?

langsdir_tmp := $(shell basename $(shell pwd))
langsdir := $(if $(findstring info,$(langsdir_tmp)),,$(langsdir_tmp))
lang := $(if $(findstring info,$(langsdir_tmp)),en,$(langsdir_tmp))

## Where to actually install the info files (and offset lists)
lang_info_dir := $(infodir)/$(langsdir)

PHONY_TARGETS=
.PHONY: $(PHONY_TARGETS)

## Autoconf local targets ############################################
all-local: info-offsets $(MAXIMA_CHM) warn_texinfo
install-data-local: install-offsets $(INSTALL_CHM) $(INSTALL_INFO)
uninstall-local: uninstall-offsets $(UNINSTALL_CHM) $(UNINSTALL_INFO)
## CLEAN_TEXI is set in common-lang-recode.mk for when we're in a
## recoded subdirectory.
clean-local: clean-info clean-offsets clean-html $(CLEAN_CHM) $(CLEAN_RECODE)
dist-hook: check_texinfo html

## CHM Targets #######################################################
if CHM
MAXIMA_CHM = maxima.chm
INSTALL_CHM = install-chm
UNINSTALL_CHM = uninstall-chm
CLEAN_CHM = clean-chm

## We make the .chm in a "chm" temporary directory. If $(langsdir) is
## "" then we're in the en top-level directory. If it has positive
## length, we're in a subdirectory, so we need to adjust the pathnames
## in the html (with the sed call).
maxima.chm: maxima.html maxima.hhp contents.hhc index.hhk
	$(MKDIR_P) chm/figures
	if test -z $(langsdir); then \
	  cp *.html chm \
	  cp figures/*.gif chm/figures \
        else \
	  for hfile in *.html ; do \
	    sed -e 's|../figures|figures|g' < $$hfile > chm/$$hfile; \
	  done \
	  cp ../figures/*.gif chm/figures \
	fi
	cp maxima.hhp contents.hhc index.hhk chm
	-(cd chm; "$(HHC)" maxima.hhp)
	mv chm/maxima.chm .

clean-chm:
	rm -f maxima.chm
	rm -rf chm

install-chm: maxima.chm
	$(MKDIR_P) "$(docchmdir)/$(langsdir)"
	$(INSTALL_DATA) maxima.chm "$(docchmdir)/$(langsdir)"

uninstall-chm:
	rm -f "$(docchmdir)/$(langsdir)/maxima.chm"
	rmdir "$(docchmdir)/$(langsdir)"

PHONY_TARGETS += clean-chm install-chm uninstall-chm

endif ## CHM

## HTML Targets ######################################################
##   (see TODO note at top: maxima.html is defined in en's Makefile.am
##    and in common-lang.mk for other languages)
html: maxima.html contents.hhc
contents.hhc index.hhk: maxima.html
	perl $(top_srcdir)/doc/info/create_index

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)/$(langsdir)
genericdirDATA = \
contents.hhc index.hhk header.hhp maxima.hhp

htmlname = maxima
htmlinstdir = $(dochtmldir)/$(langsdir)
include $(top_srcdir)/common-html.mk

EXTRA_DIST = $(genericdirDATA)

clean-html:
	rm -f maxima.html maxima_*.html
	rm -f contents.hhc
	rm -f index.hhk

PHONY_TARGETS += clean-html

## Info file stuff ###################################################
clean-info:
	rm -f maxima.info*

PHONY_TARGETS += clean-info

## Info offset files generation ######################################
info_offset_files=$(foreach x,@BUILT_LISPS@,maxima-info-offsets-$(x).lisp)
info-offsets: maxima.info $(info_offset_files)

# (the extra echo is because not all the lisps finish by outputting a
# newline)
maxima-info-offsets-%.lisp: maxima.info
	$(top_srcdir)/lisp-utils/parse-info.sh $* maxima.info \
	  maxima-info-offsets-$*.lisp
	@echo

install-offsets: info-offsets
	$(MKDIR_P) "$(lang_info_dir)"
	$(INSTALL_DATA) -t "$(lang_info_dir)" $(info_offset_files)

uninstall-offsets:
	rm -f $(foreach f,$(info_offset_files),"$(lang_info_dir)/$(f)")

clean-offsets:
	rm -f maxima-info-offsets-*.lisp

PHONY_TARGETS += info-offsets install-offsets uninstall-offsets clean-offsets

## Error checking ####################################################
find_bad_files=$$(find . -name '*.texi' -print | xargs $(EGREP) -l -e "$$pattern")
echo_error_for=echo "ERROR: The following files have $(1): $$bad_files"
texinfo_echo_soln=echo "Run $(1)/doc/info/$(2) to fix the problem."
texinfo_complain=($(call echo_error_for,$(1)); $(call texinfo_echo_soln,$(2),$(3)); exit 1)
texinfo_check= \
  pattern=$$(printf $(1));     \
  bad_files=$(find_bad_files); \
  test -z $$bad_files || $(call texinfo_complain,$(2),$(3),$(4))

check_texinfo:
	@echo -n "Checking for CR/LF pairs or unexpanded tabs... "
	@$(call texinfo_check,"\r$$","DOS-style EOLs","make distclean and then ",fix_crlf)
	@$(call texinfo_check,"\t","unexpanded Tabs","make distclean and then ",fix_crlf)
	@echo "none found."

warn_texinfo:
	@echo -n "Checking for CR/LF pairs or unexpanded tabs... "
	@$(call texinfo_check,"\r$$","DOS-style EOLs","",fix_tab)
	@$(call texinfo_check,"\t","unexpanded Tabs","",fix_tab)
	@echo "none found."

PHONY_TARGETS += check_texinfo warn_texinfo
