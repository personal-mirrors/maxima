# versioned installation directories

verpkglibdir = $(pkglibdir)/@VERSION@
verpkglibexecdir = $(libexecdir)/@PACKAGE@/@VERSION@
verpkgdatadir = $(pkgdatadir)/@VERSION@
docdir = $(verpkgdatadir)/doc
docchmdir = $(docdir)/chm
dochtmldir = $(docdir)/html
docsharedir = $(docdir)/share
demodir = $(verpkgdatadir)/demo
emacsdir = $(verpkgdatadir)/emacs
sharedir = $(verpkgdatadir)/share
instsrcdir = $(verpkgdatadir)/src
xmaximadir = $(verpkgdatadir)/xmaxima
insttestsdir = $(verpkgdatadir)/tests

# Support for installation of DATA files in a generic directory
# with subdirectories.
# To use, set genericdir to point to the installation directory.
# Set genericdirDATA to hold the list of files to install.
# genericdirDATA may contain subdirectories. Subdirectories will
# be created if necessary.

install-data-local: install-datafiles
install-datafiles: $(genericdirDATA)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(genericdir)
	@list='$(genericdirDATA)'; for p in $$list; do \
	  if test -f $(srcdir)/$$p; then \
            if test ! -d `dirname $(DESTDIR)$(genericdir)/$$p`; then \
              $(mkinstalldirs) `dirname $(DESTDIR)$(genericdir)/$$p`; \
            fi; \
	    echo " $(INSTALL_DATA) $(srcdir)/$$p $(DESTDIR)$(genericdir)/$$p"; \
	    $(INSTALL_DATA) $(srcdir)/$$p $(DESTDIR)$(genericdir)/$$p; \
	  else if test -f $$p; then \
            if test ! -d `dirname $(DESTDIR)$(genericdir)/$$p`; then \
              $(mkinstalldirs) `dirname $(DESTDIR)$(genericdir)/$$p`; \
            fi; \
	    echo " $(INSTALL_DATA) $$p $(DESTDIR)$(genericdir)/$$p"; \
	    $(INSTALL_DATA) $$p $(DESTDIR)$(genericdir)/$$p; \
	  fi; fi; \
	done

uninstall-local: uninstall-datafiles
uninstall-datafiles:
	@$(NORMAL_UNINSTALL)
	list='$(genericdirDATA)'; for p in $$list; do \
	  rm -f $(DESTDIR)$(genericdir)/$$p; \
	done

# Functions to run the various lisp implementations. Each takes a
# single argument which should be a form to run.
run_clisp = "$(CLISP_NAME)" -norc -q -x $(1)
run_cmucl = echo $(1) | "$(CMUCL_NAME)" -noinit -batch
run_scl = echo $(1) | "$(SCL_NAME)" -noinit -batch
run_acl = echo $(1) | "$(ACL_NAME)" -batch
run_sbcl = "$(SBCL_NAME)" --noinform --noprint --eval $(1) --eval '(sb-ext:quit)'
run_gcl = "$(GCL_NAME)" -batch -eval $(1)
run_ccl = echo $(1) | "$(OPENMCL_NAME)" -b
run_ecl = "$(ECL_NAME)" -norc -eval $(1) -eval '(ext:quit)'

## A rule to build binary directories of the form
## binary-<lispname>. If you wish to make some subdirs as well, set
## $(binary_subdirs). (See src/Makefile.am, for example)
bd_%:
	$(MKDIR_P) $(addprefix $(subst bd_,binary-,$@),/ $(binary_subdirs))
