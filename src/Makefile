# Copyright (C) 1991-2002  Lysator Academic Computer Association.
#
# This file is part of the LysKOM Emacs LISP client.
# 
# LysKOM is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by 
# the Free Software Foundation; either version 2, or (at your option) 
# any later version.
# 
# LysKOM is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with LysKOM; see the file COPYING.  If not, write to
# Lysator, c/o ISY, Linkoping University, S-581 83 Linkoping, SWEDEN,
# or the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, 
# MA 02139, USA.
#
# Please mail bug reports to bug-lyskom@lysator.liu.se. 
#
# $Id: Makefile,v 44.113 2004-07-03 16:17:53 qha Exp $

# Useful targets:
#
#   all      - Build lyskom.el from source files and compile it with $(EMACS).
#   verbose  - Same as above, but also print some progress messages.
#   fast     - Separately compile each uncompiled source file with $(EMACS)
#              and produce lyskom.elc from the results.
#   check    - Perform test cases.
#   release  - Make distribution archives.

######################################################################

# Version of the client.
#
# Also remember to change version number in ../doc/Makefile!
CLIENTVERSION = 0.47.1

######################################################################

# Default Emacs to use for compilation by developers.
EMACS = xemacs

# The available languages.
LANGUAGES = swedish english

# These files are included in all distribution archives.
DOCFILES = ../doc/NEWS-0.47 ../doc/NEWS-0.47.1

# Default target.
TARGET = lyskom.elc
TARGET-EL := $(TARGET:%.elc=%.el)

# These will be placed first in lyskom.el.
HEADER = 	header.el \
		defvar.el \
		feature.el \
		$(VARS-EL) \
		macros.el \
		compatibility.el \
		language.el \
		help.el


# Next come the language definitions.
LANGUAGE-EL := $(LANGUAGES:=-strings.el) $(LANGUAGES:=-help.el)

# Finally, other source files.
SOURCES	 = 	komtypes.el \
		clienttypes.el \
		faces.el \
		deferred-insert.el \
		utilities.el \
		completing-read.el \
		keyboard-menu.el \
		command.el \
		buffers.el \
		aux-items.el \
		mime.el \
		prefetch.el \
		startup.el \
		reading.el \
		internal.el \
		services.el \
		parse.el \
		cache.el \
		view-mode.el \
		names.el \
		commands1.el \
		commands2.el \
		review.el \
		edit-text.el \
		filter.el \
		filter-edit.el \
		lyskom-buttons.el \
		view-text.el \
		async.el \
		mship-edit.el \
		prioritize.el \
		flags.el \
		messages.el \
		ansaphone.el \
		remote-control.el \
		menus.el slow.el \
		elib-string.el \
		option-edit.el \
		talkback.el \
		faqs.el \
		user-hooks.el \
		lyskom-rest.el
                # lyskom-rest.el must be the last file!

# Included in the source distribution archive.
EXTRA-SRCDIST = \
	distribution-COPYING distribution-configure.in \
	distribution-Makefile.in
EXTRA-SRCVERBATIM = \
	envcheck.el lpath.el autoconf.el \
	help-swedish.xml help-english.xml \
	help-compile.el \
	$(DOCFILES)

# Included in the non-source distribution archives.
EXTRA-NONSRCDIST = \
	distribution-COPYING
EXTRA-NONSRCVERBATIM = \
	$(DOCFILES)

# Base name of distribution packages.
PACKAGENAME = lyskom-elisp-client

# Default paths in the source dist Makefile.
DEFAULTLISPDIR = /usr/local/share/emacs/site-lisp
DEFAULTPREFIX = /usr/local

ifeq ($(MAKECMDGOALS),release)
    VARS-EL = vars.el-release
else
    VARS-EL = vars.el
endif

EMACS-BATCH := $(EMACS) -batch

all: $(TARGET)

vars.el: vars.el.in Makefile
	rm -f $@
	sed "s/@@CLIENTVERSION@@/$(CLIENTVERSION) (`date +%Y-%m-%d`; CVS)/g" <$< >$@
	chmod 444 $@

vars.el-release: vars.el.in Makefile
	sed 's/@@CLIENTVERSION@@/$(CLIENTVERSION)/g' <$< >$@

$(TARGET-EL) lyskom-all-$(CLIENTVERSION).el: \
		$(HEADER) $(LANGUAGE-EL) $(SOURCES)
	rm -f $(VARS-EL)
	$(MAKE) $(VARS-EL)
	echo ";;;;; -*-coding: x-ctext;-*-" >$@
	cat $^ >>$@

%-help.el: help-%.xml
	$(EMACS) -batch -l help-compile.el $^ -f lyskom-help-compile-to-el

lyskom-%-$(CLIENTVERSION).el: $(HEADER) %-strings.el %-help.el $(SOURCES)
	echo ";;;;; -*-coding: x-ctext;-*-" >$@
	cat $^ >>$@

verbose: verbose-el $(TARGET)

verbose-el: $(HEADER) $(LANGUAGE-EL) $(SOURCES)
	echo ";;;;; -*-coding: x-ctext;-*-" >$(TARGET-EL)
	for i in $^ ; do \
		echo '(eval-when-compile (message "Compiling %s" "'$$i'"))' \
			>>$(TARGET-EL) ; \
		cat $$i >>$(TARGET-EL) ; \
	    done

%.elc: %.el _header.el
	$(EMACS-BATCH) -l lpath.el -l _header.el -f batch-byte-compile $<

$(TARGET): $(TARGET-EL)
	LYSKOM_DEBUG_COMPILE=yes $(EMACS-BATCH) -l lpath.el -f batch-byte-compile $<

HEADER-ELC = $(HEADER:%.el=%.elc)
LANGUAGE-ELC = $(LANGUAGE-EL:.el%.elc)
SOURCES-ELC = $(SOURCES:%.el=%.elc)

_header.el: $(HEADER)
	echo ";;;;; -*-coding: x-ctext;-*-" >_header.el
	cat $(HEADER) >>_header.el

fast: $(HEADER-ELC) $(LANGUAGE-ELC) $(SOURCES-ELC)
	cat $^ >$(TARGET)


check: $(TARGET-EL)
	$(EMACS-BATCH) -l lpath.el -l $< -l check-strings.el \
			-f lyskom-check-strings

TAGS: $(HEADER) $(LANGUAGE-EL) $(SOURCES)
	etags $^

distribution-README-%: \
		distribution-README.begin \
		distribution-README.install-% \
		distribution-README.end
	cat $^ >$@

RELEASE-DIST-FLAVOURS := src all $(LANGUAGES)

RELEASE-DIST-ARCHIVES := \
    $(patsubst %,$(PACKAGENAME)-%-$(CLIENTVERSION).tar.gz,$(RELEASE-DIST-FLAVOURS)) \
    $(patsubst %,$(PACKAGENAME)-%-$(CLIENTVERSION).zip,$(RELEASE-DIST-FLAVOURS))

release: $(VARS-EL) $(RELEASE-DIST-ARCHIVES)
	@echo
	@echo
	@echo "Release archives created:"
	@echo
	@for i in $(RELEASE-DIST-ARCHIVES); do \
		echo "    $$i" ; \
	    done
	@echo

%.tar: %.buildstamp
	tar cvf $@ $*

%.gz: %
	gzip -f --best $<

%.zip: %.buildstamp
	zip -R9y $@ $*/*

############################################################
# Source dist.

RELEASE-SOURCE-SRC = $(HEADER) $(LANGUAGE-EL) $(SOURCES) $(MISC)
RELEASE-BASE-SRC = $(PACKAGENAME)-src-$(CLIENTVERSION)

$(RELEASE-BASE-SRC).buildstamp: \
		$(RELEASE-SOURCE-SRC) $(EXTRA-SRCDIST) $(EXTRA-SRCVERBATIM) \
		distribution-README-src
	mkdir -p $(RELEASE-BASE-SRC)
	cp $(RELEASE-SOURCE-SRC) $(RELEASE-BASE-SRC)
	sed 's/@@CLIENTVERSION@@/$(CLIENTVERSION)/g' \
		<distribution-README-src \
		>$(RELEASE-BASE-SRC)/README
	for i in $(subst distribution-,,$(EXTRA-SRCDIST)); do \
	    sed -e 's/@@CLIENTVERSION@@/$(CLIENTVERSION)/g' \
		-e 's/@@HEADER@@/$(HEADER)/g' \
		-e 's/@@SOURCES@@/$(SOURCES)/g' \
		<distribution-$$i \
		>$(RELEASE-BASE-SRC)/$$i ; \
	    done
	cp $(EXTRA-SRCVERBATIM) $(RELEASE-BASE-SRC)
	sed -e "s/#CLIENTVERSION#/$(CLIENTVERSION)/" \
	    -e "s/#REDHATCLIENTVERSION#/$(REDHATCLIENTVERSION)/" \
	    <$(PACKAGENAME).spec \
	    >$(RELEASE-BASE-SRC)/$(PACKAGENAME)-$(REDHATCLIENTVERSION).spec
	cd $(RELEASE-BASE-SRC) && autoconf
	cd $(RELEASE-BASE-SRC) && ./configure --prefix=$(DEFAULTPREFIX) \
					      --with-lispdir=$(DEFAULTLISPDIR)
	cd $(RELEASE-BASE-SRC) && rm -f config.log config.status config.cache
	touch $@

############################################################
# Normal dist.

RELEASE-BASE-NONSRC = $(PACKAGENAME)-$*-$(CLIENTVERSION)

$(PACKAGENAME)-%-$(CLIENTVERSION).buildstamp: \
		lyskom-%-$(CLIENTVERSION).el $(EXTRA-NONSRCDIST) \
		distribution-README-nonsrc
	mkdir -p $(RELEASE-BASE-NONSRC)
	cp $< $(RELEASE-BASE-NONSRC)
	sed -e 's/@@CLIENTVERSION@@/$(CLIENTVERSION)/g' \
	    -e 's/@@FLAVOUR@@/$*/g' \
		<distribution-README-nonsrc \
		>$(RELEASE-BASE-NONSRC)/README
	for i in $(subst distribution-,,$(EXTRA-NONSRCDIST)); do \
	    sed 's/@@CLIENTVERSION@@/$(CLIENTVERSION)/g' \
		<distribution-$$i \
		>$(RELEASE-BASE-NONSRC)/$$i ; \
	    done
	cp $(EXTRA-NONSRCVERBATIM) $(RELEASE-BASE-NONSRC)
	touch $@

######################################################################
# RPM packages

REDHATCLIENTVERSION := $(subst -,.,$(CLIENTVERSION))
RPMROOT = /usr/src/redhat

redhat-packages: $(PACKAGENAME)-src-$(CLIENTVERSION).tar.gz
	cp $< $(RPMROOT)/SOURCES/
	sed -e "s/#CLIENTVERSION#/$(CLIENTVERSION)/" \
	    -e "s/#REDHATCLIENTVERSION#/$(REDHATCLIENTVERSION)/" \
	    <$(PACKAGENAME).spec \
	    >$(RPMROOT)/SPECS/$(PACKAGENAME)-$(REDHATCLIENTVERSION).spec
	rpm -ba $(RPMROOT)/SPECS/$(PACKAGENAME)-$(REDHATCLIENTVERSION).spec

######################################################################
# Debian packages

# Keeping the suffix in it's own variable lets you easily replace it
# from the commandline. This is useful when you're building a package
# just for your self and don't want it replaced by the package in the
# distribution. (make DEBIANCLIENTVERSIONSUFFIX=qha1 debian-packages).
DEBIANCLIENTVERSIONSUFFIX = 1

# and when you're the only one who'll use the package and you're not
# really the maintainer you might want to set this to something like
# '-us -uc' and perhaps -rfakeroot aswell.
DPKG_BUILDPACKAGE_OPTIONS =

DEBIANCLIENTVERSION := $(subst -,.,$(CLIENTVERSION))

debian-packages: $(RELEASE-BASE-SRC).buildstamp
	cp -R debian $(RELEASE-BASE-SRC)
	$(RM) -rf $(RELEASE-BASE-SRC)/debian/CVS
	sed '1s/(.*)/($(DEBIANCLIENTVERSION)-$(DEBIANCLIENTVERSIONSUFFIX))/' <debian/changelog >$(RELEASE-BASE-SRC)/debian/changelog
	sed 's/@@CLIENTVERSION@@/$(CLIENTVERSION)/g' <debian/rules >$(RELEASE-BASE-SRC)/debian/rules
	cd $(RELEASE-BASE-SRC) && dpkg-buildpackage $(DPKG_BUILDPACKAGE_OPTIONS)


clean:
	rm -f *~ *.gz *.zip *.buildstamp *.elc* _header.el $(TARGET-EL)
	rm -rf $(PACKAGENAME)-*-$(CLIENTVERSION)* lyskom-*-$(CLIENTVERSION).el
	rm -f vars.el vars.el-release
	rm -f distribution-README-src distribution-README-nonsrc

.PHONY: all verbose verbose-el fast check release redhat-packages clean debian-packages
