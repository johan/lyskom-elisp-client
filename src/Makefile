# -*- Makefile -*-
# $Id: Makefile,v 44.54 2000-08-11 15:35:03 byers Exp $
# Copyright (C) 1991, 1996  Lysator Academic Computer Association.
#
# This file is part of the LysKOM server.
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
# $Id: Makefile,v 44.54 2000-08-11 15:35:03 byers Exp $
#


# ======================================================================
# Useful targets: all, lyskom.el lyskom.elc check verbose TAGS release
# clean distclean install
# ======================================================================

#
# NOTE: If you change this file, don't forget to change the distribution 
# makefile too!
#

CLIENTVERSION = 0.46-BETA-L
DOCFILES=NEWS-0.46
DEBIANCLIENTVERSION = $(shell echo $(CLIENTVERSION) | tr - .)

FTPDIR=/home/davby/www-pub
#FTPDIR = /usr/ftp/pub/lyskom/elisp-client
LISPDIR = /usr/gnu/share/emacs/site-lisp
LANGUAGES = swedish english

SHELL = /bin/sh
RM = /bin/rm -f
EMACS=xemacs
EMACS-BATCH = $(EMACS) -batch
GENERIC-CLEAN = *~ *.o core
GENERIC-DIST-CLEAN = TAGS

TARGET=lyskom.elc
TARGET-EL=$(TARGET:%.elc=%.el)

LANGUAGE-EL=$(LANGUAGES:=-strings.el)
SOURCES	 = 	$(LANGUAGE-EL) \
		komtypes.el \
		clienttypes.el \
		deferred-insert.el \
		utilities.el \
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
		commands1.el \
		commands2.el \
		review.el \
		edit-text.el \
		filter.el \
		filter-edit.el \
		lyskom-buttons.el \
		view-text.el \
		async.el \
		completing-read.el \
		mship-edit.el \
		prioritize.el \
		flags.el \
		messages.el \
		ansaphone.el \
		remote-control.el \
		menus.el slow.el \
		elib-string.el \
		option-edit.el \
		lyskom-rest.el

HEADER = 	defvar.el \
		feature.el \
		vars.el \
		macros.el \
		compatibility.el \
		language.el

MISC =		envcheck.el \
		lpath.el

DISTFILES =     Makefile \
		README \
		COPYING

DISTSRC = $(DISTFILES:%=distribution-%)
HEADER-ELC = $(HEADER:%.el=%.elc)
SOURCES-ELC = $(SOURCES:%.el=%.elc)


all: $(TARGET)

vars.el: vars.el.in Makefile
	rm -f vars.el
	sed 's/@@CLIENTVERSION@@/$(CLIENTVERSION)/' < vars.el.in > vars.el
	chmod 444 vars.el

%.elc: %.el header.el
	$(EMACS-BATCH)  -l ./lpath.el -l ./header.el -f batch-byte-compile $<

header.el: $(HEADER)
	cat $(HEADER) > header.el

fast: $(HEADER-ELC) $(SOURCES-ELC)
	cat $(HEADER-ELC) $(SOURCES-ELC) > $(TARGET)

$(TARGET-EL): $(HEADER) $(SOURCES)
	cat $(HEADER) $(SOURCES) > $(TARGET-EL)

$(TARGET): $(TARGET-EL)
	$(EMACS-BATCH) -l ./lpath.el -f batch-byte-compile $(TARGET-EL)

check: $(TARGET-EL)
	$(EMACS-BATCH) -l ./lpath.el -l ./$(TARGET-EL)  -l ./check-strings.el \
			-f lyskom-check-strings

.PHONY: verbose
.PHONY: verbose-el

verbose: verbose-el $(TARGET)

verbose-el: $(HEADER) $(SOURCES)
	@echo "Building lyskom.el"
	@echo "" > $(TARGET-EL)
	@for i in $(HEADER) $(SOURCES) ; do \
		echo '(eval-when-compile (message "Compiling %s" "'$$i'"))' \
			>> $(TARGET-EL) ;\
		cat $$i >> $(TARGET-EL) ; \
	done 


TAGS: $(HEADER) $(SOURCES)
	etags $(HEADER) $(SOURCES)

install: $(TARGET) $(TARGET-EL)
	cp $(TARGET-EL) $(TARGET) $(LISPDIR)

release: release-files release-cleanup release-move

release-files: $(HEADER) $(SOURCES) $(DISTSRC) $(MISC) $(TARGET-EL)
	mkdir lyskom-$(CLIENTVERSION)
	cp $(HEADER) $(SOURCES) $(MISC) lyskom-$(CLIENTVERSION)
	for i in $(DISTFILES) ; do \
		cp distribution-$$i lyskom-$(CLIENTVERSION)/$$i ; \
	done
	for i in $(DOCFILES) ; do \
		cp ../doc/$$i lyskom-$(CLIENTVERSION)/$$i ; \
	done
	chmod -R u+rw,g+r,o+r lyskom-$(CLIENTVERSION)
	tar cvf lyskom-$(CLIENTVERSION).tar lyskom-$(CLIENTVERSION)
	gzip -9 lyskom-$(CLIENTVERSION).tar

release-cleanup:
	$(RM) -rf lyskom-$(CLIENTVERSION) lyskom-elisp-client-$(DEBIANCLIENTVERSION)

release-move: release-files
	if [ -d $(FTPDIR) ]; then \
		mv lyskom-$(CLIENTVERSION).tar.gz $(FTPDIR) ; \
		mv $(TARGET-EL) $(FTPDIR)/lyskom-$(CLIENTVERSION).el ; \
	else \
		echo "Flyttade inte release-filerna till $(FTPDIR)." ; \
	fi

debian-package: release-files debian-build release-cleanup

debian-build: release-files
	cp -R debian lyskom-$(CLIENTVERSION)
	$(RM) -rf lyskom-$(CLIENTVERSION)/debian/CVS
	perl -pi -e 's/\((.+)\)/($(DEBIANCLIENTVERSION)-1)/ if (1..1)' lyskom-$(CLIENTVERSION)/debian/changelog
	mv lyskom-$(CLIENTVERSION) lyskom-elisp-client-$(DEBIANCLIENTVERSION)
	cp lyskom-$(CLIENTVERSION).tar.gz lyskom-elisp-client_$(DEBIANCLIENTVERSION).orig.tar.gz
	cd lyskom-elisp-client-$(DEBIANCLIENTVERSION) && dpkg-buildpackage

clean:
	$(RM) *.el.gz *.elc.gz $(TARGET-EL)
	$(RM) -rf lyskom-$(CLIENTVERSION) \
		  lyskom-$(CLIENTVERSION).tar \
		  lyskom-$(CLIENTVERSION).tar.gz \
		  lyskom-elisp-client-$(DEBIANCLIENTVERSION)
	$(RM) *~ *.elc $(GENERIC-CLEAN)
	$(RM) -r build-stamp lyskom-elisp-client*

distclean: clean
	$(RM) $(GENERIC-DIST-CLEAN) vars.el
