# -*- Makefile -*-
# $Id: Makefile,v 44.35 1999-10-13 12:36:45 byers Exp $
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
# $Id: Makefile,v 44.35 1999-10-13 12:36:45 byers Exp $
#


# ======================================================================
# Useful targets: all, lyskom.el lyskom.elc check verbose TAGS release
# clean distclean install
# ======================================================================

#
# NOTE: If you change this file, don't forget to change the distribution 
# makefile too!
#

CLIENTVERSION = 0.46-BETA-9
DOCFILES=NEWS-0.46

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

LANGUAGE-EL=$(LANGUAGES:=-strings.el)
EMACS-BATCH=$(EMACS) -batch
SOURCES	 = 	$(LANGUAGE-EL) \
		komtypes.el \
		clienttypes.el \
		deferred-insert.el \
		utilities.el \
		command.el \
		buffers.el \
		aux-items.el \
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


all: lyskom.elc

vars.el: vars.el.in Makefile
	rm -f vars.el
	sed 's/@@CLIENTVERSION@@/$(CLIENTVERSION)/' < vars.el.in > vars.el
	chmod 444 vars.el

%.elc: %.el header.el
	$(EMACS-BATCH)  -l ./lpath.el -l ./header.el -f batch-byte-compile $<

header.el: $(HEADER)
	cat $(HEADER) > header.el

fast: $(HEADER-ELC) $(SOURCES-ELC)
	cat $(HEADER-ELC) $(SOURCES-ELC) > lyskom.elc

lyskom.el: $(HEADER) $(SOURCES)
	cat $(HEADER) $(SOURCES) > lyskom.el

lyskom.elc: lyskom.el
	$(EMACS-BATCH) -l ./lpath.el -f batch-byte-compile lyskom.el

check: lyskom.el
	$(EMACS-BATCH) -l ./lpath.el -l ./lyskom.el  -l ./check-strings.el \
			-f lyskom-check-strings

.PHONY: verbose
.PHONY: verbose-el

verbose: verbose-el lyskom.elc

verbose-el: $(HEADER) $(SOURCES)
	@echo "Building lyskom.el"
	@echo "" > lyskom.el
	@for i in $(HEADER) $(SOURCES) ; do \
		echo '(eval-when-compile (message "Compiling %s" "'$$i'"))' \
			>> lyskom.el ;\
		cat $$i >> lyskom.el ; \
	done 


TAGS: $(HEADER) $(SOURCES)
	etags $(HEADER) $(SOURCES)

install: lyskom.elc lyskom.el
	cp lyskom.el lyskom.elc $(LISPDIR)

release: $(HEADER) $(SOURCES) $(DISTSRC) $(MISC) lyskom.el
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
	cp lyskom-$(CLIENTVERSION).tar.gz $(FTPDIR)
	rm -rf lyskom-$(CLIENTVERSION)
	rm -rf lyskom-$(CLIENTVERSION).tar.gz
	mv lyskom.el $(FTPDIR)/lyskom-$(CLIENTVERSION).el

clean:
	$(RM) *.el.gz *.elc.gz lyskom.el english-lyskom.el
	$(RM) -rf lyskom-$(CLIENTVERSION) \
		  lyskom-$(CLIENTVERSION).tar \
		  lyskom-$(CLIENTVERSION).tar.gz
	$(RM) *~ *.elc $(GENERIC-CLEAN)

distclean: clean
	$(RM) $(GENERIC-DIST-CLEAN) vars.el
