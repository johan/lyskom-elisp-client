# -*- Makefile -*-
# $Id: Makefile,v 44.12 1997-07-02 17:45:49 byers Exp $
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
# $Id: Makefile,v 44.12 1997-07-02 17:45:49 byers Exp $
#

CLIENTVERSION = 0.45-beta

SHELL = /bin/sh

LISPDIR = /usr/gnu/share/emacs/site-lisp
FTPDIR = /usr/ftp/pub/lyskom/elisp-client

GENERIC-CLEAN = *~ *.o core
GENERIC-DIST-CLEAN = TAGS

RM = /usr/gnu/bin/rm -vf

# for compiling using emacs 19!
EMACS=emacs
EMACS-BATCH = $(EMACS) -batch -l ./lpath.el

# NOTE: lyskom-rest.el must be the last file in PARTS-EL and SRC-ELC.
PARTS-EL = komtypes.el clienttypes.el  deferred-insert.el utilities.el \
	buffers.el \
	prefetch.el \
	startup.el \
	reading.el \
	internal.el \
	services.el \
	command.el \
	view-mode.el \
	parse.el cache.el\
	commands1.el commands2.el review.el edit-text.el \
	filter.el filter-edit.el lyskom-buttons.el \
	view-text.el async.el completing-read.el \
	prioritize.el flags.el messages.el ansaphone.el remote-control.el \
	menus.el slow.el \
	elib-string.el  \
	option-edit.el \
	lyskom-rest.el
HEADER-EL = defvar.el vars.el macros.el compatibility.el language.el
HEADER-ELC = defvar.elc vars.elc macros.elc compatibility.elc language.elc
SWEDISH-EL = swedish-strings.el
SWEDISH-ELC = swedish-strings.elc
ENGLISH-EL = english-strings.el
ENGLISH-ELC = english-strings.elc
SRC-ELC = komtypes.elc clienttypes.elc deferred-insert.elc utilities.elc \
	buffers.elc \
	prefetch.elc \
	startup.elc \
	reading.elc \
	internal.elc \
	services.elc \
	command.elc \
	vire-mode.elc \
	parse.elc cache.elc\
	commands1.elc commands2.elc review.elc edit-text.elc \
	filter.elc filter-edit.elc lyskom-buttons.elc \
	view-text.elc async.elc completing-read.elc \
	prioritize.elc flags.elc messages.elc ansaphone.elc \
	remote-control.elc menus.elc slow.elc elib-string.elc \
	option-edit.elc \
	lyskom-rest.elc 
PARTS-ELC = $(SRC-ELC)


# The default target
all: lyskom.elc

.SUFFIXES : .el .elc
.el.elc:
	$(EMACS-BATCH) -f batch-byte-compile $*.el

# influenced by autoconf? Yes.
vars.el: vars.el.in Makefile
	rm -f vars.el
	sed 's/@@CLIENTVERSION@@/$(CLIENTVERSION)/' < vars.el.in > vars.el
	chmod 444 vars.el

lyskom.el: $(HEADER-EL) $(SWEDISH-EL) $(ENGLISH-EL) $(PARTS-EL)
	cat $(HEADER-EL) $(SWEDISH-EL) $(ENGLISH-EL) $(PARTS-EL) > lyskom.el

lyskom.elc: lyskom.el
	$(EMACS-BATCH) -l ./lpath.el -f batch-byte-compile lyskom.el

.PHONY: check
check: lyskom.el
	$(EMACS-BATCH) -l ./lyskom.el  -l ./check-strings.el -f lyskom-check-strings

# Do "make verbose" to see where things go wrong
.PHONY: verbose
verbose: verbose-el lyskom.elc

.PHONY: verbose-el
verbose-el: $(HEADER-EL) $(SWEDISH-EL) $(ENGLISH-EL) $(PARTS-EL)
	@echo "Building lyskom.el"
	@echo "" > lyskom.el
	@for i in $(HEADER-EL) $(SWEDISH-EL) $(ENGLISH-EL) $(PARTS-EL) ; do \
		echo '(eval-when-compile (message "Compiling %s" "'$$i'"))' >> lyskom.el ;\
		cat $$i >> lyskom.el ; \
	done 

# English version
english-lyskom.el: $(HEADER-EL) $(ENGLISH-EL) $(PARTS-EL)
	cat $(HEADER-EL) $(ENGLISH-EL) $(PARTS-EL) > english-lyskom.el

english-lyskom.elc: english-lyskom.el
	$(EMACS-BATCH) -f batch-byte-compile english-lyskom.el

# The method of compiling elc files separately is now depreceated
# I dont want any warnings of the newer file...
vars.elc: vars.el macros.elc
	$(EMACS-BATCH) -f batch-byte-compile $*.el

macros.elc: macros.el
	$(EMACS-BATCH) -f batch-byte-compile $*.el

# For fast compiling
.PHONY: fast
fast: $(HEADER-ELC) $(SWEDISH-ELC) $(PARTS-ELC)
	cat $(HEADER-ELC) $(SWEDISH-ELC) $(PARTS-ELC) > lyskom.elc

$(SRC-ELC) $(SWEDISH-ELC) $(ENGLISH-ELC): macros.elc

elc-files: $(HEADER-ELC) $(PARTS-ELC) 


lyskom-$(CLIENTVERSION).elc: lyskom.elc
	cp lyskom.elc lyskom-$(CLIENTVERSION).elc

lyskom.elc-compatibility: lyskom.el
	$(EMACS-BATCH) -l ./compatibility.el -f batch-byte-compile lyskom.el
	mv lyskom.elc lyskom.elc-compatibility

lyskom-$(CLIENTVERSION).el.gz: lyskom.el
	gzip -9 < lyskom.el > lyskom-$(CLIENTVERSION).el.gz

lyskom-$(CLIENTVERSION).elc.gz: lyskom-$(CLIENTVERSION).elc
	gzip -9 < lyskom-$(CLIENTVERSION).elc > lyskom-$(CLIENTVERSION).elc.gz


english-lyskom-$(CLIENTVERSION).el.gz: english-lyskom.el
	gzip -9 < english-lyskom.el > english-lyskom-$(CLIENTVERSION).el.gz



#Detta kr{ver gnu-make. Tyv{rr funkade inte $(SRC-ELC)-prylen l{ngre ner alls.
#Kanske denna $(SRC-ELC) pryl fungerar.
#$(SRC-ELC) :%.elc: %.el macros.elc
#	$(EMACS-BATCH) -l ./macros.elc -f batch-byte-compile $<

#$(SRC-ELC):
#	$(EMACS-BATCH) -l ./macros.el -f batch-byte-compile $*.el


wc:
	@wc $(HEADER-EL) $(PARTS-EL) $(SWEDISH-EL) $(ENGLISH-EL)
	@wc ChangeLog ../misc/todo

TAGS: $(HEADER-EL) $(PARTS-EL) $(SWEDISH-EL) $(ENGLISH-EL)
	etags $(HEADER-EL) $(PARTS-EL) $(SWEDISH-EL) $(ENGLISH-EL)

install: lyskom.elc lyskom.el
	cp lyskom.el lyskom.elc $(LISPDIR)

release: lyskom-$(CLIENTVERSION).el.gz english-lyskom-$(CLIENTVERSION).el.gz
	cp lyskom-$(CLIENTVERSION).el.gz $(FTPDIR)
	cp english-lyskom-$(CLIENTVERSION).el.gz $(FTPDIR)

clean:
	$(RM) *.el.gz *.elc.gz lyskom.el english-lyskom.el
	$(RM) *lyskom-$(CLIENTVERSION).*
	$(RM) *~ *.elc $(GENERIC-CLEAN)

distclean: clean
	$(RM) $(GENERIC-DIST-CLEAN) vars.el
