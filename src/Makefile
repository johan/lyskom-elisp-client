#
# $Id: Makefile,v 38.1 1994-01-10 18:07:53 linus Exp $
# Copyright (C) 1991  Lysator Academic Computer Association.
#
# This file is part of the LysKOM server.
# 
# LysKOM is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by 
# the Free Software Foundation; either version 1, or (at your option) 
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
# $Id: Makefile,v 38.1 1994-01-10 18:07:53 linus Exp $
# $Log: Makefile,v $
# Revision 38.1  1994-01-10 18:07:53  linus
# The installation method changed (again).
# The version is now contained in the Makefile.
#
# Revision 38.0  1994/01/06  01:56:09  linus
# New version
#
# Revision 36.9  1994/01/06  00:55:12  linus
# Removed installation of doc.
#
# Revision 36.8  1993/12/14  02:21:29  linus
# Small fixes for emacs 18.55, jump.
#
# Revision 36.7  1993/11/21  04:37:06  linus
# Better tags entry.
#
# Revision 36.6  1993/09/10  16:36:51  linus
# Removed distribution script.
#
# Revision 36.5  1993/08/31  11:22:12  linus
# Corrected ftp-site. compress => gzip
#
# Revision 36.4  1993/07/26  19:07:12  linus
# Buggfixes, bug-report, quit-ding
#
# Revision 36.3  1993/05/27  15:22:43  linus
# install in local/var/elisp
#
# Revision 36.2  1993/05/27  13:21:12  linus
# obel18 => diag19
#
# Revision 36.1  1993/04/26  19:35:11  linus
# New version
#
# Revision 35.22  1993/04/26  19:30:54  linus
# Rewound to before prefetch.
#
# Revision 35.21  1993/02/04  04:00:48  linus
# Removed the import.make dependencie of the Makefile.
#
# Revision 35.20  1992/07/30  19:49:22  linus
# Moved around functions to the new reading.el file.
#
# Revision 35.19  1992/07/26  23:14:02  linus
# Added prefetch.el
#
# Revision 35.18  1992/06/16  11:33:32  linus
# Alfa-version is replaced upon installation also.
#
# Revision 35.17  1992/05/11  01:55:38  linus
# support for distribution to nanny removed
#
# Revision 35.16  1992/03/28  19:54:10  linus
# Removed mv to old-versions
#
# Revision 35.15  1992/03/22  21:10:12  linus
# Moved info-dir.
#
# Revision 35.14  1992/02/10  02:53:33  linus
# mkdir replaced by $(MKDIR).
#
# Revision 35.13  1991/12/19  12:03:51  linus
# .elc-files require macros.elc.
#
# Revision 35.12  1991/12/18  04:13:37  linus
# $(MAKE)
#
# Revision 35.11  1991/10/08  17:11:18  linus
# Splitting the makefile install entries to make it easier to install without doc.
#
# Revision 35.10  1991/10/05  18:15:27  linus
# Now handles the english version of the client also.
#
# Revision 35.9  1991/09/26  14:02:16  linus
# Install in the correct directory on the ftp-area.
#
# Revision 35.8  1991/09/16  20:33:44  linus
# r{tt namn p} todo filen i wc.
#
# Revision 35.7  1991/09/16  19:59:28  linus
# Makefile now works with clientversion-long.
#
# Revision 35.6  1991/09/16  19:31:37  linus
# Makefile now works with new doc-file names.
#
# Revision 35.5  1991/09/16  18:02:02  linus
# * M ci     Makefile
# * A ci     makealfa.l
# =====================================================
# makealfa.l created to make the alfa version of lyskom.elc
#
# Revision 35.4  1991/09/16  16:40:27  linus
# alfa talar om att det {r alfa i versionsnumret.
#
# Revision 35.3  1991/09/15  10:07:04  linus
# Added copyright note
#
# Revision 35.2  1991/09/13  21:42:53  linus
# Lagt till distclean.
#
# Revision 35.1  1991/08/21  15:44:47  linus
# Lagt till lyskom-clientversion-long
#

CLIENTVERSION = 0.38.1

GENERIC-CLEAN = *~ *.o core
GENERIC-DIST-CLEAN = TAGS
MKDIR = /bin/mkdir

RM = /usr/gnu/bin/rm -vf

# for compiling using emacs 19!
EMACS-BATCH = emacs -batch

# NOTE: lyskom-rest.el must be the last file in PARTS-EL and SRC-ELC.
PARTS-EL = komtypes.el clienttypes.el startup.el \
	reading.el \
	internal.el parse.el services.el cache.el\
	commands1.el commands2.el review.el edit-text.el \
	view-text.el async.el completing-read.el \
	prioritize.el flags.el \
	elib-string.el \
	lyskom-rest.el
HEADER-EL = macros.el vars.el
HEADER-ELC = macros.elc vars.elc
SWEDISH-EL = swedish-strings.el
SWEDISH-ELC = swedish-strings.elc
ENGLISH-EL = english-strings.el
ENGLISH-ELC = english-strings.elc
SRC-ELC = komtypes.elc clienttypes.elc startup.elc \
	reading.elc \
	internal.elc parse.elc services.elc cache.elc\
	commands1.elc commands2.elc review.elc edit-text.elc \
	view-text.elc async.elc completing-read.elc \
	prioritize.elc flags.elc \
	elib-string.elc \
	lyskom-rest.elc 
PARTS-ELC = $(SRC-ELC)

.SUFFIXES : .el .elc
.el.elc:
	$(EMACS-BATCH) -l `pwd`/macros -l `pwd`/vars -f batch-byte-compile $*.el

all: lyskom.elc

# influenced by autoconf? Yes.
vars.el: vars.el.in Makefile
	sed 's/@@CLIENTVERSION@@/$(CLIENTVERSION)/' < vars.el.in > vars.el

lyskom.elc: $(HEADER-ELC) $(SWEDISH-ELC) $(PARTS-ELC)
	cat $(HEADER-ELC) $(SWEDISH-ELC) $(PARTS-ELC) > lyskom.elc

english-lyskom.elc: $(HEADER-ELC) $(ENGLISH-ELC) $(PARTS-ELC)
	cat $(HEADER-ELC) $(ENGLISH-ELC) $(PARTS-ELC) > english-lyskom.elc

lyskom.el: $(HEADER-EL) $(SWEDISH-EL) $(PARTS-EL)
	cat $(HEADER-EL) $(SWEDISH-EL) $(PARTS-EL) > lyskom.el

$(SRC-ELC) $(SWEDISH-ELC) $(ENGLISH-ELC): macros.elc

elc-files: $(HEADER-ELC) $(PARTS-ELC) 


#Detta kr{ver gnu-make. Tyv{rr funkade inte $(SRC-ELC)-prylen l{ngre ner alls.
#Kanske denna $(SRC-ELC) pryl fungerar.
#$(SRC-ELC) :%.elc: %.el macros.elc
#	$(EMACS-BATCH) -l ./macros.elc -f batch-byte-compile $<

#$(SRC-ELC):
#	$(EMACS-BATCH) -l ./macros.el -f batch-byte-compile $*.el


wc:
	@wc $(HEADER-EL) $(PARTS-EL) $(SWEDISH-EL) $(ENGLISH-EL)
	@wc ../doc/elisp-client.latexinfo ChangeLog ../misc/todo

TAGS: $(HEADER-EL) $(PARTS-EL) $(SWEDISH-EL) $(ENGLISH-EL)
	etags $(HEADER-EL) $(PARTS-EL) $(SWEDISH-EL) $(ENGLISH-EL)

doc:
	(cd ../doc ; $(MAKE) elisp-client elisp-client.txt ; )

install: installnodoc installdoc

installatlysator: lyskom-$(CLIENTVERSION).elc lyskom.el
	# Lysator
	cp lyskom.el /usr/local/lib/emacs/site-lisp/lyskom.el
	cp lyskom-$(CLIENTVERSION).elc /usr/local/lib/emacs/site-lisp/lyskom.elc

lyskom-$(CLIENTVERSION).elc: lyskom.el
	$(EMACS-BATCH) -f batch-byte-compile lyskom.el
	mv lyskom.elc lyskom-$(CLIENTVERSION).elc

lyskom.elc-compatibility: lyskom.el
	$(EMACS-BATCH) -l ./compatibility.el -f batch-byte-compile lyskom.el
	mv lyskom.elc lyskom.elc-compatibility

lyskom-$(CLIENTVERSION).el.gz: lyskom.el
	gzip -9 < lyskom.el > lyskom-$(CLIENTVERSION).el.gz

lyskom-$(CLIENTVERSION).elc.gz: lyskom-$(CLIENTVERSION).elc
	gzip -9 < lyskom-$(CLIENTVERSION).elc > lyskom-$(CLIENTVERSION).elc.gz

installatftp: lyskom-$(CLIENTVERSION).el.gz lyskom-$(CLIENTVERSION).elc.gz
	touch lyskom-$(CLIENTVERSION).elc.gz # Do we have confidence problems?
	mv lyskom-$(CLIENTVERSION).el.gz lyskom-$(CLIENTVERSION).elc.gz \
		/usr/ftp/pub/lyskom/elisp-client

.PHONY: installnodoc
installnodoc: installatlysator installatftp

# This is now the responsability of the Makefile in the doc-dir.
installdoc: doc
	( cd ../doc; make install )

smallclean:
	$(RM) lyskom-$(CLIENTVERSION).el.gz lyskom-$(CLIENTVERSION).elc.gz
	$(RM) lyskom-$(CLIENTVERSION).elc lyskom.el

clean: smallclean
	$(RM) *~ *.elc $(GENERIC-CLEAN)

distclean: clean
	$(RM) $(GENERIC-DIST-CLEAN) Topdir.make */Topdir.make vars.el
