#
# $Id: Makefile,v 35.18 1992-06-16 11:33:32 linus Exp $
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
# $Id: Makefile,v 35.18 1992-06-16 11:33:32 linus Exp $
# $Log: Makefile,v $
# Revision 35.18  1992-06-16 11:33:32  linus
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

include Topdir.make
include $(TOPDIR)/scripts/import.make

RM = /usr/gnu/bin/rm -vf
EMACS-BATCH = emacs -batch -l /usr/local/lib/elisp/bytecomp

# NOTE: lyskom-rest.el must be the last file in PARTS-EL and SRC-ELC.
PARTS-EL = komtypes.el clienttypes.el startup.el \
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
	internal.elc parse.elc services.elc cache.elc\
	commands1.elc commands2.elc review.elc edit-text.elc \
	view-text.elc async.elc completing-read.elc \
	prioritize.elc flags.elc \
	elib-string.elc \
	lyskom-rest.elc
PARTS-ELC = $(SRC-ELC)

.SUFFIXES : .el .elc
.el.elc:
	$(EMACS-BATCH) -l ./macros.elc -f batch-byte-compile $*.el

all: lyskom.elc

LEX = flex -8
SWEDISH-ALFA-VERSION = /usr/local/src/2kom/lyskom.elc
ENGLISH-ALFA-VERSION = /usr/local/src/2kom/english-lyskom.elc
alfa: lyskom.elc makealfa english-lyskom.elc
	./makealfa < lyskom.elc > $(SWEDISH-ALFA-VERSION)
	./makealfa < english-lyskom.elc > $(ENGLISH-ALFA-VERSION)

lyskom.elc: $(HEADER-ELC) $(SWEDISH-ELC) $(PARTS-ELC)
	cat $(HEADER-ELC) $(SWEDISH-ELC) $(PARTS-ELC) > lyskom.elc

english-lyskom.elc: $(HEADER-ELC) $(ENGLISH-ELC) $(PARTS-ELC)
	cat $(HEADER-ELC) $(ENGLISH-ELC) $(PARTS-ELC) > english-lyskom.elc

lyskom.el: $(HEADER-EL) $(SWEDISH-EL) $(PARTS-EL)
	cat $(HEADER-EL) $(SWEDISH-EL) $(PARTS-EL) > lyskom.el

makealfa: makealfa.c
	$(CC) -o makealfa $(CFLAGS) makealfa.c -ll

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

tags:
	etags $(HEADER-EL) $(PARTS-EL) $(SWEDISH-EL) $(ENGLISH-EL)

doc:
	(cd ../doc ; $(MAKE) elisp-client elisp-client.txt ; )

install: installnodoc installdoc

installnodoc: lyskom.elc lyskom.el
	cp lyskom.elc /usr/local/lib/elisp/lyskom.elc
	cp lyskom.elc $(SWEDISH-ALFA-VERSION)
#	rcp lyskom.elc nanny:/usr/local/lib/elisp/lyskom.elc
	(VER=`grep "lyskom-clientversion " vars.el | \
	 sed -n 's/^.*defconst.*lyskom-clientversion[^"]*"\([^"]*\)".*$$/\1/p'`;\
	 cp lyskom.elc lyskom-$${VER}.elc;\
	 compress lyskom-$${VER}.elc;\
	 mv lyskom-$${VER}.elc.Z /usr/local/ftp/pub/lyskom/elisp-client;\
	 cp lyskom.el lyskom-$${VER}.el;\
	 compress lyskom-$${VER}.el;\
	 cp lyskom-$${VER}.el.Z /usr/local/ftp/pub/lyskom/elisp-client;)


installdoc: doc
	(VER=`grep "lyskom-clientversion " vars.el | \
	 sed -n 's/^.*defconst.*lyskom-clientversion[^"]*"\([^"]*\)".*$$/\1/p'`;\
	 cp ../doc/elisp-client ../doc/elisp-client.txt .;\
	 compress elisp-client elisp-client.txt;\
	 mv elisp-client.Z \
		/usr/local/ftp/pub/lyskom/elisp-client/elisp-client-$${VER}.info.Z; \
	 mv elisp-client.txt.Z \
	     /usr/local/ftp/pub/lyskom/elisp-client/elisp-client-$${VER}.txt.Z; \
	 cp ../doc/elisp-client /usr/gnu/info; \
	)

clean:
	$(RM) *~ *.elc $(GENERIC-CLEAN) makealfa.c
	-$(RM) -r distribution

distclean: clean
	$(RM) $(GENERIC-DIST-CLEAN) makealfa Topdir.make */Topdir.make


distrib distribution: lyskom.el lyskom.elc doc
	-$(RM) -r distribution
	$(MKDIR)  distribution
	cp distribution-makefile distribution/Makefile
	cp distribution-README distribution/README
	cp $(HEADER-EL) $(PARTS-EL) $(SWEDISH-EL) $(ENGLISH-EL) distribution
	cat distribution-README vars.el > distribution/vars.el
	cp README distribution
	cat distribution-README lyskom.el  > distribution/lyskom.el
	$(RM) lyskom.el
	cat distribution-README lyskom.elc > distribution/lyskom.elc
	cp ../doc/elisp-client.txt ../doc/elisp-client distribution
	cd distribution; \
	  shar -v -cCmx -l 50 -o PARTS README Makefile $(HEADER-EL) $(PARTS-EL)  $(SWEDISH-EL) $(ENGLISH-EL)
	cd distribution; \
	  shar -v -cCmx -l 50 -o ALL lyskom.el
	cd distribution; \
	  shar -v -cCmx -l 50 -o BYTEC README lyskom.elc
	cd distribution; \
	  ( $(RM) lyskom.elc ; \
	    sed 's/kom\.lysator\.liu\.se/obel18.ida.liu.se/g' > lyskom.elc )\
	  < lyskom.elc ; \
	  chmod 644 lyskom.el lyskom.elc elisp-client.txt
	cd distribution; \
	  shar -v -cCmx -l 50 -o DOC elisp-client.txt
	cd distribution; \
	  shar -v -cCmx -l 50 -o IDOC elisp-client


send:
	cd distribution ; ../../bin/lyskom-do-send

