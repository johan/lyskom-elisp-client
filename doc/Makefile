#
# $Id: Makefile,v 44.4 1997-09-27 11:43:02 byers Exp $
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
# $Log: Makefile,v $
# Revision 44.4  1997-09-27 11:43:02  byers
# Nästan alla variabler är dokumenterade.
# Uppdaterade NEWS-filen för 0.45.1
#
# Revision 44.3  1997/09/16 15:07:36  byers
# Småfixar
#
# Revision 44.2  1997/08/11 19:54:02  davidk
# Ändrade versionsnumret till 0.45
# Ladda ../src/widget.el manuellt när man bygger lyskom-variables.data
#
# Revision 44.1  1996/09/03 16:05:34  byers
# Remainders for 0.44.1
#
# Revision 44.0  1996/08/30 14:41:32  davidk
# Ändrade revisionsnumret till 44.0
#
# Revision 43.5  1996/08/30 12:32:08  davidk
# *** empty log message ***
#
# Revision 43.4  1996/08/15 19:30:47  davidk
# Version 0.43.2 släppt.
#
# Revision 43.3  1996/08/14 13:49:16  byers
# Nya filer för dokumentationsdatabasen, lite upptadering i dokumentationen
#
# Revision 43.2  1996/08/14 04:15:14  davidk
# Bytte versionsnummer till 0.43.
#
# Revision 43.1  1996/08/07 20:00:46  davidk
# Ny ansvarsfördelning mellan Makefilerna.
#
# Revision 43.0  1996/08/07 16:36:24  davidk
# Nytt revisionsnummer.
#
# Revision 41.1  1996/08/07 16:22:53  davidk
# Uppdaterade copyright.
# Separerade install och release.
#
# Revision 41.0  1996/05/02 19:20:35  davidk
# Nytt revisionsnummer: 41.0
#
# Revision 40.0  1996/03/26 08:28:01  byers
# New version number for release of 0.40
# Added NEWS file for 0.40
#
# Revision 39.0  1996/03/14 18:14:56  davidk
# Ökade revisionsnumret till 39.0.
#
# Revision 38.0  1994/01/06 01:47:03  linus
# New version
#
# Revision 35.12  1994/01/06  00:54:49  linus
# Fixed bugs in install.
#
# Revision 35.11  1994/01/06  00:40:23  linus
# Install directive moved here.
#
# Revision 35.10  1994/01/06  00:04:42  linus
# Fixed so info-nuke works even if infod using makeinfo.
#
#
# If you edit this, make sure it works afterwards also. /Linus

CLIENTVERSION = 0.45.1-beta

SHELL = /bin/sh
RM = rm -vf

INFODIR = /usr/gnu/info
FTPDIR = /usr/ftp/pub/lyskom/elisp-client

all: elisp-client elisp-client.txt elisp-client.dvi
.PHONY: all

elisp-client: elisp-client.texinfo
	makeinfo elisp-client.texinfo

elisp-client.txt: elisp-client
	if [ -s elisp-client-1 ] ; then \
		mv elisp-client info.index ; \
		echo e | tr e '\037' | cat elisp-client-? - > elisp-client ; \
	fi
	emacs -batch -l `pwd`/info-nuke -f batch-info-nuke `pwd`/elisp-client
	if [ -s elisp-client-1 ] ; then \
		rm elisp-client ; \
		mv info.index elisp-client ; \
	fi

INDEXES = elisp-client.fns elisp-client.kys elisp-client.vrs

indexes: elisp-client.texinfo
	@echo pass 1
	rm -f indexes
	tex elisp-client.texinfo
	mv elisp-client.dvi indexes

$(INDEXES): indexes
	@echo pass 2 indexes
	touch $(INDEXES)
	emacs -batch -l ./texindex.el -f batch-texindex elisp-client.cp elisp-client.fn elisp-client.ky elisp-client.pg elisp-client.tp elisp-client.vr

elisp-client.dvi: $(INDEXES)
	@echo pass 3
	tex elisp-client.texinfo

lyskom-variables.data: lyskom-commands.data

lyskom-commands.data: ../src/lyskom.el
	emacs -batch -l makedoc.el -l ../src/widget.el -f lyskom-update-documentation


.PHONY: install
install: elisp-client elisp-client.txt
	cp elisp-client elisp-client-? $(INFODIR)

release: elisp-client elisp-client.txt
	cp NEWS-$(CLIENTVERSION) $(FTPDIR)
	(VER=`grep "^Dokumentationen uppdaterad till klient version " elisp-client.texinfo | \
	sed -n 's/^.* version \(.*\)$$/\1/p'` ; \
	tar cf elisp-client.info-$${VER}.tar elisp-client elisp-client-? ; \
	gzip elisp-client.info-$${VER}.tar; \
	gzip < elisp-client.txt > elisp-client.txt-$${VER}.gz; \
	mv elisp-client.info-$${VER}.tar.gz elisp-client.txt-$${VER}.gz \
	 $(FTPDIR); )


smallclean:
	$(RM) *.fns *.kys *.pgs *.tps *.vrs *.cps indexes

clean: smallclean
	$(RM) elisp-client elisp-client-? info.index elisp-client.txt
	$(RM) *.aux *.dvi 
	$(RM) *.fn  *.ky  *.pg  *.tp  *.vr  *.cp
	$(RM) *.toc
	$(RM) *.bbl *.blg *.idx *.ilg *.ind *.log
	$(RM) core *~
