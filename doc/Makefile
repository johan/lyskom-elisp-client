#
# $Id: Makefile,v 44.37 2000-11-29 14:47:59 byers Exp $
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

CLIENTVERSION = 0.46.1-BETA-3

SHELL = /bin/sh
RM = rm -f

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
