#
# $Id: Makefile,v 35.9 1993-12-14 02:10:32 linus Exp $
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
# If you edit this, make sure it works afterwards also. /Linus

RM = rm -f

elisp-client: elisp-client.texinfo
	makeinfo elisp-client.texinfo

elisp-client.txt: elisp-client
	emacs -batch -l `pwd`/info-nuke -f batch-info-nuke `pwd`/$?

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

smallclean:
	$(RM) *.fns *.kys *.pgs *.tps *.vrs *.cps indexes

clean: smallclean
	$(RM) elisp-client elisp-client.txt
	$(RM) *.aux *.dvi 
	$(RM) *.fn  *.ky  *.pg  *.tp  *.vr  *.cp
	$(RM) *.toc
	$(RM) *.bbl *.blg *.idx *.ilg *.ind *.log
	$(RM) core *~
