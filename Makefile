#
# $Id: Makefile,v 39.0 1996-03-14 18:14:14 davidk Exp $
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
all: binaries

binaries:
	(cd src; ${MAKE})

includes:;
libraries:;

install:
	echo elisp-client not installed (do that manually)

clean:
	rm -vf misc/*~ *~
	(cd src; ${MAKE} clean)

distclean: clean
	rm -vf Topdir.make
	(cd src; ${MAKE} distclean)
	rm -vf */Topdir.make

depend:;
