#
# $Id: Makefile,v 44.1 1999-11-19 22:15:14 joel Exp $
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

all: lisp doc

lisp:
	( cd src; ${MAKE} )

doc:
	( cd doc; $(MAKE) )

install:
	( cd src; $(MAKE) install )
	( cd doc; $(MAKE) install )

release:
	( cd src; $(MAKE) release )
	( cd doc; $(MAKE) release )

clean:
	rm -vf *~
	( cd src; ${MAKE} clean )
	( cd doc; ${MAKE} clean )
	( cd misc; ${MAKE} clean )

distclean: clean
	( cd src; ${MAKE} distclean )

debian-package:
	( cd src; $(MAKE) debian-package )
