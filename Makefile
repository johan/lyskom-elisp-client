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
