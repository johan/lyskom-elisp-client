all:
	(cd src; ${MAKE})

install:
	echo elisp-client not installed (do that manually)

clean:
	rm -vf misc/*~ *~
	(cd src; ${MAKE} clean)

depend:;
