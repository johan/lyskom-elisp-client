SRC=../../src

CHAPTERS=texts.xml \
	 commands.xml

INCFILES=$(CHAPTERS:%=%.inc)


# Generate the manual from DocBook XML using Jade wrappers
# I really don't know if the jw command is standard in installations
# of Jade, but it works fine in Debian/Gnu Linux.

manual.ps: $(INCFILES) manual.xml chp.xml
	jw -f docbook -b ps manual.xml


# These files are generated from the source code.

fnc.ent var.ent commands.xml: $(SRC)/lyskom.el
	emacs -batch -l $(SRC)/lyskom.el -l ./docgen.el -f lyskom-docgen


# This file is generated from the Makefile and contains references
# to all the chapters.

chp.xml: Makefile
	echo > $@
	for i in $(INCFILES) ; do \
		echo "<!ENTITY $$i SYSTEM \"$$i\">" >> $@ ; \
	done
	echo "<!ENTITY chapters \"" \
	`echo $(INCFILES) | sed 's/\([-A-Za-z0-9.][-A-Za-z0-9.]*\)/\&\1; /g'` \
	"\">" >> $@


# Build lyskom.el if necessary

$(SRC)/lyskom.el:
	cd $(SRC) && make lyskom.el


# The .xml.inc files are the chapter files without the surrounding
# book element. They are built using the xml2inc script.

%.xml.inc: %.xml
	./xml2inc < $< > $@
