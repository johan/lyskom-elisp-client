EMACS=emacs-21.4
SRC=../../src

CHAPTERS=intro.xml \
	 texts.xml \
	 etiquette.xml \
	 commands.xml \
	 variables.xml \
         dummy.xml \
	 glossary.xml

GENFILES=fnc.ent var.ent editcmd.ent commands.xml variables.xml dummy.xml
INCFILES=$(CHAPTERS:%=%.inc)


# Generate the manual from DocBook XML using Jade wrappers
# I really don't know if the jw command is standard in installations
# of Jade, but it works fine in Debian/Gnu Linux. The path to the 
# stylesheet is also Linux (maybe Debian) specific.

manual.ps: $(INCFILES) $(GENFILES) manual.xml chp.xml
	jw -f docbook -d '/usr/share/sgml/docbook/stylesheet/dsssl/ldp/ldp.dsl#print' -b ps manual.xml


# These files are generated from the source code.

$(GENFILES): $(SRC)/lyskom.el
	$(EMACS) -batch -l $(SRC)/lyskom.el -l ./docgen.el -f lyskom-docgen


# This file is generated from the Makefile and contains references
# to all the chapters.

chp.xml: Makefile docgen.el
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

clean:
	rm -f $(GENFILES)  $(INCFILES) chp.xml


# The .xml.inc files are the chapter files without the surrounding
# book element. Probably XSLT would be the Right Thing, but sed 
# gets the job done too

%.xml.inc: %.xml
	sed -e '/<book[ >]/,/<\/book>/! d' -e '/<book[ >]/ d' -e '/<\/book>/ d' < $< > $@
