# $Id: Makefile,v 35.1 1991-08-21 15:44:47 linus Exp $
# $Log: Makefile,v $
# Revision 35.1  1991-08-21 15:44:47  linus
# Lagt till lyskom-clientversion-long
#

RM = /usr/gnu/bin/rm -vf
EMACS-BATCH = emacs -batch -l /usr/local/lib/elisp/bytecomp

# NOTE: lyskom-rest.el must be the last file in PARTS-EL and SRC-ELC.
PARTS-EL = vars.el swedish-strings.el komtypes.el clienttypes.el startup.el \
	internal.el parse.el services.el cache.el\
	commands1.el commands2.el review.el edit-text.el \
	view-text.el async.el completing-read.el \
	prioritize.el flags.el \
	elib-string.el \
	lyskom-rest.el
HEADER-EL = macros.el
HEADER-ELC = macros.elc
SRC-ELC = vars.elc swedish-strings.elc komtypes.elc clienttypes.elc startup.elc \
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

alfa: lyskom.elc
	install lyskom.elc /usr/local/src/2kom/lyskom.elc

lyskom.elc: $(HEADER-ELC) $(PARTS-ELC)
	cat $(HEADER-ELC) $(PARTS-ELC) > lyskom.elc

lyskom.el: $(HEADER-EL) $(PARTS-EL)
	cat $(HEADER-EL) $(PARTS-EL) > lyskom.el


elc-files: $(HEADER-ELC) $(PARTS-ELC) 


#Detta kr{ver gnu-make. Tyv{rr funkade inte $(SRC-ELC)-prylen l{ngre ner alls.
#Kanske denna $(SRC-ELC) pryl fungerar.
#$(SRC-ELC) :%.elc: %.el macros.elc
#	$(EMACS-BATCH) -l ./macros.elc -f batch-byte-compile $<

#$(SRC-ELC):
#	$(EMACS-BATCH) -l ./macros.el -f batch-byte-compile $*.el


wc:
	wc $(HEADER-EL) $(PARTS-EL)
	wc ../doc/elisp-client-`grep lyskom-clientversion vars.el | \
	sed -n 's/^.*defconst.*lyskom-clientversion.*"\(.*\)".*$$/\1/p'`.*\
	   ChangeLog ../misc/To-do

tags:
	etags $(HEADER-EL) $(PARTS-EL)

doc:
	(VER=`grep lyskom-clientversion vars.el | \
	 sed -n 's/^.*defconst.*lyskom-clientversion.*"\(.*\)".*$$/\1/p'`;\
	 (cd ../doc ; make elisp-client elisp-client-$${VER}.txt) ; )


install: lyskom.elc lyskom.el doc
	cp lyskom.elc /usr/local/lib/elisp/lyskom.elc
	rcp lyskom.elc nanny:/usr/local/lib/elisp/lyskom.elc
	(VER=`grep lyskom-clientversion lyskom.el | \
	 sed -n 's/^.*defconst.*lyskom-clientversion.*"\(.*\)".*$$/\1/p'`;\
	 cp lyskom.el /usr/local/ftp/pub/lyskom/lyskom-$${VER}.el;\
	 cp lyskom.elc /usr/local/ftp/pub/lyskom/lyskom-$${VER}.elc;\
	 cp lyskom.el lyskom-$${VER}.el;\
	 compress lyskom-$${VER}.el;\
	 mv lyskom-$${VER}.el.Z ../old-versions/;\
	 cp ../doc/elisp-client \
	     /usr/local/ftp/pub/lyskom/elisp-client-$${VER}.info; \
	 cp ../doc/elisp-client-$${VER}.txt \
	     /usr/local/ftp/pub/lyskom/elisp-client-$${VER}.txt; \
	 cp ../doc/elisp-client /usr/gnu/emacs/info; \
	)

clean:
	$(RM) *~ *.elc core
	-$(RM) -r distribution


distrib distribution: lyskom.el lyskom.elc doc
	-$(RM) -r distribution
	mkdir     distribution
	cp distribution-makefile distribution/Makefile
	cp distribution-README distribution/README
	cp $(HEADER-EL) $(PARTS-EL) distribution
	cat distribution-README vars.el > distribution/vars.el
	cp README distribution
	cat distribution-README lyskom.el  > distribution/lyskom.el
	$(RM) lyskom.el
	cat distribution-README lyskom.elc > distribution/lyskom.elc
	(VER=`grep lyskom-clientversion vars.el | \
	 sed -n 's/^.*defconst.*lyskom-clientversion.*"\(.*\)".*$$/\1/p'`;\
	 cp ../doc/elisp-client-$${VER}.txt ../doc/elisp-client distribution)
	cd distribution; \
	  shar -v -cCmx -l 50 -o PARTS README Makefile $(HEADER-EL) $(PARTS-EL)
	cd distribution; \
	  shar -v -cCmx -l 50 -o ALL lyskom.el
	cd distribution; \
	  shar -v -cCmx -l 50 -o BYTEC README lyskom.elc
	cd distribution; \
	  ( $(RM) lyskom.elc ; \
	    sed 's/kom\.lysator\.liu\.se/obel18.ida.liu.se/g' > lyskom.elc )\
	  < lyskom.elc ; \
	  chmod 644 lyskom.el lyskom.elc elisp-client-*.txt
	cd distribution; \
	  shar -v -cCmx -l 50 -o DOC elisp-client-*.txt
	cd distribution; \
	  shar -v -cCmx -l 50 -o IDOC elisp-client


send:
	cd distribution ; ../../bin/lyskom-do-send

