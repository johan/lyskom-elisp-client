#
# Don't change these. Set the on the command line if you need to 
# change them
#

USER=$$USER
CVSROOT=$(USER)@cvs.lysator.liu.se:/cvsroot/lyskom-elisp-client
TARGET=/lysator/www/www-local/lyskom/klienter/emacslisp
LANG=sv

DOCDIR = ../doc

all: dirs index
	cd $(TARGET)/.. && \
		cvs -d $(CVSROOT) co -d `basename $(TARGET)` \
			lyskom-elisp-client/web/files
	cp $(DOCDIR)/NEWS* $(TARGET)/doc
	ln -sf index.$(LANG).html $(TARGET)/index.html
	echo "DefaultType text/plain" > $(TARGET)/doc/.htaccess

dirs:
	if [ \! -d $(TARGET) ] ; then mkdir $(TARGET); fi
	if [ \! -d $(TARGET)/doc ] ; then mkdir $(TARGET)/doc; fi

index:
	./bin/mkindex index-template.xml items
	mv index.en.html $(TARGET)
	mv index.sv.html $(TARGET)
