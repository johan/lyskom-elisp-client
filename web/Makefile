CVSROOT=$$USER@cvs.lysator.liu.se:/cvsroot/lyskom-elisp-client
 #TARGET=/lysator/www/projects/lyskom/klienter/elisp
TARGET=/var/www/lyskom
LANG=sv

DOCDIR = ../doc

all:
	./bin/mkindex index-template.xml items
	cvs -d $(CVSROOT) co -d $(TARGET) lyskom-elisp-client/web/files
	mkdir $(TARGET)/doc
	cp $(DOCDIR)/NEWS* $(TARGET)/doc
	mv index.en.html $(TARGET)
	mv index.sv.html $(TARGET)
	ln -sf index.$(LANG).html $(TARGET)/index.html
