VERSION = 0.34

DOCFILE = elisp-client-$(VERSION)

# If you edit this, make sure it works afterwards also. /Linus

elisp-client:	../doc/$(DOCFILE).latexinfo
	emacs -batch -l /usr/local/lib/elisp/latexinfo-mode \
	-l /usr/local/lib/elisp/latexinfo \
	-f batch-latexinfo-format $?

$(DOCFILE).txt:	../doc/elisp-client
	emacs -batch -l `pwd`/info-nuke -f batch-info-nuke $?
	mv elisp-client.txt $(DOCFILE).txt

