# $Id$

DOCUMENT=$(basename $(wildcard *.lyx))

all: $(DOCUMENT).tex $(DOCUMENT).html $(DOCUMENT).ps $(DOCUMENT).pdf

%.ps:%.lyx
	lyx -e ps $<

%.pdf:%.lyx
	lyx -e pdf $<

%.tex:%.lyx
	lyx -e latex $<

%.html:%.tex
	hevea -fix -francais url-fix.hva $<
	-rm *.htoc *.haux

clean:
	-rm -f *~
	-rm -f $(DOCUMENT).tex $(DOCUMENT).html $(DOCUMENT).ps $(DOCUMENT).pdf
