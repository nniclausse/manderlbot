# $Id$

MANPAGE  = manderlbot.1.sgml
DOCUMENT = $(basename $(wildcard *.lyx))

all: man $(DOCUMENT).tex $(DOCUMENT).html $(DOCUMENT).ps $(DOCUMENT).pdf

man: $(MANPAGE)
	docbook-to-man $< >/dev/null 2>&1

%.ps:%.lyx
	lyx -e ps $<

%.pdf:%.lyx
	lyx -e pdf $<

%.tex:%.lyx
	lyx -e latex $<

%.html:%.tex
	hevea -fix url-fix.hva $<
	-rm *.htoc *.haux

clean:
	-@rm -f *~
	-@rm -f $(DOCUMENT).tex $(DOCUMENT).html $(DOCUMENT).ps $(DOCUMENT).pdf
