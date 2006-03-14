# $Id$

MANPAGES  = manderlbot.1 manderlbot.conf.5

all: man

man: $(MANPAGES)

%.1:%.1.sgml
	docbook2man $< >/dev/null 2>&1

%.5:%.5.sgml
	docbook2man $< >/dev/null 2>&1


clean:
	-rm -f *~
	-rm -f manpage.links manpage.refs *.1 *.5
