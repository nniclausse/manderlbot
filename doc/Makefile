# $Id$

MANPAGES  = manderlbot.1.sgml manderlbot.conf.5.sgml

all: man

man: $(MANPAGE)
	docbook2man manderlbot.1.sgml >/dev/null 2>&1
	docbook2man manderlbot.conf.5.sgml >/dev/null 2>&1

clean:
	-rm -f *~
	-rm -f manpage.links manpage.refs
