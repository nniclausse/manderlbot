# Build the .beam erlang VM files
OPT = -W
INC = ./inc
CC = erlc
ERL = erl

ESRC = ./src
EBIN = ./ebin

ERLANG_INSTALL_DIR = $(DESTDIR)/usr/lib/erlang/lib
APPLICATION = manderlbot
VERSION = 0.7

TARGETDIR= $(ERLANG_INSTALL_DIR)/$(APPLICATION)-$(VERSION)
CONFFILE = $(TARGETDIR)/config.xml

SRC      = $(wildcard src/*.erl)
TMP      = $(wildcard src/*~) $(wildcard inc/*~)
TARGET   = $(addsuffix .beam, $(basename $(addprefix ebin/, $(notdir $(SRC)))))
EMAKE    = $(addsuffix \'., $(addprefix \'../, $(SRC)))
APPFILES = $(EBIN)/$(APPLICATION).app $(EBIN)/$(APPLICATION).rel

manderlbot: $(TARGET) boot

all: clean manderlbot

boot: $(EBIN)/make_boot.beam $(APPFILES)
	cd $(EBIN) && $(ERL) -noshell -s make_boot make_boot manderlbot

# used to generate the erlang Emakefile
emake:
	@echo $(EMAKE) | tr -s ' ' '\n' > ebin/Emakefile

clean:
	-rm -f $(TARGET) $(TMP)

# We also have to install the xmerl lib
xmerl:
	cp -r contrib/xmerl-0.15 $(ERLANG_INSTALL_DIR)
	find $(ERLANG_INSTALL_DIR)/xmerl-0.15 -type f -exec chmod a-x {} \;
	chmod a+x $(ERLANG_INSTALL_DIR)/xmerl-0.15/src/compile_grammar.sh

install: xmerl clean manderlbot
	-rm -f $(TMP)
	mkdir -p $(TARGETDIR) 
	-cp -r . $(TARGETDIR)

	# no file should be executable
	find $(TARGETDIR) -type f -exec chmod a-x {} \;

	# added for debian
	cp $(CONFFILE) $(DESTDIR)/etc/manderlbot.xml

uninstall:
	rm -rf $(TARGETDIR)

ebin/%.beam: src/%.erl 
	$(CC) $(OPT) -I $(INC) -o ebin $<
