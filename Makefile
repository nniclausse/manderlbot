# Build the .beam erlang VM files
OPT = -W
INC = ./inc
CC  = erlc
ERL = erl
SED = $(shell which sed)

ESRC = ./src
EBIN = ./ebin

RAW_INSTALL_DIR = /usr/lib/erlang/
ERLANG_INSTALL_DIR = $(DESTDIR)/$(RAW_INSTALL_DIR)/lib
APPLICATION = manderlbot
VERSION = 0.9.1

TARGETDIR= $(ERLANG_INSTALL_DIR)/$(APPLICATION)-$(VERSION)
BINDIR   = $(DESTDIR)/usr/bin
CONFFILE = config.xml
LOGFILE  = /var/log/manderlbot.log

SRC      = $(wildcard src/*.erl)
TMP      = $(wildcard *~) $(wildcard src/*~) $(wildcard inc/*~)
INC_FILES= $(wildcard $(INC)/*.hrl)
TARGET   = $(addsuffix .beam, $(basename $(addprefix ebin/, $(notdir $(SRC)))))
EMAKE    = $(addsuffix \'., $(addprefix \'../, $(SRC)))
APPFILES = $(ESRC)/$(APPLICATION).app.src $(ESRC)/$(APPLICATION).rel.src

SCRIPT   = $(BINDIR)/manderlbot
BUILD_OPTIONS =	'[{app_dir, "$(TARGETDIR)"}, \
	{systools, [{variables,[{"ROOT","$(RAW_INSTALL_DIR)"}]}]}, \
	{sh_script, none}, {report, verbose}, {make_app, true }].'
BUILD_OPTIONS_FILE = ./BUILD_OPTIONS 

.PHONY: doc

manderlbot: $(TARGET)

all: clean manderlbot

# used to generate the erlang Emakefile
emake:
	@echo $(EMAKE) | tr -s ' ' '\n' > ebin/Emakefile

clean:
	-rm -f $(TARGET) $(TMP) $(BUILD_OPTIONS_FILE)
	make -C doc clean

install: manderlbot builder.beam manderlbot.sh
	-rm -f $(TMP)
	install -d $(TARGETDIR)/ebin
	install -d $(TARGETDIR)/src
	install -d $(TARGETDIR)/include
	@cp $(INC_FILES) $(TARGETDIR)/include
	@cp $(TARGET) $(TARGETDIR)/ebin
	@cp $(SRC) $(APPFILES) $(TARGETDIR)/src

# use builder to make boot file
	@echo $(BUILD_OPTIONS) > $(BUILD_OPTIONS_FILE)
	erl -s builder go -s init stop

# create startup script
	@cp manderlbot.sh $(SCRIPT)
	@chmod +x $(SCRIPT)

# added for debian
	@cp $(CONFFILE) $(DESTDIR)/etc/manderlbot.xml

uninstall:
	rm -rf $(TARGETDIR) $(SCRIPT)

doc: 
	make -C doc

release:
	rm -fr $(APPLICATION)-$(VERSION)
	mkdir -p $(APPLICATION)-$(VERSION)
	@tar zcf tmp.tgz $(SRC) $(APPFILES) $(INC_FILES) doc/*.lyx doc/Makefile doc/*.hva \
		 			LICENSE README TODO $(CONFFILE) Makefile priv/builder.erl \
					manderlbot.sh.in
	tar -C $(APPLICATION)-$(VERSION) -zxf tmp.tgz
	mkdir $(APPLICATION)-$(VERSION)/ebin
	tar zvcf  $(APPLICATION)-$(VERSION).tar.gz $(APPLICATION)-$(VERSION)
	rm -fr $(APPLICATION)-$(VERSION)
	rm -fr tmp.tgz

builder.beam: priv/builder.erl 
	$(CC) $(OPT) -I $(INC) $<

ebin/%.beam: src/%.erl 
	$(CC) $(OPT) -I $(INC) -o ebin $<

manderlbot.sh: manderlbot.sh.in
	@$(SED) \
		-e 's;%INSTALL_DIR%;${RAW_INSTALL_DIR};g' \
		-e 's;%VERSION%;${VERSION};g' < $< > $@

%:%.sh
# Override makefile default implicit rule
