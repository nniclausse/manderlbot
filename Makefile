# Build the .beam erlang VM files
OPT = -W
INC = ./inc
CC = erlc
ERL = erl

ESRC = ./src
EBIN = ./ebin

RAW_INSTALL_DIR = /usr/lib/erlang/
ERLANG_INSTALL_DIR = $(DESTDIR)/$(RAW_INSTALL_DIR)/lib
APPLICATION = manderlbot
VERSION = 0.9.0

TARGETDIR= $(ERLANG_INSTALL_DIR)/$(APPLICATION)-$(VERSION)
BINDIR   = $(DESTDIR)/usr/bin
CONFFILE = config.xml

SRC      = $(wildcard src/*.erl)
TMP      = $(wildcard src/*~) $(wildcard inc/*~)
INC_FILES= $(wildcard $(INC)/*.hrl)
TARGET   = $(addsuffix .beam, $(basename $(addprefix ebin/, $(notdir $(SRC)))))
EMAKE    = $(addsuffix \'., $(addprefix \'../, $(SRC)))
APPFILES = $(ESRC)/$(APPLICATION).app.src $(ESRC)/$(APPLICATION).rel.src
SCRIPT   = $(BINDIR)/manderlbot
BUILD_OPTIONS =	'[{app_dir, "$(TARGETDIR)"}, \
	{systools, [{variables,[{"ROOT","$(RAW_INSTALL_DIR)"}]}]}, \
	{sh_script, none}, {report, verbose}, {make_app, true }].'
BUILD_OPTIONS_FILE = ./BUILD_OPTIONS 

manderlbot: $(TARGET)

all: clean manderlbot

# used to generate the erlang Emakefile
emake:
	@echo $(EMAKE) | tr -s ' ' '\n' > ebin/Emakefile

clean:
	-rm -f $(TARGET) $(TMP) $(BUILD_OPTIONS_FILE)

install: manderlbot builder.beam
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
	@echo "#!/bin/sh" > $(SCRIPT)
	@echo "erl -detached -boot $(RAW_INSTALL_DIR)/lib/$(APPLICATION)-$(VERSION)/priv/manderlbot" >> $(SCRIPT)
	@chmod +x $(SCRIPT)
# added for debian
	@cp $(CONFFILE) $(DESTDIR)/etc/manderlbot.xml

uninstall:
	rm -rf $(TARGETDIR) $(SCRIPT)

builder.beam: priv/builder.erl 
	$(CC) $(OPT) -I $(INC) $<

ebin/%.beam: src/%.erl 
	$(CC) $(OPT) -I $(INC) -o ebin $<
