# Build the .beam erlang VM files
OPT = -W
INC = ./inc
CC = erlc
ERL = erl

ESRC = ./src
EBIN = ./ebin

ERLANG_INSTALL_DIR = /usr/lib/erlang/lib
APPLICATION = manderlbot
VERSION = 0.7

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

install: manderlbot
	-rm -f $(TMP)
	cp -r . $(ERLANG_INSTALL_DIR)/$(APPLICATION)-$(VERSION)

uninstall:
	rm -rf $(ERLANG_INSTALL_DIR)/$(APPLICATION)-$(VERSION)

ebin/%.beam: src/%.erl 
	$(CC) $(OPT) -I $(INC) -o ebin $<
