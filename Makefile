# Build the .beam erlang VM files
OPT = -W
INC = ./inc
CC = erlc

ERLANG_INSTALL_DIR = /usr/lib/erlang/lib
APPLICATION = manderlbot
VERSION = 0.7

SRC=$(wildcard src/*.erl)
TMP=$(wildcard src/*~) $(wildcard inc/*~)
TARGET=$(addsuffix .beam, $(basename $(addprefix ebin/, $(notdir $(SRC)))))
EMAKE = $(addsuffix \'., $(addprefix \'../, $(SRC)))

manderlbot: $(TARGET)

all: clean manderlbot

# used to generate the erlang Emakefile
emake:
	@echo $(EMAKE) | tr -s ' ' '\n' > ebin/Emakefile

clean:
	-rm -f $(TARGET) $(TMP)

install: $(TARGET)
	-rm -f $(TMP)
	cp -r . $(ERLANG_INSTALL_DIR)/$(APPLICATION)-$(VERSION)

uninstall:
	rm -rf $(ERLANG_INSTALL_DIR)/$(APPLICATION)-$(VERSION)

ebin/%.beam: src/%.erl 
	$(CC) $(OPT) -I $(INC) -o ebin $<
