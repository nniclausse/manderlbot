# Build the .beam erlang VM files
OPT = -W
INC = ./inc
CC = erlc

SRC=$(wildcard src/*.erl)
TMP=$(wildcard src/*~) $(wildcard inc/*~)
TARGET=$(addsuffix .beam, $(basename $(addprefix ebin/, $(notdir $(SRC)))))
EMAKE = $(addsuffix \'., $(addprefix \'../, $(SRC)))

all: $(TARGET)

# used to generate the erlang Emakefile
emake:
	@echo $(EMAKE) | tr -s ' ' '\n' > ebin/Emakefile

clean:
	-rm -f $(TARGET) $(TMP)

ebin/%.beam: src/%.erl 
	$(CC) $(OPT) -I $(INC) -o ebin $<
