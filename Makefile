ML = mlton
MLB = ./src/metaphono.mlb
BIN = ./bin/main

all:
	${ML} -output ${BIN} ${MLB}

run: all
	./bin/main

.PHONY = all run

.DEFAULT_GOAL := run
