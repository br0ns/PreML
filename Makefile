SML = mlton
# .PHONY: all

all: bin/preml

bin/preml: src/*
	@bash build.sh

install : bin/preml
	install bin/preml /usr/local/bin
	install bin/premlton /usr/local/bin

.PHONY: clean
clean :
	-rm bin/preml