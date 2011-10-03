SML = mlton
# .PHONY: all

all: preml

preml:
	@bash build.sh

install : preml
	install preml /usr/local/bin
	install premlton /usr/local/bin

.PHONY: clean
clean :
	-rm preml