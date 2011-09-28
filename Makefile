SML = mlton
# .PHONY: all

all: preml

preml:
	bash build.sh

install : preml
	install preml /usr/bin
	install premlton /usr/bin

.PHONY: clean
clean :
	-rm preml