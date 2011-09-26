SML = mlton

# .PHONY: all

all: preml

preml:
	$(SML) -output preml PreML.mlb

install : preml
	install preml /usr/bin
	install premlton /usr/bin

.PHONY: clean
clean :
	-rm preml
