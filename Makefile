mlton: src/*
	@bash build.sh

.PHONO: smackage-install
smackage-install:
	mv bin/* ../../../bin

install : bin/preml
	install bin/preml /usr/local/bin
	install bin/premlton /usr/local/bin

.PHONY: clean
clean :
	-rm bin/preml