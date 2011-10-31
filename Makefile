mlton: src/*
	@bash build.sh

.PHONO: install
install:
	mv bin/* ../../../bin

.PHONY: clean
clean :
	-rm bin/preml