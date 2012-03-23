mlton: src/*
	git submodule update --init
	@bash build.sh

.PHONY: install
install:
	cp bin/* /usr/local/bin

.PHONO: smack-install
smack-install:
	cp bin/* ../../../bin

.PHONY: clean
clean :
	-rm bin/preml