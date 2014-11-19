BIN = ~/bin/
VER = 1.1.2

all: monpoly mfotl2sql table2log

.PHONY: monpoly doc clean clean-all depend 

monpoly: 
	cd src && $(MAKE) monpoly
	mv src/monpoly .

install: monpoly 
	cp -v monpoly $(BIN)


mfotl2sql: 
	cd tools && $(MAKE) mfotl2sql

table2log: 
	cd tools && $(MAKE) table2log

install-all: install mfotl2sql table2log
	cp -v tools/mfotl2sql $(BIN)
	cp -v tools/table2log $(BIN)


doc: 
	cd src && $(MAKE) doc


clean: 
	cd src && $(MAKE) clean
	cd tools && $(MAKE) clean


clean-all: clean
	rm -f monpoly
	rm -f doc/*
	rm -f tools/mfotl2sql tools/table2log
	rm -f $(BIN)monpoly $(BIN)mfotl2sql $(BIN)table2log 


depend:
	cd src && $(MAKE) depend


monpoly-$(VER).tgz:
	tar --exclude=.svn -zcf ../monpoly-$(VER).tgz ../monpoly-$(VER)

release: monpoly-$(VER).tgz
