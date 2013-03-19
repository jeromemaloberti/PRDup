SUFFIXES = .rl .dot
RAGEL = ragel

all:
	obuild configure && obuild build

install: all
	cp dist/build/PRDuplicate/PRDuplicate ${PREFIX}/bin/

clean: 
	obuild clean && obuild configure
