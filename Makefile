PREFIX?=/usr/local

.DEFAULT: all
all: dist/setup
	obuild build

dist/setup:
	obuild configure

install: all
	cp dist/build/PRDuplicate/PRDuplicate $(PREFIX)/bin/

uninstall: all
	rm $(PREFIX)/bin/PRDuplicate

clean:
	obuild clean
