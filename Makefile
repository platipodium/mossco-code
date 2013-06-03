EXTRA_DIST=README ACKNOWLEDGEMENTS AUTHORS

.PHONY: default doc src examples

default: src
all: doc src examples

doc:
	$(MAKE) -C doc

examples:
	$(MAKE) -C examples

src: 
	$(MAKE) -C src
