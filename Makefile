EXTRA_DIST = README ACKNOWLEDGEMENTS AUTHORS .gitignore
SUBDIRS = doc src examples

export MOSSCODIR=$(CURDIR)

.PHONY: default doc src info examples all clean subdirs $(SUBDIRS)

default: src
all:  examples doc
examples: src

clean:
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done 
	@rm -rf modules lib bin

distclean: clean

all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

check: 
	make -C src check

update:
	git pull 

info:
	make -C src info

run: examples
	(cd examples/omexdia_p && ./omexdia_p_test)
	(cd examples/esmf_sediment && ./esmf_sediment_test)
