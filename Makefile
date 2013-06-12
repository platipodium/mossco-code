EXTRA_DIST = README ACKNOWLEDGEMENTS AUTHORS .gitignore
SUBDIRS = doc src examples

.PHONY: default doc src examples all clean subdirs $(SUBDIRS)

default: src
all:  examples doc
examples: src

clean:
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done 
	@rm -rf modules lib bin

all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

check: 
	make -C src check

update:
	git pull 
