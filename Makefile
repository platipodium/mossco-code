EXTRA_DIST = README ACKNOWLEDGEMENTS AUTHORS .gitignore
SUBDIRS = doc src examples

.PHONY: default doc src examples all clean subdirs $(SUBDIRS)

default: src
all: doc examples

clean:
	$(foreach dir,$(SUBDIRS), make -C $(dir) clean)
	rm -rf modules lib

all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

examples: 
	@echo Please change to examples directory and make your individual examples.

check: 
	make -C src check

update: pull
pull: git pull 
