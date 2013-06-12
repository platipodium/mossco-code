EXTRA_DIST = README ACKNOWLEDGEMENTS AUTHORS .gitignore
SUBDIRS = doc src examples

.PHONY: default doc src examples all clean subdirs $(SUBDIRS)

default: src
all: doc examples

clean:
	$(foreach dir,$(SUBDIRS), make -C $(dir) clean)

all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

check: 
	make -C src check

update:
	git pull 
