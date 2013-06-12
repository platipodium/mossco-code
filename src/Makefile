include Rules.make

EXTRA_DIST=README
SUBDIRS=components mediators drivers

.PHONY: subdirs $(SUBDIRS)

default: all
all: subdirs

subdirs: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@

clean:
	for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done 

check:
	@echo The check target is not implemented, yet
