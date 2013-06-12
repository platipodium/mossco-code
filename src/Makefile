include Rules.make

EXTRA_DIST=README Rules.make

SUBDIRS=components mediators drivers

.PHONY: subdirs $(SUBDIRS)

default: all
all: subdirs

components: drivers

subdirs: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@

clean: extraclean
extraclean:
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done 

check:
	@echo The check target is not implemented, yet
