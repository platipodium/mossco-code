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
	$(foreach dir,$(SUBDIRS),make -C $(dir) clean)

check:
	@echo The check target is not implemented, yet
