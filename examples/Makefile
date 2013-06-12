EXTRA_DIST=README
SUBDIRS=esmf_sediment omexdia_p

include ../src/Rules.make

.PHONY: subdirs $(SUBDIRS)

default: all

all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean: extraclean

extraclean:
	@for dir in $(SUBDIRS) ; do make -C $$dir clean; done

