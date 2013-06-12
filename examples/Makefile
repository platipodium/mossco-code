EXTRA_DIST=README
SUBDIRS=esmf_component omexdia_p

include ../src/Rules.make

.PHONY: subdirs (SUBDIRS)

default: all

all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@



