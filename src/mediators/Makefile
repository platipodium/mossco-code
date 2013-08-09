ifndef MOSSCODIR
MOSSCODIR=$(subst /src/mediators$,,$(PWD))
endif

include $(MOSSCODIR)/src/Rules.make


EXTRA_DIST=README
SUBDIRS=

.PHONY: default all subdirs $(SUBDIRS)

all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):


