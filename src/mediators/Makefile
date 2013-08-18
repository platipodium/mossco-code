ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/mediators$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make


EXTRA_DIST=README
SUBDIRS=

.PHONY: default all subdirs $(SUBDIRS)

all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):


