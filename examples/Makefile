ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

SUBDIRS=omexdia_p

ifdef ESMFMKFILE
SUBDIRS +=esmf_sediment
endif

ifdef GOTMDIR
SUBDIRS +=esmf_gotm
SUBDIRS +=esmf_fabm0d
endif

.PHONY: all clean extraclean subdirs $(SUBDIRS)

default: all

all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean: extraclean

extraclean:
	@for dir in $(SUBDIRS) ; do make -C $$dir clean; done

