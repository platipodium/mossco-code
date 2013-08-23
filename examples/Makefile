ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

# Determine examples to build
SUBDIRS = 
ifeq ($(MOSSCO_ESMF),true)
SUBDIRS += esmf_remtc
ifeq ($(MOSSCO_DELFT),true)
SUBDIRS += erosed
endif
ifeq ($(MOSSCO_FABM),true)
SUBDIRS += omexdia_p
ifeq ($(MOSSCO_ESMF),true)
SUBDIRS += esmf_sediment
ifeq ($(MOSSCO_GOTM),true)
SUBDIRS += esmf_gotm esmf_fabm0d
endif
endif
endif
else
ifeq ($(MOSSCO_FABM),true)
SUBDIRS += omexdia_p
endif
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

