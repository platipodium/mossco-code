# This Makefile is part of MOSSCO
# 
# Copyright (C) 2013 Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
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
SUBDIRS += fabmsed_fabm0d
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

