# This Makefile is part of MOSSCO
# 
# Copyright (C) 2013             Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/components/remtc$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

SUBDIRS=

TARGETS=SUBDIRS

ifeq ($(MOSSCO_ESMF),true)
TARGETS += libatmosphere libocean
endif

.PHONY: $(TARGETS)

#--------------------------------------
# Make targets

all: $(TARGETS)

libocean: prefix $(MOSSCO_LIBRARY_PATH)/libocean.a
libatmosphere: prefix $(MOSSCO_LIBRARY_PATH)/libatmosphere.a
libtypes: prefix $(MOSSCO_MODULE_PATH)/mossco_variable_types.mod

$(MOSSCO_LIBRARY_PATH)/libocean.a: remtc_ocean.o
	$(AR) cruvs $@ remtc_ocean.o

$(MOSSCO_LIBRARY_PATH)/libatmosphere.a: libtypes remtc_atmosphere.o
	$(AR) cruvs $@ remtc_atmosphere.o

$(MOSSCO_MODULE_PATH)/mossco_variable_types.mod:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities libtypes

clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libocean.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libatmosphere.a

subdirs: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@
