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
export MOSSCO_DIR=$(subst /src/mediators$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

TARGETS=
ifeq ($(MOSSCO_ESMF),true)
TARGETS += libaocoupler
ifeq ($(MOSSCO_FABM),true)
TARGETS += liboceansedimentcoupler
endif
endif

SUBDIRS=

.PHONY: default all subdirs $(SUBDIRS) $(TARGETS)

all: subdirs $(TARGETS)

libaocoupler: prefix $(MOSSCO_LIBRARY_PATH)/libaocoupler.a
liboceansedimentcoupler: prefix $(MOSSCO_LIBRARY_PATH)/liboceansedimentcoupler.a

$(MOSSCO_LIBRARY_PATH)/libaocoupler.a: remtc_atmos_ocean_coupler.o 
	$(AR) cruvs $@ remtc_atmos_ocean_coupler.o

$(MOSSCO_LIBRARY_PATH)/liboceansedimentcoupler.a: ocean_sediment_coupler.o
	$(AR) cruvs $@ remtc_atmos_ocean_coupler.o

subdirs: $(SUBDIRS)

$(SUBDIRS):

clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libaocoupler.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/liboceansedimentcoupler.a
	
