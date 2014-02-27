# This Makefile is part of MOSSCO
# 
# @copyright (C) 2013, 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
# @author Richard Hofmeister, Helmholtz-Zentrum Geesthacht    
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/esmf/gotm$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

LIBS := -lgotm -lesmf $(LIBS) -lmossco_util
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS)
LDFLAGS += -L$(GOTM_LIBRARY_PATH) -lgotm_prod
ifeq ($(MOSSCO_GOTM_FABM),true)
LDFLAGS += -lmossco_gotmfabm -lsolver
LDFLAGS += -L$(FABM_LIBRARY_PATH) -lfabm_prod
endif
LDFLAGS += -lairsea_prod -lmeanflow_prod -lseagrass_prod -lturbulence_prod -lutil_prod -lutil_prod -loutput_prod -lobservations_prod  -linput_prod $(NC_LIBS) 

.PHONY: all exec libgotm libmossco_util

all: exec
exec: libmossco_util libsolver libmossco_gotmfabm libgotm gotm

libsolver libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libmossco_gotmfabm libgotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

gotm: toplevel_component.o ../../common/main.o
	$(F90) $(F90FLAGS) -o $@ $^ $(LDFLAGS)
	@echo "Created example $(MOSSCO_DIR)/examples/esmf/$@"

clean: extraclean

extraclean: 
	@rm -f esmf_gotm_example main.F90
	@rm -f PET*.*
