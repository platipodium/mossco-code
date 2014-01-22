# This Makefile is part of MOSSCO
# 
# @copyright (C) 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#

ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/esmf_gotmfabm$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifneq ($(MOSSCO_GOTM_FABM),true)
$(error This example only works with MOSSCO_GOTM_FABM=true)
endif

LIBS := -lesmf  $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS)
LDFLAGS += -lgotm -lmossco_gotmfabm -lsolver -lmossco_util
LDFLAGS += -L$(GOTM_LIBRARY_PATH) -lgotm_prod
LDFLAGS += -L$(FABM_LIBRARY_PATH) -lfabm_prod
LDFLAGS += -lairsea_prod -lmeanflow_prod -lseagrass_prod -lturbulence_prod -lutil_prod -lutil_prod -loutput_prod -lobservations_prod  -linput_prod $(NC_LIBS) 

.PHONY: all exec libmossco_gotmfabm libgotm

all: exec
exec: esmf_gotmfabm

esmf_gotmfabm: toplevel_component.o main.o  
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)
	@echo "Created example binary $(MOSSCO_DIR)/examples/$@/$@" 
	@echo "Please use the binary $(MOSSCO_DIR)/examples/$@/$@." > $(MOSSCO_BIN_PATH)/$@.txt

libmossco_gotmfabm libgotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@
	
main.o: toplevel_component.o main.F90 libmossco_util
main.F90: ../common/main.F90
	ln -f $< $@

toplevel_component.o: toplevel_component.F90 libmossco_gotmfabm libmossco_util  libsolver  libgotm

clean: extraclean

extraclean: 
	@-rm -f esmf_gotmfabm main.F90
