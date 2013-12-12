# This Makefile is part of MOSSCO
# 
# Copyright (C) 2013 Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#                Richard Hofmeister, Helmholtz-Zentrum Geesthacht    
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#

ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/gotmfabm_fabmsed$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifneq ($(MOSSCO_GOTM_FABM),true)
$(error This example only works with MOSSCO_GOTM_FABM=true)
endif

LIBS := -lsediment -lmossco_sediment -lesmf  $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS)
LDFLAGS += -L$(GOTM_LIBRARY_PATH) -lgotm_prod
LDFLAGS += -lmossco_gotmfabm -lsolver
LDFLAGS += -L$(FABM_LIBRARY_PATH) -lfabm_prod
LDFLAGS += -lairsea_prod -lmeanflow_prod -lseagrass_prod -lturbulence_prod -lutil_prod -lutil_prod -loutput_prod -lobservations_prod  -linput_prod $(NC_LIBS) 

.PHONY: all exec libsediment libmossco_gotmfabm libmossco_sediment 

all: exec
exec: gotmfabm_fabmsed

gotmfabm_fabmsed: toplevel_component.o main.o  
	$(F90) $(F90FLAGS) -o gotmfabm_fabmsed  $^ $(LDFLAGS)
	@echo "Created example binary gotmfabm_fabmsed in $(PWD)" 
	@echo "Please copy this binary $(MOSSCO_DIR)/examples/gotmfabm_fabmsed/gotmfabm_fabmsed to a setup directory." > $(MOSSCO_BIN_PATH)/gotmfabm_fabmsed.txt

libsediment libmossco_gotmfabm libmossco_sediment:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

main.o: toplevel_component.o
toplevel_component.o: toplevel_component.F90 libmossco_gotmfabm libsediment libmossco_sediment libsolver

clean: extraclean

extraclean: 
	@rm -f 
