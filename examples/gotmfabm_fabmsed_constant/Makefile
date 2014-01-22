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
export MOSSCO_DIR=$(subst /examples/gotmfabm_fabmsed_constant$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifneq ($(MOSSCO_GOTM_FABM),true)
$(error This example only works with MOSSCO_GOTM_FABM=true)
endif

LIBS := -lsediment -lconstant -lmossco_sediment -lmossco_util -lesmf  $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS)
LDFLAGS += -lgotm -lmossco_gotmfabm -lsolver
LDFLAGS += -L$(GOTM_LIBRARY_PATH) -lgotm_prod
LDFLAGS += -L$(FABM_LIBRARY_PATH) -lfabm_prod
LDFLAGS += -lairsea_prod -lmeanflow_prod -lseagrass_prod -lturbulence_prod -lutil_prod -lutil_prod -loutput_prod -lobservations_prod  -linput_prod $(NC_LIBS) 

.PHONY: all exec libsediment libmossco_gotmfabm libmossco_sediment libconstant libgotm

all: exec
exec: gotmfabm_fabmsed_constant

gotmfabm_fabmsed_constant: toplevel_component.o main.o  
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)
	@echo "Created example binary $@ in $(PWD)" 
	@echo "Please copy this binary $(MOSSCO_DIR)/examples/$@/$@ to a setup directory." > $(MOSSCO_BIN_PATH)/$@.txt

libmossco_gotmfabm libgotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libsediment libconstant:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_sediment libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

main.o: toplevel_component.o main.F90
main.F90: ../common/main.F90
	ln $< $@

toplevel_component.o: toplevel_component.F90 libmossco_gotmfabm libmossco_util libsediment libmossco_sediment libsolver libconstant libgotm

clean: extraclean

extraclean: 
	@-rm -f gotmfabm_fabmsed_constant
