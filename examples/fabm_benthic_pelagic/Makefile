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
export MOSSCO_DIR=$(subst /examples/fabm_benthic_pelagic$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifneq ($(MOSSCO_GOTM_FABM),true)
$(error This example only works with MOSSCO_GOTM_FABM=true)
endif

LIBS := -lsediment -lconstant -lerosed -lmossco_erosed -lmossco_sediment -lesmf  $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS)
LDFLAGS += -lgotm -lmossco_gotmfabm -lpelagicbenthiccoupler -lsolver -lmossco_util
LDFLAGS += -L$(GOTM_LIBRARY_PATH) -lgotm_prod
LDFLAGS += -L$(FABM_LIBRARY_PATH) -lfabm_prod
LDFLAGS += -lmossco_util -lesmf
LDFLAGS += -lairsea_prod -lmeanflow_prod -lseagrass_prod -lturbulence_prod -lutil_prod -loutput_prod -lobservations_prod  -linput_prod $(NC_LIBS) 

.PHONY: all exec libsediment libmossco_gotmfabm libmossco_sediment libconstant libgotm libmossco_erosed liberosed

all: exec
exec: fabm_benthic_pelagic

fabm_benthic_pelagic: toplevel_component.o main.o  
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)
	@echo "Created example binary $@ in $(PWD)" 
	@echo "Please copy this binary $(MOSSCO_DIR)/examples/$@/$@ to a setup directory." > $(MOSSCO_BIN_PATH)/$@.txt

libmossco_gotmfabm libgotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libsediment libconstant liberosed:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libpelagicbenthiccoupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators pelagicbenthiccoupler benthicpelagiccoupler

libmossco_erosed libmossco_sediment libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

main.o: toplevel_component.o main.F90 libmossco_util
main.F90: ../common/main.F90
	ln -f $< $@

toplevel_component.o: toplevel_component.F90 libmossco_gotmfabm libmossco_util libsediment libmossco_sediment libsolver libconstant libgotm libpelagicbenthiccoupler libmossco_erosed liberosed

clean: extraclean

extraclean: 
	@-rm -f fabm_benthic_pelagic main.F90
