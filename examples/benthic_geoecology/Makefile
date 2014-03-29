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
export MOSSCO_DIR=$(subst /examples/benthic_geoecology$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifneq ($(MOSSCO_GOTM_FABM),true)
$(error This example only works with MOSSCO_GOTM_FABM=true)
endif

LIBS := -lconstant -lmossco_erosed -lmossco_benthos -lmossco_netcdf -lesmf  $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS)
LDFLAGS += -lgotm -lmossco_gotmfabm -lsolver -lmossco_util
LDFLAGS += -L$(GOTM_LIBRARY_PATH) -lgotm_prod
LDFLAGS += -L$(FABM_LIBRARY_PATH) -lfabm_prod
LDFLAGS += -lmossco_util -lesmf
LDFLAGS += -lairsea_prod -lmeanflow_prod -lseagrass_prod -lturbulence_prod -lutil_prod -loutput_prod -lobservations_prod  -linput_prod $(NC_LIBS) 

.PHONY: all exec libmossco_gotmfabm libconstant libgotm libmossco_erosed libmossco_benthos

all: exec
exec: benthic_geoecology

benthic_geoecology: toplevel_component.o main.o  
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)
	@echo "Created example binary $@ in $(PWD)" 
	@echo "Please copy this binary $(MOSSCO_DIR)/examples/$@/$@ to a setup directory." > $(MOSSCO_BIN_PATH)/$@.txt

libmossco_gotmfabm libgotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libsediment libconstant libmossco_erosed libmossco_benthos libmossco_netcdf:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

main.o: toplevel_component.o main.F90 libmossco_util
main.F90: ../common/main.F90
	ln -f $< $@

toplevel_component.o: toplevel_component.F90 libmossco_gotmfabm libmossco_util libconstant libgotm libmossco_erosed libmossco_benthos libmossco_netcdf

clean: extraclean

extraclean: 
	@-rm -f benthic_geoecology main.F90
	@-rm -f main.o toplevel_component.o
