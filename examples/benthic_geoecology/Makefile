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

LDFLAGS += $(MOSSCO_LDFLAGS) $(LIBRARY_PATHS) $(LIBS)
LDFLAGS += -lgotm -lmossco_fabmgotm -lsolver -lmossco_util
LDFLAGS += $(GOTM_LDFLAGS)
LDFLAGS += -L$(FABM_LIBRARY_PATH) -lfabm_prod
LDFLAGS += -lmossco_util -lesmf $(NC_LIBS)

.PHONY: all exec libmossco_fabmgotm libconstant libgotm libmossco_erosed libmossco_benthos

all: exec
exec: libmossco_util libmossco_fabmgotm libconstant libgotm libmossco_erosed libmossco_benthos libmossco_netcdf benthic_geoecology

benthic_geoecology: toplevel_component.o ../common/main.o  
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)
	@echo "Created example binary $(PWD)/$@" 

libmossco_fabmgotm libgotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libsediment libconstant libmossco_erosed libmossco_benthos libmossco_netcdf:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

clean: extraclean

extraclean: 
	@-rm -f benthic_geoecology
	@-rm -f toplevel_component.o
