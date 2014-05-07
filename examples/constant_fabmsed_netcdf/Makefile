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
export MOSSCO_DIR=$(subst /examples/constant_fabmsed_netcdf$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

LIBS := -lsediment -lconstant -lmossco_sediment -lmossco_netcdf -lesmf  $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS)
LDFLAGS += -lpelagicbenthiccoupler -lsolver -lmossco_util
LDFLAGS += -L$(FABM_LIBRARY_PATH) -lfabm_prod
LDFLAGS += -lmossco_util -lesmf
LDFLAGS += $(NC_LIBS) 

.PHONY: all exec libsediment libmossco_sediment libconstant libmossco_netcdf

all: exec
exec: constant_fabmsed_netcdf

constant_fabmsed_netcdf: toplevel_component.o main.o  
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)
	@echo "Created example binary $@ in $(PWD)" 
	@echo "Please copy this binary $(MOSSCO_DIR)/examples/$@/$@ to a setup directory." > $(MOSSCO_BIN_PATH)/$@.txt

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libsediment libconstant libmossco_netcdf:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libpelagicbenthiccoupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators pelagicbenthiccoupler benthicpelagiccoupler

main.o: toplevel_component.o main.F90 libmossco_util
main.F90: ../common/main.F90
	ln -f $< $@

toplevel_component.o: toplevel_component.F90 libmossco_util libsediment libmossco_sediment libconstant libpelagicbenthiccoupler libmossco_netcdf

clean: extraclean

extraclean: 
	@-rm -f constant_fabmsed_netcdf main.F90
