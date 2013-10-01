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
export MOSSCO_DIR=$(subst /examples/esmf_gotm$,,$(PWD))
endif

export FABMHOST=gotm
include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

LIBS := -lesmf -lgotm $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS) -L$(GOTMDIR)/lib/GFORTRAN -lgotm_prod -lairsea_prod -lmeanflow_prod -lseagrass_prod -lturbulence_prod -lutil_prod -lgotm_fabm_prod -lutil_prod -loutput_prod -lobservations_prod  -linput_prod $(FABMDIR)/lib/gotm/GFORTRAN/libfabm_prod.a $(NC_LIBS) 

.PHONY: all exec libgotm

all: exec

libgotm: prefix $(MOSSCO_MODULE_PATH)/gotm_component.mod $(MOSSCO_DIR)/src/components/gotm_component.o
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libgotm.a $(MOSSCO_DIR)/src/components/gotm_component.o
	@echo "Created gotm library  $(MOSSCO_LIBRARY_PATH)/libgotm.a" 

exec: toplevel_component.o main.o
	$(F90) $(F90FLAGS) -o esmf_gotm_example $^ $(LDFLAGS)
	@echo "Created example binary esmf_example_test in  $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/esmf_gotm to run the example." > $(MOSSCO_BIN_PATH)/esmf_gotm.txt

$(MOSSCO_MODULE_PATH)/gotm_component.mod: $(MOSSCO_DIR)/src/components/gotm_component.F90
	$(MAKE) -C $(MOSSCO_DIR)/src/components libgotm

main.o: toplevel_component.o
toplevel_component.o: toplevel_component.F90 libgotm

clean: extraclean

extraclean: 
	@rm -f esmf_gotm
	@rm -f PET*.*
