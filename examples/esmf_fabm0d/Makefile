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
export MOSSCO_DIR=$(subst /examples/esmf_fabm0d$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifneq ($(MOSSCO_GOTM),true)
$(error You need GOTM to compile this example)
endif


LIBS := -lesmf $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS) -lfabm0d -lmossco_fabm0d -lsolver -L$(GOTMDIR)/lib/GFORTRAN -lairsea_prod -linput_prod -lutil_prod 
ifeq ($(HAVE_LD_FORCE_LOAD),true)
LDFLAGS += -Wl,-force_load
endif
LDFLAGS += $(FABMDIR)/lib/mossco/GFORTRAN/libfabm_prod.a 

.PHONY: all exec libfabm0d

all: exec

libfabm0d: prefix $(MOSSCO_MODULE_PATH)/fabm_0d_component.mod $(MOSSCO_DIR)/src/components/fabm_0d_component.o
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libfabm0d.a $(MOSSCO_DIR)/src/components/fabm_0d_component.o
	@echo "Created fabm0d library  $(MOSSCO_LIBRARY_PATH)/libfabm0d.a" 

exec: main.o libfabm0d
	$(F90) $(F90FLAGS) -o esmf_fabm0d toplevel_component.o main.o $(LDFLAGS)
	@echo "Created example binary esmf_fabm0d in  $(PWD)" 
	@echo "Please change to $(FABMDIR)/testcases/0d to run the example." > $(MOSSCO_BIN_PATH)/esmf_fabm0d.txt

$(MOSSCO_MODULE_PATH)/esmf_fabm_0d_component.mod: $(MOSSCO_DIR)/src/components/esmf_fabm_0d_component.F90
	$(MAKE) -C $(MOSSCO_DIR)/src/components

main.o: toplevel_component.o
toplevel_component.o: toplevel_component.F90

fabm0d:
	( unset LIBS; export FABMHOST=0d ; $(MAKE) -C $(FABMDIR)/src/ )
	( unset LIBS; export FABMHOST=0d ; $(MAKE) -C $(FABMDIR)/src/drivers/0d fabm0d )
	mv $(FABMDIR)/src/drivers/0d/fabm0d .

clean: extraclean

extraclean: 
	@rm -f esmf_fabm0d
	@rm -f fabm0d
	@rm -f PET*.*
