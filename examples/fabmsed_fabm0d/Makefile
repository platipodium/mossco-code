# This Makefile is part of MOSSCO
# 
# Copyright (C) 2013 Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/fabmsed_fabm0d$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

LIBS := -lsediment -lfabm0d -lmossco_fabm0d -lsolver -lesmf $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS) -L$(GOTMDIR)/lib/GFORTRAN -lairsea_prod -linput_prod -lutil_prod 

.PHONY: all exec libfabm0d libsediment

all: exec

exec: main.o libsediment libfabm0d toplevel_component.o
	$(F90) $(F90FLAGS) -o fabmsed_fabm0d toplevel_component.o main.o $(LDFLAGS)
	@echo "Created example binary fabmsed_fabm0d in  $(PWD)" 

libsediment: prefix $(MOSSCO_MODULE_PATH)/fabm_sediment_component.mod $(MOSSCO_DIR)/src/components/fabm_sediment_component.o
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libsediment.a $(MOSSCO_DIR)/src/components/fabm_sediment_component.o
	@echo "Created sediment library  $(MOSSCO_LIBRARY_PATH)/libsediment.a" 

libfabm0d: prefix $(MOSSCO_MODULE_PATH)/fabm_0d_component.mod $(MOSSCO_DIR)/src/components/fabm_0d_component.o
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libfabm0d.a $(MOSSCO_DIR)/src/components/fabm_0d_component.o
	@echo "Created fabm0d library  $(MOSSCO_LIBRARY_PATH)/libfabm0d.a" 


$(MOSSCO_MODULE_PATH)/fabm0d_component.mod: $(MOSSCO_DIR)/src/components/fabm_0d_component.F90
	$(MAKE) -C $(MOSSCO_DIR)/src/components libfabm0d

main.o: toplevel_component.o
toplevel_component.o: toplevel_component.F90   $(MOSSCO_MODULE_PATH)/fabm_sediment_component.mod  $(MOSSCO_MODULE_PATH)/fabm0d_component.mod


$(MOSSCO_MODULE_PATH)/fabm_sediment_component.mod:
	$(MAKE) -C $(MOSSCO_DIR)/src/components libsediment

clean: extraclean

extraclean: 
	@rm -f fabmsed_fabm0d
	@rm -f PET*.* swr.png output.dat 
