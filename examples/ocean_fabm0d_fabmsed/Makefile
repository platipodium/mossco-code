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
export MOSSCO_DIR=$(subst /examples/ocean_fabm0d_fabmsed$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

NC_LIBS += $(shell nf-config --flibs)
LIBS := -lsediment -lsolver -lesmf -locean -loceansedimentcoupler $(LIBS) -lfabm0d -lmossco_fabm0d -lsolver -L$(GOTMDIR)/lib/GFORTRAN -lairsea_prod -linput_prod -lutil_prod 
ifeq ($(HAVE_LD_FORCE_LOAD),true)
LDFLAGS += -Wl,-force_load
endif
LDFLAGS += $(FABMDIR)/lib/mossco/GFORTRAN/libfabm_prod.a 

LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec libocean libsediment liboceansedimentcoupler

all: exec
exec: ocean_fabm0d_fabmsed

ocean_fabm0d_fabmsed: toplevel_component.o main.o  
	$(F90) $(F90FLAGS) -o ocean_fabm0d_fabmsed  $^ $(LDFLAGS)
	@echo "Created example binary ocean_fabm0d_fabmsed in $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/ocean_fabm0d_fabmsed/ to run the example." > $(MOSSCO_BIN_PATH)/ocean_fabm0d_fabmsed.txt

libfabm0d libsediment libocean:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

liboceansedimentcoupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators $@
	
main.o: toplevel_component.o
toplevel_component.o: toplevel_component.F90 libfabm0d libsediment libocean liboceansedimentcoupler

clean: extraclean

extraclean: 
	@rm -f ocean_sediment
