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
export MOSSCO_DIR=$(subst /examples/ocean_fabm0d$,,$(PWD))
endif

OCEAN=GOTM

include $(MOSSCO_DIR)/src/Rules.make

NC_LIBS += $(shell nf-config --flibs)
ifeq ($(OCEAN),GOTM)
LIBS :=  -locean -lgotm -lmossco_fabm0d -lsolver -lesmf $(LIBS)
else
LIBS := -locean -lmossco_fabm0d -lsolver -lesmf $(LIBS)
endif
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)
ifeq ($(OCEAN),GOTM)
LDFLAGS += -L$(GOTM_LIBRARY_PATH) -lgotm_prod -lairsea_prod -lmeanflow_prod -lturbulence_prod -lseagrass_prod -lobservations_prod -lutil_prod -linput_prod -loutput_prod
else
LDFLAGS += -L$(GOTM_LIBRARY_PATH) -lairsea_prod -linput_prod -lutil_prod
endif
LDFLAGS += -L$(FABM_LIBRARY_PATH) -lfabm_prod
ifeq ($(HAVE_LD_FORCE_LOAD),true)
LDFLAGS += -Wl,-force_load
endif


.PHONY: all exec libocean libsediment liboceansedimentcoupler libgotm

all: exec
exec: ocean_fabm0d

ocean_fabm0d: toplevel_component.o main.o
	$(F90) $(F90FLAGS) -o ocean_fabm0d  $^ $(LDFLAGS)
	@echo "Created example binary ocean_fabm0d in $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/ocean_fabm0d/ to run the example." > $(MOSSCO_BIN_PATH)/ocean_fabm0d.txt

libocean:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/remtc $@

libgotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_fabm0d:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@
	
main.o: toplevel_component.o main.F90 libmossco_util
ifeq ($(OCEAN),GOTM)
CPPFLAGS += -D_GOTMOCEAN_
toplevel_component.o: toplevel_component.F90 libmossco_fabm0d libgotm libocean
else
toplevel_component.o: toplevel_component.F90 libmossco_fabm0d libocean
endif

main.F90: ../common/main.F90
	ln -f  $< $@

clean: extraclean

extraclean: 
	@rm -f ocean_fabm0d
	@rm -f output_0d.dat main.F90
