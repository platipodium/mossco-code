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
export MOSSCO_DIR=$(subst /examples/ocean_sediment$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lsediment -lsolver -lesmf -locean -loceansedimentcoupler $(LIBS)
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec libocean libsediment liboceansedimentcoupler

all: exec

exec: main.o esmf_toplevel_component.o libocean libsediment liboceansedimentcoupler
	$(F90) $(F90FLAGS) -o ocean_sediment esmf_toplevel_component.o main.o $(LDFLAGS)
	@echo "Created example binary ocean_sediment in $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/ocean_sediment/ to run the example." > $(MOSSCO_BIN_PATH)/ocean_sediment.txt

libocean: $(MOSSCO_LIBRARY_PATH)/libocean.a
liboceansedimentcoupler: $(MOSSCO_LIBRARY_PATH)/liboceansedimentcoupler.a
libsediment: $(MOSSCO_LIBRARY_PATH)/libsediment.a

$(MOSSCO_LIBRARY_PATH)/libsediment.a:
	$(MAKE) -C $(MOSSCO_DIR)/src/components libsediment
	
$(MOSSCO_LIBRARY_PATH)/liboceansedimentcoupler.a:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators liboceansedimentcoupler
	
$(MOSSCO_LIBRARY_PATH)/libocean.a:
	$(MAKE) -C $(MOSSCO_DIR)/src/components libocean
	
main.o: esmf_toplevel_component.o
esmf_toplevel_component.o: esmf_toplevel_component.F90

clean: extraclean

extraclean: 
	@rm -f ocean_sediment
