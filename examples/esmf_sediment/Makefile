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
export MOSSCO_DIR=$(subst /examples/esmf_sediment$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lesmf -lsediment -lsolver $(LIBS)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec libsediment

all: exec

exec: main.o libsediment esmf_toplevel_component.o
	$(F90) $(F90FLAGS) -o esmf_sediment_example esmf_toplevel_component.o main.o $(LDFLAGS)
	@echo "Created example binary esmf_example_test in  $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/esmf_sediment to run the example." > $(MOSSCO_BIN_PATH)/esmf_sediment.txt

libsediment: prefix $(MOSSCO_MODULE_PATH)/esmf_fabm_sediment_component.mod $(MOSSCO_DIR)/src/components/esmf_fabm_sediment_component.o
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libsediment.a $(MOSSCO_DIR)/src/components/esmf_fabm_sediment_component.o
	@echo "Created sediment library  $(MOSSCO_LIBRARY_PATH)/libsediment.a" 
	
$(MOSSCO_MODULE_PATH)/esmf_toplevel_component.mod: $(MOSSCO_DIR)/src/components/esmf_fabm_sediment_component.F90
	$(MAKE) -C $(MOSSCO_DIR)/src/components

main.o: esmf_toplevel_component.o
esmf_toplevel_component.o: esmf_toplevel_component.F90 $(MOSSCO_MODULE_PATH)/esmf_fabm_sediment_component.mod
$(MOSSCO_MODULE_PATH)/esmf_fabm_sediment_component.mod:
	$(MAKE) -C $(MOSSCO_DIR)/src/components

clean: extraclean

extraclean: 
	@rm -f esmf_sediment_example
