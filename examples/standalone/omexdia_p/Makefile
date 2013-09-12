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
export MOSSCO_DIR=$(subst /examples/standalone/omexdia_p$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lsediment -lsolver $(LIBS)

F90FLAGS += $(INCLUDES)
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)
FABMLIBTARGET=$(FABM_LIBRARY_PATH)/$(FABM_LIBRARY_FILE)

.PHONY: libsediment fabmdriver exec

all: exec

fabmlib: $(FABMLIBTARGET)

$(FABMLIBTARGET):
	(export FABMHOST=mossco ; $(MAKE) -C $(FABMDIR) )
	(export FABMHOST=mossco ; $(MAKE) -C $(FABMDIR)/src )

fabmdriver: fabmlib libsolver libsediment

libsediment: prefix $(MOSSCO_MODULE_PATH)/fabm_sediment_driver.mod $(MOSSCO_DIR)/src/drivers/fabm_sediment_driver.o
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libsediment.a $(MOSSCO_DIR)/src/drivers/fabm_sediment_driver.o
	@echo "Created sediment library  $(MOSSCO_LIBRARY_PATH)/libsediment.a" 

$(MOSSCO_DIR)/src/drivers/fabm_sediment_driver.o $(MOSSCO_MODULE_PATH)/fabm_sediment_driver.mod: 
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers

libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities

exec: fabmdriver main.F90
	$(F90) $(F90FLAGS) -c -o omexdia_p_test.o main.F90 
	$(F90) -o omexdia_p_test omexdia_p_test.o $(LDFLAGS)
	@echo "Created example binary omexdia_p_test in  $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/omexdia_p to run the example." > $(MOSSCO_BIN_PATH)/omexdia_p.txt

clean: extraclean

extraclean: 
	@rm -f output.dat omexdia_p_test
