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

.PHONY: exec libsediment

all: exec

libsediment:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

exec: main.F90 libsolver libsediment
	$(F90) $(F90FLAGS) -c -o omexdia_p.o main.F90 
	$(F90) -o omexdia_p omexdia_p.o $(LDFLAGS)
	@echo "Created example binary omexdia_p in  $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/standalone/omexdia_p to run the example." > $(MOSSCO_BIN_PATH)/omexdia_p.txt

clean: extraclean

extraclean: 
	@rm -f output.dat omexdia_p
