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
export MOSSCO_DIR=$(subst /examples/esmf_empty$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lesmf
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec

all: exec

exec: main.o toplevel_component.o
	$(F90) $(F90FLAGS) -o esmf_empty toplevel_component.o main.o $(LDFLAGS)
	@echo "Created example binary esmf_empty in $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/esmf_empty/ to run the example." > $(MOSSCO_BIN_PATH)/esmf_empty.txt
	
main.o: toplevel_component.o
toplevel_component.o: toplevel_component.F90

clean: extraclean

extraclean: 
	@rm -f esmf_empty