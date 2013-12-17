# This Makefile is part of MOSSCO
# 
# @copyright (C) 2013 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#

ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/esmf_erosed$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lerosed -lmossco_erosed -lesmf $(LIBS)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec

all: exec

exec: toplevel_component.o main.o
	$(F90) $(F90FLAGS) -o esmf_erosed $^ $(LDFLAGS)
	@echo "Created example binary esmf_erosed in $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/esmf_erosed/ to run the example." > $(MOSSCO_BIN_PATH)/esmf_erosed.txt

liberosed:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@
	
main.o: toplevel_component.o
toplevel_component.o: toplevel_component.F90 liberosed

clean: extraclean

extraclean: 
	@rm -f esmf_erosed
	@rm -f PET*.*
