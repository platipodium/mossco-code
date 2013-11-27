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
export MOSSCO_DIR=$(subst /examples/esmf_remtc$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lesmf -locean -latmosphere -laocoupler $(LIBS)
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec libocean libatmosphere libaocoupler

all: exec

exec: toplevel_component.o main.o
	$(F90) $(F90FLAGS) -o esmf_remtc $^ $(LDFLAGS)
	@echo "Created example binary esmf_remtc in $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/esmf_remtc/ to run the example." > $(MOSSCO_BIN_PATH)/esmf_remtc.txt

libocean libatmosphere:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/remtc $@

libaocoupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators $@

main.o: toplevel_component.o 
toplevel_component.o: toplevel_component.F90 libocean libatmosphere libaocoupler


clean: extraclean

extraclean: 
	@rm -f esmf_remtc
