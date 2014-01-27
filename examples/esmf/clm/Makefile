# This Makefile is part of MOSSCO
# 
# @copyright (C) 2013, 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#

ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/esmf/clm$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lmossco_clm -lmossco_util -lesmf $(LIBS)
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec libmossco_clm libmossco_util

all: exec

exec: toplevel_component.o main.o
	$(F90) $(F90FLAGS) -o clm $^ $(LDFLAGS)
	@echo "Created example binary clm in $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/esmf/clm to run the example." > $(MOSSCO_BIN_PATH)/esmf_clm.txt

libmossco_clm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/clm $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

data: atmos.nc

atmos.nc:
	@-ln -s /media/data/forcing/CLM/cDII.00.kss.2003.nc $@ || \
	ln -s /h/ksedata02/data/model/CLM/cDII.00.kss.2003.nc $@ || \
	echo "Could not find data file cDII.00.kss.2003.nc."
	
main.o: toplevel_component.o main.F90 libmossco_util
toplevel_component.o: toplevel_component.F90 libmossco_clm
main.F90: ../../common/main.F90
	ln -f  $< $@

clean: extraclean

extraclean: 
	@rm -f clm main.F90
	@rm -f atmos_*.nc PET*
