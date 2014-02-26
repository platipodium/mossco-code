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
export MOSSCO_DIR=$(subst /examples/esmf/remtc$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -locean -latmosphere -laocoupler -lmossco_util -lesmf $(LIBS)
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec libocean libatmosphere libaocoupler libmossco_util

all: exec
exec: remtc

remtc: toplevel_component.o main.o
	$(F90) $(F90FLAGS) -o $@ $^ $(LDFLAGS)
	@echo "Created example $(MOSSCO_DIR)/examples/esmf/$@"

libocean libatmosphere:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/remtc $@

libaocoupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@
	
main.o: toplevel_component.o main.F90 libmossco_util
toplevel_component.o: toplevel_component.F90 libocean libatmosphere libaocoupler
main.F90: ../../common/main.F90
	ln -f  $< $@


clean: extraclean

extraclean: 
	@rm -f esmf_remtc main.F90
