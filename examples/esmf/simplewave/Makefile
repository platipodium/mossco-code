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
export MOSSCO_DIR=$(subst /examples/esmf/simplewave$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LDFLAGS += -L$(MOSSCO_LIBRARY_PATH) -lmossco_simplewave -lmossco_util
LDFLAGS += $(ESMF_LDFLAGS)

.PHONY: all exec simplewave libmossco_simplewave libmossco_util

all: exec

exec: libmossco_util libmossco_simplewave simplewave

simplewave: toplevel_component.o ../../common/main.o
	$(F90) $(F90FLAGS) $^ $(LDFLAGS) -o $@
	@echo "Created example binary $(PWD)/$@" 

libmossco_simplewave:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@


clean: extraclean

extraclean: 
	@rm -f esmf_simplewave
	@rm -f PET*.*
