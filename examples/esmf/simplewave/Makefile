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

LIBS := -lmossco_simplewave -lmossco_util -lesmf $(LIBS)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec

all: exec

exec: libmossco_simplewave esmf_simplewave

esmf_simplewave: toplevel_component.o ../../common/main.o
	$(F90) $(F90FLAGS) $^ -o $@ $(LDFLAGS)
	@echo "Created example binary esmf_simplewave in $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/esmf_simplewave/ to run the example." > $(MOSSCO_BIN_PATH)/esmf_simplewave.txt

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libmossco_simplewave:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@


clean: extraclean

extraclean: 
	@rm -f esmf_simplewave
	@rm -f PET*.*
