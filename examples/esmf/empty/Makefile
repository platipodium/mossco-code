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
export MOSSCO_DIR=$(subst /examples/esmf/empty$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS :=  -lempty -lmossco_util -lesmf $(LIBS)
LDFLAGS += $(MOSSCO_LDFLAGS)

.PHONY: all exec empty

all: exec
exec: libmossco_util libempty empty

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

empty: toplevel_component.o ../../common/main.o
	$(F90) $(F90FLAGS) $^ $(LDFLAGS) $(LIBS) -o $@
	@echo "Created example $(PWD)/$@"

libempty:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

clean: extraclean

extraclean:
	@rm -f empty main.F90
