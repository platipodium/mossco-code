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
export MOSSCO_DIR=$(subst /examples/esmf/default$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS :=  -ldefault -lmossco_util -lesmf $(LIBS)
MOSSCO_LDFLAGS += $(MOSSCO_F90LDFLAGS)

.PHONY: all exec default

all: exec
exec: libmossco_util libdefault default

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

constant: toplevel_component.o ../../common/main.o
	$(F90) $(F90FLAGS) $^ $(MOSSCO_LDFLAGS) $(LDFLAGS) $(LIBS) -o $@
	@echo "Created example binary $(CURDIR)/$@"

libconstant:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

clean: extraclean

extraclean:
	@rm -f default main.F90
