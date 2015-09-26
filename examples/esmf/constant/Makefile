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
export MOSSCO_DIR=$(subst /examples/esmf/constant$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS :=  -lconstant -lmossco_util -lesmf $(LIBS)
LDFLAGS += $(MOSSCO_F90LDFLAGS)

.PHONY: all exec constant

all: exec
exec: libmossco_util libconstant constant

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

constant: toplevel_component.o ../../common/main.o
	$(F90) $(F90FLAGS) $^ $(LDFLAGS) $(LIBS) -o $@
	@echo "Created example binary $(CURDIR)/$@"

libconstant:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

clean: extraclean

extraclean:
	@rm -f constant main.F90
