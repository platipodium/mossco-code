# This Makefile is part of MOSSCO
#
# @copyright (C) 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/esmf/pelagicfabm1d$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifneq ($(MOSSCO_GOTM_FABM),true)
$(error This example only works with MOSSCO_GOTM_FABM=true)
endif

MOSSCO_LDFLAGS += -L$(MOSSCO_LIBRARY_PATH) -lmossco_gotmfabm -lmossco_fabmpelagic -lmossco_gotm -lsolver -lmossco_util
MOSSCO_LDFLAGS += $(GOTM_LDFLAGS)
MOSSCO_LDFLAGS += $(FABM_LDFLAGS)
MOSSCO_LDFLAGS += $(ESMF_F90LDFLAGS)

.PHONY: all exec pelagicfabm1d libmossco_gotmfabm

all: exec
exec: libmossco_util libsolver libmossco_gotmfabm pelagicfabm1d

pelagicfabm1d: toplevel_component.o ../../common/main.o
	$(F90) $^ $(F90FLAGS) $(MOSSCO_LDFLAGS) $(LDFLAGS) -o $@
	@echo "Created example binary $(CURDIR)/$@"

libmossco_gotmfabm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_util libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

clean: extraclean

extraclean:
	@-rm -f pelagicfabm1d main.F90
