# This Makefile is part of MOSSCO
#
# @copyright (C) 2013, 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
# @author Richard Hofmeister, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/esmf/gotm$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

LDFLAGS += -L$(MOSSCO_LIBRARY_PATH)

ifeq ($(MOSSCO_GOTM_FABM),true)
  LDFLAGS += -lmossco_gotmfabm -lsolver
endif

LDFLAGS += -lmossco_gotm -lmossco_util
LDFLAGS += $(GOTM_LDFLAGS)
ifeq ($(MOSSCO_GOTM_FABM),true)
  LDFLAGS += $(FABM_LDFLAGS)
endif
  LDFLAGS += $(ESMF_F90LDFLAGS)


.PHONY: all exec gotm libmossco_gotm libmossco_util

all: exec
exec: libmossco_util libsolver libmossco_gotmfabm gotm

libsolver libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libmossco_gotmfabm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

gotm: toplevel_component.o ../../common/main.o
	$(F90) $(F90FLAGS) $^ $(LDFLAGS) -o $@
	@echo "Created example binary $(CURDIR)/$@"

clean: extraclean

extraclean:
	@rm -f gotm main.F90
	@rm -f PET*.*
