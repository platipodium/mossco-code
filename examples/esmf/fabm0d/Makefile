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
export MOSSCO_DIR=$(subst /examples/esmf/fabm0d$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifneq ($(MOSSCO_GOTM),true)
$(error You need GOTM to compile this example)
endif

LDFLAGS += -L$(MOSSCO_LIBRARY_PATH) -lmossco_fabm0d -lsolver -lmossco_util
LDFLAGS += -L$(GOTM_LIBRARY_PATH) -lairsea -linput -lutil
LDFLAGS += $(FABM_LDFLAGS)
LDFLAGS += $(ESMF_F90LDFLAGS)
#ifeq ($(HAVE_LD_FORCE_LOAD),true)
#LDFLAGS += -Wl,-force_load
#endif

.PHONY: all exec fabm0d

all: exec
exec: libmossco_util libmossco_fabm0d fabm0d

libmossco_fabm0d:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

fabm0d: toplevel_component.o ../../common/main.o
	$(F90) $(F90FLAGS) $^ -o $@ $(LDFLAGS)
	@echo "Created example binary $(PWD)/$@"

clean: extraclean

extraclean:
	@rm -f fabm0d toplevel_component.o
	@rm -f PET*.*
