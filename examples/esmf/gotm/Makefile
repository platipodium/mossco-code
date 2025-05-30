# This Makefile is part of MOSSCO
#
# @copyright (C) 2013--2019 Helmholtz-Zentrum Geesthacht
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

MOSSCO_LDFLAGS += -L$(MOSSCO_LIBRARY_PATH)
MOSSCO_LDFLAGS += -lmossco_gotm -lmossco_util
MOSSCO_LDFLAGS += $(GOTM_LDFLAGS)
MOSSCO_LDFLAGS += $(ESMF_F90LDFLAGS)

.PHONY: all exec gotm libmossco_gotm libmossco_util

all: exec
exec: libmossco_util libmossco_gotm gotm

libsolver libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libmossco_gotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

gotm: toplevel_component.o ../../common/main.o
	$(F90) $(F90FLAGS) $^ $(MOSSCO_LDFLAGS) $(LDFLAGS) -o $@
	@echo "Created example binary $(CURDIR)/$@"

clean: extraclean

extraclean:
	@rm -f gotm toplevel_component.o
	@rm -f PET*.*
