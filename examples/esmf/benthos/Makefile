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
export MOSSCO_DIR=$(subst /examples/esmf/benthos$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lmossco_benthos -lmossco_util -lesmf $(LIBS)
MOSSCO_LDFLAGS += $(MOSSCO_F90LDFLAGS) $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec benthos libmossco_benthos libmossco_util

all: exec
exec: libmossco_util libmossco_benthos benthos

benthos: toplevel_component.o ../../common/main.o
	$(F90) $(F90FLAGS) $^ $(MOSSCO_LDFLAGS) $(LDFLAGS) -o $@
	@echo "Created example $(CURDIR)/$@"

libmossco_benthos:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

clean: extraclean

extraclean:
	@rm -f benthos main.F90
	@rm -f PET*
