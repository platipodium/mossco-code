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
export MOSSCO_DIR=$(subst /examples/fabmsed_fabm0d$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

LIBS :=  -lsediment -lmossco_fabm0d -lmossco_sediment -lsolver -lesmf -lmossco_util $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(MOSSCO_LDFLAGS) $(LIBRARY_PATHS) $(LIBS)
LDFLAGS += $(FABM_LDFLAGS)
LDFLAGS += -L$(GOTM_LIBRARY_PATH) -lairsea_prod -linput_prod -lutil_prod

.PHONY: all exec fabmsed_fabm0d

all: exec
exec: libmossco_util libmossco_fabm0d libsediment fabmsed_fabm0d

fabmsed_fabm0d: toplevel_component.o ../common/main.o
	$(F90) $(F90FLAGS) $^ $(LDFLAGS) -o $@
	@echo "Created example binary $(PWD)/$@"

libmossco_fabm0d libsediment:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

toplevel_component.o: toplevel_component.F90

clean: extraclean

extraclean:
	@rm -f fabmsed_fabm0d main.F90
	@rm -f PET*.* swr.png output.dat
