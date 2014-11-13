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
export MOSSCO_DIR=$(subst /examples/fabmgotm_fabmsed_constant$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifneq ($(MOSSCO_GOTM_FABM),true)
$(error This example only works with MOSSCO_GOTM_FABM=true)
endif

LIBS := -lsediment -lconstant -lsurfacescoupler -lmossco_sediment -lesmf  $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(MOSSCO_LDFLAGS) $(LIBRARY_PATHS) $(LIBS)
LDFLAGS += -lgotm -lmossco_fabmgotm -lsolver -lmossco_util
LDFLAGS += $(GOTM_LDFLAGS)
LDFLAGS += $(FABM_LDFLAGS)
LDFLAGS += -lmossco_util -lesmf
LDFLAGS += $(NC_LIBS)

.PHONY: all exec fabmgotm_fabmsed_constant libsediment libmossco_fabmgotm libmossco_sediment libconstant libgotm libsurfacescoupler

all: exec
exec: libmossco_util libmossco_fabmgotm libsediment libmossco_sediment libsolver libconstant libgotm libsurfacescoupler fabmgotm_fabmsed_constant

fabmgotm_fabmsed_constant: toplevel_component.o ../common/main.o
	$(F90) $(F90FLAGS)  $^ $(LDFLAGS) -o $@
	@echo "Created example binary $(PWD)/$@"

libmossco_fabmgotm libgotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libsediment libconstant:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_sediment libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

libsurfacescoupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators $@

clean: extraclean

extraclean:
	@-rm -f fabmgotm_fabmsed_constant main.F90
