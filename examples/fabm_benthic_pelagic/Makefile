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
export MOSSCO_DIR=$(subst /examples/fabm_benthic_pelagic$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifneq ($(MOSSCO_GOTM_FABM),true)
$(error This example only works with MOSSCO_GOTM_FABM=true)
endif

ifeq ($(MOSSCO_EROSED),true)
EROSED_DEP = libmossco_erosed
EROSED_LIB = -lmossco_erosed
else
EROSED_DEP =
EROSED_LIB =
endif
LIBS := -lsediment -lconstant $(EROSED_LIB) -lmossco_sediment -lmossco_connector -lmossco_netcdf -lesmf  $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(MOSSCO_F90LDFLAGS) $(LIBRARY_PATHS) $(LIBS)
LDFLAGS += -lmossco_gotm -lmossco_fabmgotm -lpelagicbenthiccoupler -lsolver -lmossco_util
LDFLAGS += $(GOTM_LDFLAGS)
LDFLAGS += $(FABM_LDFLAGS)
LDFLAGS += -lmossco_util -lesmf
LDFLAGS += $(NC_LIBS)

.PHONY: all exec fabm_benthic_pelagic libsediment libmossco_fabmgotm libmossco_sediment libconstant libmossco_gotm $(EROSED_DEP)

all: exec
exec: libmossco_util libmossco_fabmgotm libsediment libmossco_sediment libconstant libmossco_gotm libpelagicbenthiccoupler $(EROSED_DEP) libmossco_connector libmossco_netcdf fabm_benthic_pelagic

fabm_benthic_pelagic: toplevel_component.o ../common/main.o
	$(F90) $(F90FLAGS) $^ $(LDFLAGS) -o $@
	@echo "Created example binary $(PWD)/$@"

libmossco_fabmgotm libmossco_gotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libmossco_connector:
	$(MAKE) -C $(MOSSCO_DIR)/src/connectors $@

libsediment libconstant libmossco_erosed libmossco_netcdf:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libpelagicbenthiccoupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators pelagicbenthiccoupler benthicpelagiccoupler

clean: extraclean

extraclean:
	@-rm -f fabm_benthic_pelagic
#	@-rm -f toplevel_component.o
