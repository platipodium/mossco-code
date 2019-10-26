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
export MOSSCO_DIR=$(subst /examples/default_fabmsed_netcdf$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

LIBS := -lsediment -ldefault -lmossco_sediment -lmossco_netcdf -lesmf  $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

MOSSCO_LDFLAGS += $(MOSSCO_F90LDFLAGS) $(LIBRARY_PATHS) $(LIBS)
MOSSCO_LDFLAGS += -lpelagicbenthiccoupler -lsolver -lmossco_util
MOSSCO_LDFLAGS += $(FABM_LDFLAGS)
MOSSCO_LDFLAGS += -lmossco_util -lesmf
MOSSCO_LDFLAGS += $(NC_LIBS)

.PHONY: all exec default_fabmsed_netcdf libsediment libmossco_sediment libdefault libmossco_netcdf

all: exec
exec: libmossco_util libsediment libmossco_sediment libdefault libpelagicbenthiccoupler libmossco_netcdf default_fabmsed_netcdf

deprecation:
	@-echo This hardcoded example is deprecated and will be removed in the near future.
	@-echo You can obtain the same simulation with the generic example found in
	@-echo $(MOSSCO_DIR)/examples/generic
	@-echo and the configuration default--fabm_sediment--netcdf.yaml.
	@-echo In that directory, execute the following:
	@-echo     python ./create_coupling.py default--fabm_sediment--netcdf.yaml && make
	@-echo In your setups directory, execute the generated program
	@-echo     $(MOSSCO_DIR)/examples/generic/coupling

default_fabmsed_netcdf: toplevel_component.o ../common/main.o
	$(F90) $(F90FLAGS) $^ $(MOSSCO_LDFLAGS) $(LDFLAGS) -o $@
	@echo "Created example binary $(CURDIR)/$@"

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libsediment libdefault libmossco_netcdf:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libpelagicbenthiccoupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators pelagicbenthiccoupler benthicpelagiccoupler

clean: extraclean

extraclean:
	@-rm -f default_fabmsed_netcdf main.F90
