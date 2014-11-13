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
export MOSSCO_DIR=$(subst /examples/constant_fabmsed_netcdf$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

LIBS := -lsediment -lconstant -lmossco_sediment -lmossco_netcdf -lesmf  $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(MOSSCO_LDFLAGS) $(LIBRARY_PATHS) $(LIBS)
LDFLAGS += -lpelagicbenthiccoupler -lsolver -lmossco_util
LDFLAGS += $(FABM_LDFLAGS)
LDFLAGS += -lmossco_util -lesmf
LDFLAGS += $(NC_LIBS)

.PHONY: all exec constant_fabmsed_netcdf libsediment libmossco_sediment libconstant libmossco_netcdf

all: exec
exec: libmossco_util libsediment libmossco_sediment libconstant libpelagicbenthiccoupler libmossco_netcdf constant_fabmsed_netcdf

deprecation:
	@-echo This hardcoded example is deprecated and will be removed in the near future.
	@-echo You can obtain the same simulation with the generic example found in
	@-echo $(MOSSCO_DIR)/examples/generic
	@-echo and the configuration constant--fabm_sediment--netcdf.yaml.
	@-echo In that directory, execute the following:
	@-echo     python ./create_coupling.py constant--fabm_sediment--netcdf.yaml && make
	@-echo In your setups directory, execute the generated program
	@-echo     $(MOSSCO_DIR)/examples/generic/coupling

constant_fabmsed_netcdf: toplevel_component.o ../common/main.o
	$(F90) $(F90FLAGS) $^ $(LDFLAGS) -o $@
	@echo "Created example binary $(PWD)/$@"

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libsediment libconstant libmossco_netcdf:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libpelagicbenthiccoupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators pelagicbenthiccoupler benthicpelagiccoupler

clean: extraclean

extraclean:
	@-rm -f constant_fabmsed_netcdf main.F90
