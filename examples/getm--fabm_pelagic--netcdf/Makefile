# This Makefile is part of MOSSCO
#
# @copyright Copyright (C) 2014, Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, <carsten.lemmen@hzg.de>

#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

TARGET=getm--fabm_pelagic--netcdf

ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/getm--fabm_pelagic--netcdf$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

ifneq ($(MOSSCO_GETM),true)
$(error This example only works with MOSSCO_GETM = true)
endif
LDFLAGS += $(MOSSCO_F90LDFLAGS) $(LIBRARY_PATHS)
LDFLAGS += -lmossco_connector
LDFLAGS += -lmossco_netcdf
LDFLAGS += -lmossco_getm $(GETM_LDFLAGS)
LDFLAGS += -lmossco_fabmpelagic -lutil -lsolver $(FABM_LDFLAGS)
LDFLAGS += $(LIBS) -lmossco_util -lesmf $(ESMF_NETCDF_LIBS)

.PHONY: all exec $(TARGET)

all: exec

exec: libmossco_util  libmossco_connector libmossco_netcdf libmossco_getm libmossco_fabmpelagic libmossco_util libsolver $(TARGET)

$(TARGET): toplevel_component.o ../common/main.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)
	@echo "Created example $(PWD)/$@"

# Other subsidiary targets that might not be needed, these should evetually
# end up in some global Rules.make

libmossco_gotmfabm libmossco_gotm libmossco_fabmgotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libsediment libconstant libmossco_clm libmossco_erosed libmossco_fabm0d libmossco_fabmpelagic:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libempty libmossco_getm libmossco_simplewave libmossco_netcdf libmossco_benthos:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_info libmossco_test:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_sediment libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

libsurfacescoupler libaocoupler libgridcoupler libregridcoupler libcopycoupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators $@

libmossco_connector:
	$(MAKE) -C $(MOSSCO_DIR)/src/connectors $@

libremtc:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/remtc $@

libpelagicbenthiccoupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators pelagicbenthiccoupler benthicpelagiccoupler

atmos.nc:
	@-ln -s /media/data/forcing/CLM/cDII.00.kss.2003.nc $@ || 	ln -s /h/ksedata02/data/model/CLM/cDII.00.kss.2003.nc $@ || 	echo "Could not find data file cDII.00.kss.2003.nc."

clean: extraclean
extraclean:
	@-rm -f $(TARGET)

