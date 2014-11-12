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
export MOSSCO_DIR=$(subst /examples/ocean_fabm0d_fabmsed$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

NC_LIBS += $(shell nf-config --flibs)
LIBS := -lsediment -lremtc -loceansedimentcoupler -lmossco_fabm0d -lmossco_util -lmossco_sediment -lsolver $(LIBS)
LDFLAGS += $(MOSSCO_LDFLAGS) $(LIBRARY_PATHS) $(LIBS)
LDFLAGS += -L$(GOTM_LIBRARY_PATH) -lairsea_prod -linput_prod -lutil_prod
LDFLAGS += $(FABM_LDFLAGS)
#ifeq ($(HAVE_LD_FORCE_LOAD),true)
#LDFLAGS += -Wl,-force_load
#endif

.PHONY: all exec libremtc libsediment liboceansedimentcoupler libmossco_util

all: exec
exec: libmossco_util libmossco_fabm0d libsediment libremtc liboceansedimentcoupler ocean_fabm0d_fabmsed

ocean_fabm0d_fabmsed: toplevel_component.o ../common/main.o
	$(F90) $(F90FLAGS)  $^ $(LDFLAGS) -o $@
	@echo "Created example binary $(PWD)/$@"

libremtc:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/remtc $@

libmossco_fabm0d libsediment:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

liboceansedimentcoupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

clean: extraclean

extraclean:
	@rm -f ocean_fabm0d_fabmsed
