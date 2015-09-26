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
export MOSSCO_DIR=$(subst /examples/ocean_fabm0d$,,$(CURDIR))
endif

OCEAN?=GOTM

include $(MOSSCO_DIR)/src/Rules.make

LDFLAGS += -L$(MOSSCO_LIBRARY_PATH)
ifeq ($(OCEAN),GOTM)
CPPFLAGS += -D_GOTMOCEAN_
LDFLAGS += -lmossco_gotm -lmossco_fabm0d -lsolver -lmossco_util
LDFLAGS += $(GOTM_LDFLAGS)
dep := libmossco_gotm
else
LDFLAGS += -lremtc -lmossco_fabm0d -lsolver -lmossco_util
LDFLAGS += -L$(GOTM_LIBRARY_PATH) -lairsea -linput -lutil
dep := libremtc
endif
LDFLAGS += $(FABM_LDFLAGS)
LDFLAGS += $(ESMF_F90LDFLAGS)
ifeq ($(HAVE_LD_FORCE_LOAD),true)
#LDFLAGS += -Wl,-force_load
endif


.PHONY: all exec ocean_fabm0d libremtc libsediment liboceansedimentcoupler libmossco_gotm

all: exec
exec: libmossco_util libmossco_fabm0d $(dep) ocean_fabm0d

ocean_fabm0d: toplevel_component.o ../common/main.o
	$(F90) $(F90FLAGS)  $^ $(LDFLAGS) -o $@
	@echo "Created example binary $(CURDIR)/$@"

libremtc:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/remtc $@

libmossco_gotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_fabm0d:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

clean: extraclean

extraclean:
	@rm -f ocean_fabm0d
	@rm -f output_0d.dat main.F90
