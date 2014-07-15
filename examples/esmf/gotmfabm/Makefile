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
export MOSSCO_DIR=$(subst /examples/esmf/gotmfabm$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifneq ($(MOSSCO_GOTM_FABM),true)
$(error This example only works with MOSSCO_GOTM_FABM=true)
endif

LDFLAGS += -L$(MOSSCO_LIBRARY_PATH) -lmossco_gotmfabm -lmossco_fabmgotm -lgotm -lsolver -lmossco_util
LDFLAGS += $(GOTM_LDFLAGS)
LDFLAGS += -L$(FABM_LIBRARY_PATH) -lfabm_prod
LDFLAGS += $(ESMF_LDFLAGS)

.PHONY: all exec gotmfabm libmossco_gotmfabm libgotm

all: exec
exec: libmossco_util libsolver libmossco_gotmfabm libgotm gotmfabm

gotmfabm: toplevel_component.o ../../common/main.o  
	$(F90) $^ $(F90FLAGS) $(LDFLAGS) -o $@
	@echo "Created example binary $(PWD)/$@"

libmossco_gotmfabm libgotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_util libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

clean: extraclean

extraclean: 
	@-rm -f gotmfabm main.F90
