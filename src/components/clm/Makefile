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
export MOSSCO_DIR=$(subst /src/components/clm$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifeq ($(MOSSCO_CLM),true)
TARGETS += libmossco_clm
endif

CLM_DATADIR ?= /h/ksedata02/data/model/CLM/

.PHONY: $(TARGETS) 

#--------------------------------------------------------
# Make targets

all: $(TARGETS)

libmossco_clm: prefix data $(MOSSCO_LIBRARY_PATH)/libmossco_clm.a(clm_netcdf_component.o)

$(MOSSCO_LIBRARY_PATH)/libmossco_clm.a:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_clm

data: atmos.nc

atmos.nc: $(CLM_DATADIR)/cDII.00.kss.2003.nc
	ln -s $< $@


clean: extraclean
extraclean:
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_clm.a
