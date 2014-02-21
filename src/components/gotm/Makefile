# This Makefile is part of MOSSCO
# 
# @copyright (C) 2013, 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
# @author Richard Hofmeister, Helmholtz-Zentrum Geesthacht    
# @author Knut Klingbeil, Institut für Ostseeforschung Warnemünde  
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/components/gotm$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifeq ($(MOSSCO_GOTM),true)
TARGETS += libgotm
ifeq ($(MOSSCO_FABM),true)
TARGETS += libmossco_gotmfabm
endif
endif

.PHONY: $(TARGETS) libgotm_external libtypes libmossco_gotmfabm libgotm

#--------------------------------------
# Make targets
# add checks for libfabm and libsediment

all: $(TARGETS)

libgotm: prefix libgotm_external libtypes $(MOSSCO_LIBRARY_PATH)/libgotm.a(gotm_component.o)
libmossco_gotmfabm: prefix libtypes gotmfabm_drv				\
	$(MOSSCO_LIBRARY_PATH)/libmossco_gotmfabm.a(fabm_gotm_component.o)
gotmfabm_drv:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_gotmfabm
libtypes: prefix $(MOSSCO_MODULE_PATH)/mossco_variable_types.mod


$(MOSSCO_MODULE_PATH)/mossco_variable_types.mod:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities libtypes

clean: extraclean
extraclean:
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libgotm.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_gotmfabm.a
