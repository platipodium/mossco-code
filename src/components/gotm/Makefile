# This Makefile is part of MOSSCO
# 
# Copyright (C) 2013             Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#                            Richard Hofmeister, Helmholtz-Zentrum Geesthacht  
#                     Knut Klingbeil, Institut für Ostseeforschung Warnemünde  
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

.PHONY: $(TARGETS)

#--------------------------------------
# Make targets
# add checks for libfabm and libsediment

all: $(TARGETS)

libgotm: prefix $(MOSSCO_LIBRARY_PATH)/libgotm.a
libmosscogotm_fabm: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_gotmfabm.a

$(MOSSCO_LIBRARY_PATH)/libgotm.a: libgotm_external gotm_component.o
	$(AR) cruvs $@ gotm_component.o

# KK-TODO: this can be removed when gotm's timestep routine is used
ifeq ($(MOSSCO_GOTM_FABM),true)
gotm_component.o: gotm_component.F90 libmossco_gotmfabm
	$(F90) $(CPPFLAGS) $(F90FLAGS) -D_FABM_ -c $< -o $@
endif

$(MOSSCO_LIBRARY_PATH)/libmossco_gotmfabm.a:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_gotmfabm

clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libgotm.a
