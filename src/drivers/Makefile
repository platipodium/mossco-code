# This Makefile is part of MOSSCO
# 
# Copyright (C) 2013 Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#                Richard Hofmeister, Helmholtz-Zentrum Geesthacht    
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/drivers$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

DELFT_FOREIGN_SOURCES=$(DELFT_DIR)/modules/precision_basics.f90 $(DELFT_DIR)/modules/precision.f90 \
 $(DELFT_DIR)/source/erosand.f90 \
 $(DELFT_DIR)/source/eromud.f90 \
 $(DELFT_DIR)/source/vanRijn84.f90 \
 $(DELFT_DIR)/source/shld.f90 \
 $(DELFT_DIR)/source/sand_mud.f90 \
 $(DELFT_DIR)/include/sedparams.inc

DELFT_OBJS    = precision_basics.o precision.o erosand.o eromud.o vanRijn84.o shld.o sand_mud.o 

.PHONY: libsediment liberosed libdelft localsources

# Make targets

ALL_TARGETS =

ifeq ($(MOSSCO_FABM),true)
ALL_TARGETS=libsediment libmossco_fabm0d
endif

ifeq ($(MOSSCO_DELFT),true)
ALL_TARGETS += localsources liberosed
endif

all: $(ALL_TARGETS)


# FABM/sediment section

libsediment: prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a
libmossco_fabm0d: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a

$(MOSSCO_LIBRARY_PATH)/libsediment.a: libsolver libfabm_external fabm_sediment_driver.o
	$(AR) crus $@ fabm_sediment_driver.o

$(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a: libsolver libfabm_external libgotm_external fabm_0d_driver.o
	        $(AR) crus $@ fabm_0d_driver.o
	
libsolver: $(MOSSCO_LIBRARY_PATH)/libsolver.a

$(MOSSCO_LIBRARY_PATH)/libsolver.a:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities libsolver

# Delft section
localsources: $(DELFT_FOREIGN_SOURCES)
	for F in $(DELFT_FOREIGN_SOURCES) ; do ln -sf $$F; done

libdelft:  prefix $(MOSSCO_LIBRARY_PATH)/libmossco_delft.a
liberosed: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a

$(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a: libdelft delft_erosed_driver.o $(DELFT_OBJS)
	$(AR) crus $@ delft_erosed_driver.o $(DELFT_OBJS)

$(MOSSCO_LIBRARY_PATH)/libmossco_delft.a: $(DELFT_OBJS)
	$(AR) crus $(MOSSCO_LIBRARY_PATH)/libmossco_delft.a $(DELFT_OBJS) 

# Generic section


clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libsediment.a
	@rm -f $(MOSSCO_MODULE_PATH)/fabm_sediment_driver.mod
	@rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a
	@rm -f $(MOSSCO_MODULE_PATH)/fabm_0d_driver.mod
	@rm -f $(MOSSCO_MODULE_PATH)/delft_erosed_driver.mod
