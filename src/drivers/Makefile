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

DELFT_OBJS= \
	$(DELFT_DIR)/modules/precision_basics.o	\
	$(DELFT_DIR)/modules/precision.o	\
	$(DELFT_DIR)/source/erosand.o		\
	$(DELFT_DIR)/source/eromud.o		\
	$(DELFT_DIR)/source/vanRijn84.o		\
	$(DELFT_DIR)/source/shld.o		\
	$(DELFT_DIR)/source/sand_mud.o

.PHONY: libsediment liberosed libdelft

# Make targets

ALL_TARGETS =

ifeq ($(MOSSCO_FABM),true)
ALL_TARGETS=libsediment libmossco_fabm0d
endif

ifeq ($(MOSSCO_DELFT),true)
ALL_TARGETS += localsources liberosed
INCLUDES += -I$(DELFT_DIR)/include
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
liberosed: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a

$(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a: $(DELFT_OBJS) delft_erosed_driver.o 
	$(AR) crus $@ $^

# Generic section


clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libsediment.a
	@rm -f $(MOSSCO_MODULE_PATH)/fabm_sediment_driver.mod
	@rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a
	@rm -f $(MOSSCO_MODULE_PATH)/fabm_0d_driver.mod
	@rm -f $(MOSSCO_MODULE_PATH)/delft_erosed_driver.mod
