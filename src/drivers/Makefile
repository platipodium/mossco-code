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
export MOSSCO_DIR=$(subst /src/drivers$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

.PHONY: libmossco_sediment libmossco_erosed libmossco_clm

# Make targets

ALL_TARGETS =

ifeq ($(MOSSCO_FABM),true)
ALL_TARGETS=libmossco_sediment libmossco_fabm0d
ifeq ($(MOSSCO_GOTM),true)
ALL_TARGETS+=libmossco_gotmfabm
endif
endif

ifeq ($(MOSSCO_EROSED),true)
ALL_TARGETS += libmossco_erosed
INCLUDES += -I$(EROSED_DIR)/include
endif

ifeq ($(MOSSCO_CLM),true)
ALL_TARGETS += libmossco_clm
endif

all: $(ALL_TARGETS)


# FABM/sediment section

libmossco_sediment: prefix libsolver $(MOSSCO_LIBRARY_PATH)/libmossco_sediment.a(fabm_sediment_driver.o)
libmossco_fabm0d: prefix libsolver libfabm_external libgotm_external $(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a(fabm0d_driver.o)
libmossco_gotmfabm: prefix libsolver libfabm_external libgotm_external $(MOSSCO_LIBRARY_PATH)/libmossco_gotmfabm.a(fabm_gotm_driver.o)
libmossco_clm: $(MOSSCO_LIBRARY_PATH)/libmossco_clm.a(clm_driver.o)

libsolver: $(MOSSCO_LIBRARY_PATH)/libsolver.a

$(MOSSCO_LIBRARY_PATH)/libsolver.a:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities libsolver

# EROSED section
EROSED_OBJS= \
	$(EROSED_DIR)/modules/precision_basics.o	\
	$(EROSED_DIR)/modules/precision.o		\
	$(EROSED_DIR)/source/erosand.o			\
	$(EROSED_DIR)/source/eromud.o			\
	$(EROSED_DIR)/source/vanRijn84.o		\
	$(EROSED_DIR)/source/shld.o			\
	$(EROSED_DIR)/source/sand_mud.o

libmossco_erosed: prefix						\
	$(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_OBJS))	\
	$(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a(delft_erosed_driver.o)

# Generic section


clean: extraclean
extraclean:
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_sediment.a
	@-rm -f $(MOSSCO_MODULE_PATH)/fabm_sediment_driver.mod
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a
	@-rm -f $(MOSSCO_MODULE_PATH)/fabm0d_driver.mod
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_gotmfabm.a
	@-rm -f $(MOSSCO_MODULE_PATH)/fabm_gotm_driver.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/delft_erosed_driver.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/clm_driver.mod
