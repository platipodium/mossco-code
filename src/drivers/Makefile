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

TARGETS = libmossco_benthos
ifeq ($(MOSSCO_NETCDF),true)
ifeq ($(MOSSCO_MPI),true)
TARGETS+=libmossco_clm
endif
endif

ifeq ($(MOSSCO_FABM),true)
TARGETS+=libmossco_sediment libmossco_fabm0d
ifeq ($(MOSSCO_GOTM),true)
ifeq ($(MOSSCO_NETCDF),true)
TARGETS+=libmossco_gotmfabm
endif
endif
endif

ifeq ($MOSSCO_GETM),true)
TARGETS+=libmossco_getm_driver
endif

ifeq ($(MOSSCO_EROSED),true)
TARGETS += libmossco_erosed
INCLUDES += -I$(EROSED_DIR)/include 
endif

.PHONY: $(TARGETS)

# Make targets
# Default is to make all of the targets
all: $(TARGETS)

# FABM/sediment section

libmossco_sediment: prefix libmossco_util libsolver libfabm_external $(MOSSCO_LIBRARY_PATH)/libmossco_sediment.a(fabm_sediment_driver.o)
libmossco_fabm0d: prefix libsolver libfabm_external libgotm_external $(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a(fabm0d_driver.o)
libmossco_gotmfabm: prefix libmossco_util libsolver libfabm_external libgotm_external $(MOSSCO_LIBRARY_PATH)/libmossco_gotmfabm.a(fabm_gotm_driver.o)
libmossco_clm: prefix libmossco_clm_driver
libmossco_clm_driver: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_clm.a(clm_driver.o)

libmossco_getm_driver: prefix libgetm_external $(MOSSCO_LIBRARY_PATH)/libmossco_getm.a(getm_driver.o)
# A really, really dirty hack for now...
# ...and automatic rm of .o also does not work because of this.
getm_driver.o: getm_driver.F90
	@echo "Compiling $<"
	$(F90) -I$(GETM_INCLUDE_PATH) -I$(GETM_MODULE_PATH) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

# Benthos section
libmossco_benthos: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(benthoseffect_class.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(biotypes.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(microphytobenthos_class.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(macrofauna_class.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(mbalthica_class.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(macrofauna_interface.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(criticalsheareffect.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(erodibilityeffect.o)

microphytobenthos_class.o: criticalsheareffect.o benthoseffect_class.o biotypes.o erodibilityeffect.o
mbalthica_class.o: macrofauna_class.o
macrofauna_class.o: benthoseffect_class.o biotypes.o
benthoseffect_class.o: biotypes.o
macrofauna_interface.o: mbalthica_class.o biotypes.o
criticalsheareffect.o: macrofauna_class.o biotypes.o
erodibilityeffect.o: macrofauna_class.o biotypes.o

# utilities
libmossco_util libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

# EROSED section
EROSED_OBJS= \
	$(EROSED_DIR)/modules/precision_basics.o	\
	$(EROSED_DIR)/modules/precision.o		\
	$(EROSED_DIR)/source/erosand.o			\
	$(EROSED_DIR)/source/eromud.o			\
	$(EROSED_DIR)/source/vanRijn84.o		\
	$(EROSED_DIR)/source/shld.o			\
	$(EROSED_DIR)/source/sand_mud.o

libmossco_erosed_dependencies: prefix							\
  $(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(biotypes.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/modules/precision_basics.o)	\
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/modules/precision.o)		\
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/source/erosand.o)		\
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/source/eromud.o)		\
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/source/vanRijn84.o)		\
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/source/shld.o)		\
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/source/sand_mud.o)

libmossco_erosed: prefix biotypes.o libmossco_erosed_dependencies 			\
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a(erosed_driver.o)

# Generic section


clean: extraclean
extraclean:
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_sediment.a
	@-rm -f $(MOSSCO_MODULE_PATH)/fabm_sediment_driver.mod
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a
	@-rm -f $(MOSSCO_MODULE_PATH)/fabm0d_driver.mod
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_gotmfabm.a
	@-rm -f $(MOSSCO_MODULE_PATH)/fabm_gotm_driver.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/erosed_driver.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/clm_driver.mod
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a
	$(RM) $(EROSED_OBJS)
