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
export MOSSCO_DIR=$(subst /src/components$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

TARGETS=
ifeq ($(MOSSCO_ESMF),true)
TARGETS += libocean libatmosphere
ifeq ($(MOSSCO_EROSED),true)
TARGETS += liberosed
endif
endif

ifeq ($(MOSSCO_FABM),true)
TARGETS +=  libsolver libsediment

ifeq ($(MOSSCO_GOTM),true)
TARGETS += libgotm libfabm0d
endif
endif

.PHONY: $(TARGETS)

#--------------------------------------
# Make targets
# add checks for libfabm and libsediment

all: $(TARGETS)

libfabm_mossco:  prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a
libsediment: prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a
liberosed: prefix $(MOSSCO_LIBRARY_PATH)/liberosed.a
libsolver: prefix $(MOSSCO_module_path)/solver_library.mod
libgotm: prefix $(MOSSCO_LIBRARY_PATH)/libgotm.a
libfabm0d: prefix $(MOSSCO_LIBRARY_PATH)/libfabm0d.a
libocean: prefix $(MOSSCO_LIBRARY_PATH)/libocean.a
libatmosphere: prefix $(MOSSCO_LIBRARY_PATH)/libatmosphere.a

$(MOSSCO_LIBRARY_PATH)/liberosed.a: erosed_component.o
	$(AR) cruvs $@ erosed_component.o

$(MOSSCO_LIBRARY_PATH)/libsediment.a: fabm_sediment_component.o
	$(AR) cruvs $@ fabm_sediment_component.o

$(MOSSCO_LIBRARY_PATH)/libgotm.a: libgotm_external gotm_component.o
	$(AR) cruvs $@ gotm_component.o

$(MOSSCO_LIBRARY_PATH)/libfabm0d.a: fabm_0d_component.o
	$(AR) cruvs $@ fabm_0d_component.o

$(MOSSCO_LIBRARY_PATH)/libocean.a: remtc_ocean.o
	$(AR) cruvs $@ remtc_ocean.o

$(MOSSCO_LIBRARY_PATH)/libatmosphere.a: remtc_atmosphere.o
	$(AR) cruvs $@ remtc_atmosphere.o

fabm_0d_component.o: fabm_0d_component.F90
	@echo "Compiling $<"
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_fabm0d
	$(F90) $(CPPFLAGS) -I$(GOTMDIR)/modules/$(FORTRAN_COMPILER) -I$(FABMDIR)/modules/mossco/$(FORTRAN_COMPILER) $(F90FLAGS) -c $< -o $@

fabm_sediment_component.o: fabm_sediment_component.F90
	@echo "Compiling $<"
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libsediment
	$(F90) $(CPPFLAGS) -I$(FABMDIR)/modules/mossco/$(FORTRAN_COMPILER) $(F90FLAGS) -c $< -o $@

erosed_component.o: erosed_component.F90
	@echo "Compiling $<"
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_erosed
	$(F90) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

$(MOSSCO_module_path)/solver_library.mod:
	make -C $(MOSSCO_DIR)/src/utilities libsolver

# KK-TODO: this can be removed when gotm's timestep routine is used
ifeq ($(MOSSCO_GOTM_FABM),true)
gotm_component.o: gotm_component.F90
	$(F90) $(CPPFLAGS) $(F90FLAGS) -D_FABM_ -c $< -o $@
endif

clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libsediment.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libgotm.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libocean.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libatmosphere.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/liberosed.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libfabm0d.a
	@rm -f $(MOSSCO_MODULE_PATH)/fabm_sediment_component.mod
	@rm -f $(MOSSCO_MODULE_PATH)/fabm_0d_component.mod
