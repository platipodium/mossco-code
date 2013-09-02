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
endif

ifeq ($(MOSSCO_FABM),true)
TARGETS +=  libsolver libsediment

ifeq ($(MOSSCO_GOTM),true)
TARGETS += libgotm libfabm0d
endif
endif

#--------------------------------------
# Make targets
# add checks for libfabm and libsediment


all: $(TARGETS)

libfabm_mossco:  prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a
libsediment: prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a
libsolver: prefix $(MOSSCO_module_path)/solver_library.mod
libgotm: prefix $(MOSSCO_LIBRARY_PATH)/libgotm.a
libfabm0d: prefix $(MOSSCO_LIBRARY_PATH)/libfabm0d.a
libocean: prefix $(MOSSCO_LIBRARY_PATH)/libocean.a
libatmosphere: prefix $(MOSSCO_LIBRARY_PATH)/libatmosphere.a

$(MOSSCO_LIBRARY_PATH)/libsediment.a: esmf_fabm_sediment_component.o
	$(AR) cruvs $@ esmf_fabm_sediment_component.o

$(MOSSCO_LIBRARY_PATH)/libgotm.a: libgotm_external  esmf_gotm_component.o 
	$(AR) cruvs $@ esmf_gotm_component.o

$(MOSSCO_LIBRARY_PATH)/libfabm0d.a: esmf_fabm_0d_component.o
	$(AR) cruvs $@ esmf_fabm_0d_component.o

$(MOSSCO_LIBRARY_PATH)/libocean.a: remtc_ocean.o
	$(AR) cruvs $@ remtc_ocean.o

$(MOSSCO_LIBRARY_PATH)/libatmosphere.a: remtc_atmosphere.o
	$(AR) cruvs $@ remtc_atmosphere.o

esmf_gotm_component.o: esmf_gotm_component.F90
	@echo "Compiling $<"
	$(F90) $(CPPFLAGS) -I$(GOTMDIR)/modules/$(FORTRAN_COMPILER) $(F90FLAGS) -c $< -o $@

esmf_fabm_0d_component.o: esmf_fabm_0d_component.F90
	@echo "Compiling $<"
	make -C $(FABMDIR)/src 0d
	make -C $(FABMDIR)/src/drivers/0d fabm0d.o
	$(F90) $(CPPFLAGS) -I$(GOTMDIR)/modules/$(FORTRAN_COMPILER) -I$(FABMDIR)/modules/0d/$(FORTRAN_COMPILER) $(F90FLAGS) -c $< -o $@

$(MOSSCO_module_path)/solver_library.mod:
	make -C $(MOSSCO_DIR)/src/utilities libsolver

clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libsediment.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libgotm.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libocean.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libatmosphere.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libfabm0d.a
	@rm -f $(MOSSCO_MODULE_PATH)/esmf_fabm_sediment_component.mod
	@rm -f $(MOSSCO_MODULE_PATH)/esmf_fabm_0d_component.mod

# External libraries
libgotm_external: $(GOTMDIR)/lib/$(FORTRAN_COMPILER)/libgotm_prod.a
$(GOTMDIR)/lib/$(FORTRAN_COMPILER)/libgotm_prod.a:
	$(MAKE) -C $(FABMDIR)/src gotm
	$(MAKE) -C $(GOTMDIR)/src

