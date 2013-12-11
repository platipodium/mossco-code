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

SUBDIRS=gotm remtc

TARGETS=libempty libconstant
ifeq ($(MOSSCO_EROSED),true)
TARGETS += liberosed
endif

ifeq ($(MOSSCO_FABM),true)
TARGETS +=  libsolver libsediment

ifeq ($(MOSSCO_GOTM),true)
TARGETS += libmossco_fabm0d
endif
endif

ifeq ($(MOSSCO_GETM),true)
TARGETS += libgetm
endif

.PHONY: $(TARGETS) subdirs

#--------------------------------------
# Make targets
# add checks for libfabm and libsediment

all: subdirs $(TARGETS)

libgetm: prefix libgetm_external \
	$(MOSSCO_LIBRARY_PATH)/libgetm.a(getm_component.o)

libfabm_mossco:  prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a
libsediment: prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a $(MOSSCO_MODULE_PATH)/fabm_sediment_component.mod
liberosed: prefix $(MOSSCO_LIBRARY_PATH)/liberosed.a
libsolver: prefix $(MOSSCO_module_path)/solver_library.mod
libgotm: prefix $(MOSSCO_LIBRARY_PATH)/libgotm.a
libmossco_fabm0d: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a
libempty: prefix $(MOSSCO_LIBRARY_PATH)/libempty.a
libconstant: prefix $(MOSSCO_LIBRARY_PATH)/libconstant.a
libgotm_fabm: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_gotmfabm.a

$(MOSSCO_LIBRARY_PATH)/liberosed.a: erosed_component.o
	$(AR) cruvs $@ erosed_component.o

$(MOSSCO_LIBRARY_PATH)/libsediment.a: fabm_sediment_component.o
	$(AR) cruvs $@ fabm_sediment_component.o
$(MOSSCO_MODULE_PATH)/fabm_sediment_component.mod: fabm_sediment_component.F90

$(MOSSCO_LIBRARY_PATH)/libempty.a: empty_component.o
	$(AR) cruvs $@ empty_component.o

$(MOSSCO_LIBRARY_PATH)/libconstant.a: constant_component.o
	$(AR) cruvs $@ constant_component.o

$(MOSSCO_LIBRARY_PATH)/libgotm.a: libgotm_external gotm_component.o
	$(AR) cruvs $@ gotm_component.o

$(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a: fabm0d_component.o
	$(AR) cruvs $@ fabm0d_component.o

# A really, really dirty hack for now...
# ...and automatic rm of .o also does not work because of this.
getm_component.o: getm_component.F90
	@echo "Compiling $<"
	$(F90) -I$(GETM_INCLUDE_PATH) -I$(GETM_MODULE_PATH) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

fabm0d_component.o: fabm0d_component.F90
	@echo "Compiling $<"
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_fabm0d
	$(F90) $(CPPFLAGS) -I$(GOTMDIR)/modules/$(FORTRAN_COMPILER) -I$(FABMDIR)/modules/mossco/$(FORTRAN_COMPILER) $(F90FLAGS) -c $< -o $@

fabm_sediment_component.o: fabm_sediment_component.F90
	@echo "Compiling $<"
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_sediment
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
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done
	$(RM) getm_component.o
	@rm -f $(MOSSCO_LIBRARY_PATH)/libsediment.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libgetm.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/liberosed.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libempty.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libconstant.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a
	@rm -f $(MOSSCO_MODULE_PATH)/fabm_sediment_component.mod
	@rm -f $(MOSSCO_MODULE_PATH)/fabm0d_component.mod

subdirs:
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir ; done
