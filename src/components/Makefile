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
export MOSSCO_DIR=$(subst /src/components$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

SUBDIRS=gotm remtc

TARGETS=libempty libconstant libmossco_clm libmossco_benthos libmossco_simplewave

ifeq ($(MOSSCO_EROSED),true)
TARGETS += liberosed
endif

ifeq ($(MOSSCO_FABM),true)
TARGETS +=  libsediment

ifeq ($(MOSSCO_GOTM),true)
TARGETS += libmossco_fabm0d
endif
endif

ifeq ($(MOSSCO_GETM),true)
TARGETS += libgetm
endif

.PHONY: $(TARGETS) subdirs utilities

#--------------------------------------
# Make targets
# add checks for libfabm and libsediment

all: utilities subdirs $(TARGETS)

libmossco_clm: libmossco_clm_component libmossco_clm_driver
libmossco_benthos: libmossco_benthos_component libmossco_benthos_drivers
libmossco_clm_component: libmossco_clm_driver $(MOSSCO_LIBRARY_PATH)/libmossco_clm.a(clm_netcdf_component.o)
libmossco_clm_driver:    prefix $(MOSSCO_LIBRARY_PATH)/libmossco_clm.a(clm_driver.o)

libmossco_benthos_drivers: prefix  $(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(microphytobenthos_class.o)
libmossco_benthos_component: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(benthos_component.o)
libmossco_simplewave: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_simplewave.a(simplewave_component.o)
libmossco_erosed: liberosed

$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(microphytobenthos_class.o):
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_benthos

$(MOSSCO_LIBRARY_PATH)/libmossco_clm.a(clm_driver.o):
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_clm

libgetm: prefix libgetm_external \
	$(MOSSCO_LIBRARY_PATH)/libgetm.a(getm_component.o)

# Why these next two lines???
#libfabm_mossco:  prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a
#libgotm_fabm: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_gotmfabm.a
libsediment: prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a(fabm_sediment_component.o) $(MOSSCO_MODULE_PATH)/fabm_sediment_component.mod
liberosed: prefix $(MOSSCO_LIBRARY_PATH)/liberosed.a(erosed_component.o)
libmossco_fabm0d: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a(fabm0d_component.o)
libempty: prefix $(MOSSCO_LIBRARY_PATH)/libempty.a(empty_component.o)
libconstant: prefix modtypes $(MOSSCO_LIBRARY_PATH)/libconstant.a(constant_component.o)
modtypes: prefix $(MOSSCO_MODULE_PATH)/mossco_variable_types.mod

$(MOSSCO_MODULE_PATH)/fabm_sediment_component.mod: fabm_sediment_component.F90

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

# KK-TODO: this can be removed when gotm's timestep routine is used
ifeq ($(MOSSCO_GOTM_FABM),true)
gotm_component.o: gotm_component.F90
	$(F90) $(CPPFLAGS) $(F90FLAGS) -D_FABM_ -c $< -o $@
endif

$(MOSSCO_MODULE_PATH)/mossco_variable_types.mod:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities libtypes

utilities:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities

clean: extraclean

extraclean:
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done
	@-rm -f getm_component.o
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libsediment.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libgetm.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/liberosed.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libempty.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libconstant.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_simplewave.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a
	@-rm -f $(MOSSCO_MODULE_PATH)/fabm_sediment_component.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/fabm0d_component.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/simplewave_component.mod

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

