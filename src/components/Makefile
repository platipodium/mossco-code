# This Makefile is part of MOSSCO
#
# @copyright (C) 2021-2025 Helmholtz-Zentrum hereon GmbH
# @copyright (C) 2013-2021 Helmholtz-Zentrum Geesthacht GmbH
# @author Carsten Lemmen <carsten.lemmen@hereon.de>
# @author Richard Hofmeister
# @author Knut Klingbeil <knut.klingbeil@io-warnemuende.de>
# @author Markus Kreus <markus.kreus@hereon.de>
# @author Johannes Bieser <johannes.bieser@hereon.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/components$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

SUBDIRS=gotm remtc

TARGETS=libmossco_technical libmossco_default libmossco_random
TARGETS+=libmossco_aggregation libmossco_grid
TARGETS+=libmossco_simplewave libmossco_info libmossco_macrobenthos

ifeq ($(MOSSCO_CMAQ),true)
  TARGETS += libmossco_cmaq
endif
ifeq ($(MOSSCO_NETCDF),true)
  TARGETS += libmossco_netcdf libmossco_clm #libmossco_particle#libmossco_river
endif
ifeq ($(MOSSCO_OASIS),true)
  TARGETS += libmossco_oasis #libmossco_river
endif
ifeq ($(MOSSCO_EROSED),true)
  TARGETS += libmossco_erosed
  ifeq ($(MOSSCO_BENTHOS),true)
    TARGETS+=libmossco_benthos
  endif
endif
ifeq ($MOSSCO_XIOS),true)
  TARGETS += libmossco_xios
endif

ifeq ($(MOSSCO_FABM),true)
TARGETS +=  libsediment libmossco_fabmpelagic
# @todo reenable once fabm_benthic_driver is vectoized
#TARGETS +=  libmossco_fabmbenthic
ifeq ($(MOSSCO_GOTM),true)
  #TARGETS += libmossco_fabm0d
endif
endif
ifeq ($(MOSSCO_GETM),true)
  TARGETS += libmossco_getm
endif
ifeq ($(MOSSCO_HAMSOM),true)
  TARGETS += libmossco_hamsom
endif
ifeq ($(MOSSCO_SCHISM),true)
  TARGETS += libmossco_schism
endif

.PHONY: $(TARGETS) $(SUBDIRS) subdirs utilities

#--------------------------------------
# Make targets
# add checks for libfabm and libsediment

all: utilities subdirs $(TARGETS)

libmossco_default: libdefault libmossco_util
libmossco_info: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_info.a(info_component.o)

libmossco_benthos: libmossco_benthos_driver libmossco_util \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(benthos_component.o)

libmossco_particle: libmossco_particle_driver libmossco_particle_component libmossco_util
libmossco_particle_driver:  prefix $(MOSSCO_LIBRARY_PATH)/libmossco_particle.a(particle_driver.o)
libmossco_particle_component: libmossco_particle_driver $(MOSSCO_LIBRARY_PATH)/libmossco_particle.a(particle_component.o)

libmossco_hamsom: libmossco_hamsom_driver libmossco_hamsom_component
libmossco_hamsom_driver:  prefix $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a(hamsom_driver.o)
libmossco_hamsom_component: libmossco_hamsom_driver $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a(hamsom_component.o)

libmossco_xios: libmossco_xios $(MOSSCO_LIBRARY_PATH)/libmossco_xios.a(xios_component.o)
libmossco_schism:  libmossco_schism_component
libmossco_schism_component: prefix schism_component.F90 $(MOSSCO_LIBRARY_PATH)/libmossco_schism.a(schism_component.o)

libmossco_cmaq: libmossco_cmaq_driver libmossco_cmaq_component
libmossco_cmaq_driver:  prefix $(MOSSCO_LIBRARY_PATH)/libmossco_cmaq.a(cmaq_driver.o)
libmossco_cmaq_component: libmossco_cmaq_driver $(MOSSCO_LIBRARY_PATH)/libmossco_cmaq.a(cmaq_component.o)

libmossco_clm: libmossco_clm_driver libmossco_clm_component
libmossco_clm_component: libmossco_clm_driver $(MOSSCO_LIBRARY_PATH)/libmossco_clm.a(clm_netcdf_component.o)
libmossco_clm_driver:    prefix $(MOSSCO_LIBRARY_PATH)/libmossco_clm.a(clm_driver.o)

libmossco_oasis: $(MOSSCO_LIBRARY_PATH)/libmossco_oasis.a(oasis_component.o)


$(MOSSCO_LIBRARY_PATH)/libmossco_particle.a(particle_driver.o):
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_particle

$(MOSSCO_LIBRARY_PATH)/libmossco_macrobenthos.a(vertical_macrobenthos_driver.o):
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_macrobenthos

libmossco_macrobenthos: prefix libmossco_util \
  $(MOSSCO_LIBRARY_PATH)/libmossco_macrobenthos.a(vertical_macrobenthos_driver.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_macrobenthos.a(vertical_macrobenthos_component.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_macrobenthos.a(filtration_component.o)

libmossco_simplewave: libmossco_simplewave_driver libmossco_util \
  $(MOSSCO_LIBRARY_PATH)/libmossco_simplewave.a(simplewave_component.o)

libmossco_grid: libmossco_util \
  $(MOSSCO_LIBRARY_PATH)/libmossco_grid.a(grid_component.o)

libmossco_location: libmossco_util \
  $(MOSSCO_LIBRARY_PATH)/libmossco_location.a(location_component.o)

libmossco_river: libmossco_river_driver libmossco_util \
  $(MOSSCO_LIBRARY_PATH)/libmossco_river.a(river_component.o)

libmossco_river_driver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_river

libmossco_netcdf: prefix libmossco_util \
  $(MOSSCO_LIBRARY_PATH)/libmossco_netcdf.a(netcdf_component.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_netcdf.a(netcdf_input_component.o)
libmossco_erosed: erosed_driver erosed_component
.PHONY: erosed_driver
erosed_driver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_erosed
.PHONY: erosed_component
erosed_component: prefix libmossco_util $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a(erosed_component.o)

libmossco_benthos_driver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_benthos

$(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a(hamsom_driver.o):
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_hamsom

$(MOSSCO_LIBRARY_PATH)/libmossco_cmaq.a(cmaq_driver.o):
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_cmaq

$(MOSSCO_LIBRARY_PATH)/libmossco_clm.a(clm_driver.o):
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_clm

libmossco_fabmpelagic_driver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_fabmpelagic

libmossco_fabmbenthic_driver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_fabmbenthic

# Why these next two lines???
#libfabm_mossco:  prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a
#libgotm_fabm: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_gotmfabm.a
libsediment: prefix libmossco_sediment_driver \
	$(MOSSCO_LIBRARY_PATH)/libsediment.a(fabm_sediment_component.o)

libmossco_fabmpelagic: prefix libmossco_fabmpelagic_driver \
	$(MOSSCO_LIBRARY_PATH)/libmossco_fabmpelagic.a(fabm_pelagic_component.o)

libmossco_fabmbenthic: prefix libmossco_fabmbenthic_driver \
	$(MOSSCO_LIBRARY_PATH)/libmossco_fabmbenthic.a(fabm_benthic_component.o)

libmossco_fabm0d: prefix libmossco_fabm0d_driver \
	$(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a(fabm0d_component.o)

libmossco_getm: libmossco_getm_driver libmossco_util \
	$(MOSSCO_LIBRARY_PATH)/libmossco_getm.a(getm_component.o)

#libmossco_technical: prefix libmossco_util test_component.F90 \
libmossco_technical: prefix libmossco_util \
	$(MOSSCO_LIBRARY_PATH)/libmossco_technical.a(empty_component.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_technical.a(dummy_component.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_technical.a(inout_component.o)

#	$(MOSSCO_LIBRARY_PATH)/libmossco_technical.a(test_component.o) \
#	
#	
libmossco_aggregation: prefix libmossco_util  \
		$(MOSSCO_LIBRARY_PATH)/libmossco_aggregation.a(time_aggregation_component.o)

libdefault: prefix modtypes $(MOSSCO_LIBRARY_PATH)/libdefault.a(default_component.o)
modtypes: prefix $(MOSSCO_MODULE_PATH)/mossco_variable_types.mod

$(MOSSCO_MODULE_PATH)/fabm_sediment_component.mod: fabm_sediment_component.F90

libmossco_simplewave_driver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_simplewave

libmossco_fabm0d_driver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_fabm0d

libmossco_sediment_driver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_sediment

libmossco_getm_driver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_getm

getm_component.o: getm_component.F90
	@echo "Compiling $<"
	$(F90) $(GETM_CPPFLAGS) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

fabm0d_component.o: fabm0d_component.F90
	@echo "Compiling $<"
	$(F90) $(GOTM_CPPFLAGS) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

fabm_sediment_component.o: fabm_sediment_component.F90
	@echo "Compiling $<"
	$(F90) $(FABM_CPPFLAGS) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

fabm_pelagic_component.o: fabm_pelagic_component.F90
	@echo "Compiling $<"
	$(F90) $(FABM_CPPFLAGS) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

fabm_benthic_component.o: fabm_benthic_component.F90
	@echo "Compiling $<"
	$(F90) $(FABM_CPPFLAGS) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

hamsom_component.o: hamsom_component.F90
	@echo "Compiling $<"
	$(F90) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@
#	$(F90) $(HAMSOM_CPPFLAGS) $(CPPFLAGS) $(F90FLAGS) $(HAMSOM_FFLAGS) -c $< -o $@

xios_component.o: xios_component.F90
cmaq_component.o: cmaq_component.F90

	@echo "Compiling $<"
	$(F90) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

hamsom_component.F90: dummy_component.F90
	@echo "Creating dummy HAMSOM component $@"
	sed 's/dummy/hamsom/g' $< > $@

schism_component.F90: dummy_component.F90
	@echo "Creating dummy SCHISM component $@"
	sed 's/dummy/schism/g' $< > $@

tracer_component.F90: dummy_component.F90
	@echo "Creating dummy TRACER component $@"
	sed 's/dummy/tracer/g' $< > $@

test_component.F90: $(MOSSCO_DIR)/src/config/test_component.yaml $(MOSSCO_DIR)/scripts/create_component.py
	@echo "Creating $@"
	python $(MOSSCO_DIR)/scripts/create_component.py $(MOSSCO_DIR)/src/config/test_component.yaml

calculator_component.F90: $(MOSSCO_DIR)/src/mediators/calculator.F90
	@-echo "Creating $@"
	@-sed 's/cplComp/gridComp/g;s/CplComp/GridComp/g' $< > $@

# KK-TODO: this can be removed when gotm's timestep routine is used
ifeq ($(MOSSCO_GOTM_FABM),true)
gotm_component.o: gotm_component.F90
	$(F90) $(CPPFLAGS) $(F90FLAGS) -D_FABM_ -c $< -o $@
endif

$(MOSSCO_MODULE_PATH)/mossco_variable_types.mod:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities libmossco_util

utilities:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities

libmossco_util libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

clean: extraclean
	$(RM) *.o

extraclean:
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done
	@-rm -f getm_component.o
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libsediment.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libgetm.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libempty.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_aggregation.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_technical.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libdefault.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_simplewave.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_cmaq.a
	@-rm -f $(MOSSCO_MODULE_PATH)/fabm_sediment_component.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/fabm0d_component.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/simplewave_component.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/hamsom_component.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/cmaq_component.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/tracer_component.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/schism_component.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/calculator_component.mod

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

ifneq ($(wildcard *.make),)
  include *.make
endif
ifneq ($(wildcard *.mk),)
  include *mk
endif
