# This Makefile is part of MOSSCO
#
# @copyright (C) 2013, 2014, 2015 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/mediators$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

TARGETS=
ifeq ($(MOSSCO_ESMF),true)
TARGETS += libremtc  libxgridcoupler libredistcoupler
TARGETS += libcopycoupler libregridcoupler libinoutcoupler
ifeq ($(MOSSCO_FABM),true)
TARGETS += liboceansedimentcoupler
TARGETS += pelagicbenthiccoupler
TARGETS += pelagicsoilconnector soilpelagicconnector
TARGETS += libmossco_mediator
TARGETS += benthicpelagiccoupler
endif
ifeq ($(MOSSCO_SQLITE),true)
TARGETS += soilpelagicmediator
endif
ifeq ($(MOSSCO_GOTM),true)
TARGETS += libverticalmeancoupler
endif
endif

SUBDIRS=

.PHONY: default all subdirs $(TARGETS)

all: subdirs $(TARGETS)

ifeq ($(MOSSCO_SQLITE),true)
libmossco_mediator: pelagicsoilconnector soilpelagicconnector soilpelagicmediator
else
libmossco_mediator: pelagicsoilconnector soilpelagicconnector
endif

libpelagicbenthic_mediator: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_mediator.a(pelagic_benthic_mediator.o)

libinoutcoupler: prefix $(MOSSCO_LIBRARY_PATH)/libinoutcoupler.a
libxgridcoupler: prefix $(MOSSCO_LIBRARY_PATH)/libxgridcoupler.a
libcopycoupler: prefix $(MOSSCO_LIBRARY_PATH)/libcopycoupler.a
libredistcoupler: prefix $(MOSSCO_LIBRARY_PATH)/libredistcoupler.a
libregridcoupler: prefix $(MOSSCO_LIBRARY_PATH)/libregridcoupler.a
libremtc: prefix $(MOSSCO_LIBRARY_PATH)/libremtc.a(remtc_atmos_ocean_coupler.o)
liboceansedimentcoupler: prefix $(MOSSCO_LIBRARY_PATH)/liboceansedimentcoupler.a
libverticalmeancoupler: prefix $(MOSSCO_LIBRARY_PATH)/libverticalmeancoupler.a
libsurfacescoupler: prefix $(MOSSCO_LIBRARY_PATH)/libsurfacescoupler.a(surfaces_coupler.o)
pelagicbenthiccoupler: libmossco_util $(MOSSCO_LIBRARY_PATH)/libpelagicbenthiccoupler.a(pelagic_benthic_coupler.o)
pelagicsoilconnector: libmossco_util $(MOSSCO_LIBRARY_PATH)/libmossco_mediator.a(pelagic_soil_connector.o)
pelagicsoilconnector: libmossco_util $(MOSSCO_LIBRARY_PATH)/libmossco_mediator.a(soil_pelagic_connector.o)
ifeq ($(MOSSCO_SQLITE),true)
soilpelagicmediator: libmossco_util libmossco_db $(MOSSCO_LIBRARY_PATH)/libmossco_mediator.a(soil_pelagic_mediator.o)
endif
benthicpelagiccoupler: libmossco_util $(MOSSCO_LIBRARY_PATH)/libpelagicbenthiccoupler.a(benthic_pelagic_coupler.o)

libsediment:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_util libmossco_db:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

$(MOSSCO_LIBRARY_PATH)/libmossco_mediator.a(pelagic_benthic_mediator.o): pelagic_benthic_mediator.F90 pelagic_benthic_mediator.o
	$(AR) cruvs $@ pelagic_benthic_mediator.o

$(MOSSCO_LIBRARY_PATH)/libregridcoupler.a: regrid_coupler.o
	$(AR) cruvs $@ regrid_coupler.o

$(MOSSCO_LIBRARY_PATH)/libinoutcoupler.a: inout_coupler.o
	$(AR) cruvs $@ inout_coupler.o

$(MOSSCO_LIBRARY_PATH)/libredistcoupler.a: redist_coupler.o
	$(AR) cruvs $@ redist_coupler.o

$(MOSSCO_LIBRARY_PATH)/libcopycoupler.a: copy_coupler.o
	$(AR) cruvs $@ copy_coupler.o

$(MOSSCO_LIBRARY_PATH)/libxgridcoupler.a: xgrid_coupler.o
	$(AR) cruvs $@ xgrid_coupler.o

$(MOSSCO_LIBRARY_PATH)/libremtc.a: remtc_atmos_ocean_coupler.o
	$(AR) cruvs $@ remtc_atmos_ocean_coupler.o

$(MOSSCO_LIBRARY_PATH)/liboceansedimentcoupler.a: ocean_sediment_coupler.o
	$(AR) cruvs $@ ocean_sediment_coupler.o

$(MOSSCO_LIBRARY_PATH)/libverticalmeancoupler.a: vertical_mean_coupler.o
	$(AR) cruvs $@ vertical_mean_coupler.o

$(MOSSCO_LIBRARY_PATH)/libsurfacescoupler.a(surfaces_coupler.o): surfaces_coupler.o

$(MOSSCO_LIBRARY_PATH)/libpelagicbenthiccoupler.a: pelagic_benthic_coupler.o benthic_pelagic_coupler.o pelagic_soil_connector.o soil_pelagic_connector.o
	$(AR) cruvs $@ $?

$(MOSSCO_LIBRARY_PATH)/libmossco_mediator.a: pelagic_soil_connector.o pelagic_benthic_mediator.o soil_pelagic_connector.o
	$(AR) cruvs $@ $?

ifeq ($(MOSSCO_SQLITE),true)
$(MOSSCO_LIBRARY_PATH)/libmossco_mediator.a(soil_pelagic_mediator.o): soil_pelagic_mediator.F90 soil_pelagic_mediator.o
	$(AR) cruvs $@ soil_pelagic_mediator.o
endif

subdirs:
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir ; done

clean: extraclean

extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_mediator.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libxgridcoupler.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libinoutcoupler.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libcopycoupler.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libredistcoupler.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libregridcoupler.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libremtc.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/liboceansedimentcoupler.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libverticalmeancoupler.a
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done

test: test_regrid_coupler

test_regrid_coupler: regrid_coupler.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS) -lesmf
