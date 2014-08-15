# This Makefile is part of MOSSCO
# 
# @copyright (C) 2013, 2014 Helmholtz-Zentrum Geesthacht
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
TARGETS += libremtc liblinkcoupler libgridcoupler libredistcoupler libcopycoupler libregridcoupler
ifeq ($(MOSSCO_FABM),true)
TARGETS += liboceansedimentcoupler
TARGETS += pelagicbenthiccoupler
TARGETS += benthicpelagiccoupler
endif
ifeq ($(MOSSCO_GOTM),true)
TARGETS += libverticalmeancoupler
endif
endif

SUBDIRS=

.PHONY: default all subdirs $(TARGETS)

all: subdirs $(TARGETS)

libgridcoupler: prefix $(MOSSCO_LIBRARY_PATH)/libgridcoupler.a
liblinkcoupler: prefix $(MOSSCO_LIBRARY_PATH)/liblinkcoupler.a
libcopycoupler: prefix $(MOSSCO_LIBRARY_PATH)/libcopycoupler.a
libredistcoupler: prefix $(MOSSCO_LIBRARY_PATH)/libredistcoupler.a
libregridcoupler: prefix $(MOSSCO_LIBRARY_PATH)/libregridcoupler.a
libremtc: prefix $(MOSSCO_LIBRARY_PATH)/libremtc.a(remtc_atmos_ocean_coupler.o)
liboceansedimentcoupler: prefix $(MOSSCO_LIBRARY_PATH)/liboceansedimentcoupler.a
libverticalmeancoupler: prefix $(MOSSCO_LIBRARY_PATH)/libverticalmeancoupler.a
libsurfacescoupler: prefix $(MOSSCO_LIBRARY_PATH)/libsurfacescoupler.a(surfaces_coupler.o)
pelagicbenthiccoupler: prefix  $(MOSSCO_LIBRARY_PATH)/libpelagicbenthiccoupler.a(pelagic_benthic_coupler.o)
benthicpelagiccoupler: prefix  $(MOSSCO_LIBRARY_PATH)/libpelagicbenthiccoupler.a(benthic_pelagic_coupler.o)

$(MOSSCO_LIBRARY_PATH)/libregridcoupler.a: regrid_coupler.o 
	$(AR) cruvs $@ regrid_coupler.o

$(MOSSCO_LIBRARY_PATH)/libredistcoupler.a: redist_coupler.o 
	$(AR) cruvs $@ redist_coupler.o

$(MOSSCO_LIBRARY_PATH)/libcopycoupler.a: copy_coupler.o 
	$(AR) cruvs $@ copy_coupler.o

$(MOSSCO_LIBRARY_PATH)/liblinkcoupler.a: link_coupler.o 
	$(AR) cruvs $@ link_coupler.o

$(MOSSCO_LIBRARY_PATH)/libgridcoupler.a: grid_coupler.o 
	$(AR) cruvs $@ grid_coupler.o

$(MOSSCO_LIBRARY_PATH)/libremtc.a: remtc_atmos_ocean_coupler.o 
	$(AR) cruvs $@ remtc_atmos_ocean_coupler.o

$(MOSSCO_LIBRARY_PATH)/liboceansedimentcoupler.a: ocean_sediment_coupler.o
	$(AR) cruvs $@ ocean_sediment_coupler.o

$(MOSSCO_LIBRARY_PATH)/libverticalmeancoupler.a: vertical_mean_coupler.o
	$(AR) cruvs $@ vertical_mean_coupler.o

$(MOSSCO_LIBRARY_PATH)/libsurfacescoupler.a(surfaces_coupler.o): surfaces_coupler.o

$(MOSSCO_LIBRARY_PATH)/libpelagicbenthiccoupler.a: pelagic_benthic_coupler.o benthic_pelagic_coupler.o
	$(AR) cruvs $@ $?

subdirs: 
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir ; done 

clean: extraclean

extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libgridcoupler.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/liblinkcoupler.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libcopycoupler.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libredistcoupler.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libregridcoupler.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libremtc.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/liboceansedimentcoupler.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libverticalmeancoupler.a
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done 

test: test_grid_coupler	

test_grid_coupler: grid_coupler.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS) -lesmf
