# This Makefile is part of MOSSCO
#
# @copyright (C) 2013, 2014, 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de
# @author Richard Hofmeister, Helmholtz-Zentrum Geesthacht
# @author Knut Klingbeil, Institut für Ostseeforschung Warnemünde
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/components/gotm$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifeq ($(MOSSCO_GOTM),true)
TARGETS += libmossco_gotm
ifeq ($(MOSSCO_FABM),true)
TARGETS += libmossco_fabmgotm
TARGETS += libmossco_gotmfabm
endif
endif

.PHONY: $(TARGETS) libgotm_external libmossco_util

#--------------------------------------
# Make targets
# add checks for libfabm and libsediment

all: $(TARGETS)

libmossco_gotm: prefix libgotm_external libmossco_util \
	$(MOSSCO_LIBRARY_PATH)/libmossco_gotm.a(gotm_component.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_gotm.a(gotm_transport_component.o)

libmossco_fabmgotm: prefix libmossco_fabmgotm_driver \
	$(MOSSCO_LIBRARY_PATH)/libmossco_fabmgotm.a(fabm_gotm_component.o)

libmossco_gotmfabm: prefix libmossco_fabmpelagic libmossco_gotm libmossco_fabmgotm \
	$(MOSSCO_LIBRARY_PATH)/libmossco_gotmfabm.a(gotmfabm_component.o)

%.o: %.F90
	@echo "Compiling components/gotm/$<"
	$(F90) $(GOTM_CPPFLAGS) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

libmossco_fabmpelagic:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_fabmgotm_driver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers libmossco_fabmgotm

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities libmossco_util

clean: extraclean
	$(RM) *.o

extraclean:
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_gotm.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_fabmgotm.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_gotmfabm.a
