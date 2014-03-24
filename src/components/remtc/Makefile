# This Makefile is part of MOSSCO
# 
# @copyright (C) 2013 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/components/remtc$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

SUBDIRS=

TARGETS=SUBDIRS

ifeq ($(MOSSCO_ESMF),true)
TARGETS += libremtc
endif

.PHONY: $(TARGETS)

#--------------------------------------
# Make targets

all: $(TARGETS)

libremtc: prefix libmossco_util $(MOSSCO_LIBRARY_PATH)/libremtc.a(remtc_ocean.o) \
          $(MOSSCO_LIBRARY_PATH)/libremtc.a(remtc_atmosphere.o)
libmossco_util: prefix $(MOSSCO_MODULE_PATH)/mossco_variable_types.mod

$(MOSSCO_MODULE_PATH)/mossco_variable_types.mod:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities libmossco_util

clean: extraclean
extraclean:
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libremtc.a
	
subdirs: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@
