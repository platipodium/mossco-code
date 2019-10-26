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
export MOSSCO_DIR=$(subst /examples/esmf/getm$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

MOSSCO_LDFLAGS += -L$(MOSSCO_LIBRARY_PATH) -lmossco_getm -lmossco_util
MOSSCO_LDFLAGS += $(GETM_LDFLAGS)
MOSSCO_LDFLAGS += $(ESMF_F90LDFLAGS)

.PHONY: all exec getm libmossco_getm libmossco_util

all: exec
exec: libmossco_util libmossco_getm getm

libmossco_getm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

getm: toplevel_component.o ../../common/main.o
	$(F90) $^ $(MOSSCO_LDFLAGS) $(LDFLAGS) -o $@
	@echo "Created example binary $(CURDIR)/$@"

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

clean: extraclean

extraclean: 
	@rm -f getm toplevel_component.o
	@rm -f PET*.*
