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
export MOSSCO_DIR=$(subst /examples/esmf/getm$,,$(PWD))
endif

#export MOSSCO_GETM_FABM=true

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

.PHONY: all exec libmossco_getm_component libmossco_util

all: exec
exec: libmossco_util libmossco_getm_component getm

libmossco_getm_component:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

getm: toplevel_component.o main.o
	$(F90) -o $@  $^ $(LDFLAGS) -lmossco_getm $(GETM_LINKDIRS) $(GETM_LIBS) $(ESMF_F90ESMFLINKLIBS) -lmossco_util
	@echo "Created example binary esmf_getm_example in  $(PWD)"
	@echo "Please change to $(MOSSCO_DIR)/examples/esmf_getm to run the example." > $(MOSSCO_BIN_PATH)/esmf_getm.txt

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

#main.F90: ../../common/main.F90
#	ln -f  $< $@

clean: extraclean

extraclean: 
	@rm -f getm main.o
	@rm -f PET*.*
