# This Makefile is part of MOSSCO
# 
# @copyright (C) 2013, 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
# @author Richard Hofmeister, Helmholtz-Zentrum Geesthacht    
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#

ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/ocean_fabm0d_fabmsed$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

NC_LIBS += $(shell nf-config --flibs)
LIBS := -lsediment -lremtc -loceansedimentcoupler -lmossco_fabm0d -lmossco_util -lmossco_sediment -lsolver $(LIBS)
LDFLAGS += $(MOSSCO_LDFLAGS) $(LIBRARY_PATHS) $(LIBS)
LDFLAGS += -L$(GOTM_LIBRARY_PATH) -lairsea_prod -linput_prod -lutil_prod
LDFLAGS += -L$(FABM_LIBRARY_PATH) -lfabm_prod
#ifeq ($(HAVE_LD_FORCE_LOAD),true)
#LDFLAGS += -Wl,-force_load
#endif

.PHONY: all exec libremtc libsediment liboceansedimentcoupler libmossco_util

all: exec
exec: ocean_fabm0d_fabmsed

ocean_fabm0d_fabmsed: toplevel_component.o main.o  
	$(F90) $(F90FLAGS) -o ocean_fabm0d_fabmsed  $^ $(LDFLAGS)
	@echo "Created example binary ocean_fabm0d_fabmsed in $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/ocean_fabm0d_fabmsed/ to run the example." > $(MOSSCO_BIN_PATH)/ocean_fabm0d_fabmsed.txt

libremtc:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/remtc $@

libmossco_fabm0d libsediment:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

liboceansedimentcoupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators $@
	
libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@
	
main.o: toplevel_component.o main.F90 libmossco_util
toplevel_component.o: toplevel_component.F90 libmossco_fabm0d libsediment libremtc liboceansedimentcoupler

main.F90: ../common/main.F90
	ln -f $< $@

clean: extraclean

extraclean: 
	@rm -f ocean_fabm0d_fabmsed main.F90
