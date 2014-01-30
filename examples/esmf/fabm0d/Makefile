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
export MOSSCO_DIR=$(subst /examples/esmf/fabm0d$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

ifneq ($(MOSSCO_GOTM),true)
$(error You need GOTM to compile this example)
endif


LIBS := -lesmf $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS) -lmossco_fabm0d -lsolver -lmossco_util
LDFLAGS += -L$(GOTM_LIBRARY_PATH) -lairsea_prod -linput_prod -lutil_prod
LDFLAGS += -L$(FABM_LIBRARY_PATH) -lfabm_prod
#ifeq ($(HAVE_LD_FORCE_LOAD),true)
#LDFLAGS += -Wl,-force_load
#endif

.PHONY: all exec 

all: exec
exec: fabm0d

libmossco_fabm0d:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@
	
libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

fabm0d: toplevel_component.o main.o
	$(F90) $(F90FLAGS) -o $@ $^ $(LDFLAGS)
	@echo "Created example $(MOSSCO_DIR)/examples/esmf/$@"

main.o: toplevel_component.o main.F90 libmossco_util
toplevel_component.o: toplevel_component.F90 libmossco_fabm0d
main.F90: ../../common/main.F90
	ln -f  $< $@

fabm0d:
	( unset LIBS; export FABMHOST=0d ; $(MAKE) -C $(FABMDIR)/src/ )
	( unset LIBS; export FABMHOST=0d ; $(MAKE) -C $(FABMDIR)/src/drivers/0d fabm0d )
	mv $(FABMDIR)/src/drivers/0d/fabm0d .

clean: extraclean

extraclean: 
	@rm -f esmf_fabm0d main.F90
	@rm -f fabm0d
	@rm -f PET*.*
