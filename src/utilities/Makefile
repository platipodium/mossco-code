# This Makefile is part of MOSSCO
# 
# Copyright (C) 2013 Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#

ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/utilities$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

.PHONY: libsolver

# Make targets

all: libsolver libtypes

libsolver: prefix $(MOSSCO_LIBRARY_PATH)/libsolver.a
libtypes:  prefix $(MOSSCO_LIBRARY_PATH)/libtypes.a

$(MOSSCO_MODULE_PATH)/solver_library.mod: solver_library.F90 
$(MOSSCO_MODULE_PATH)/mossco_variable_types.mod: mossco_variable_types.F90

$(MOSSCO_LIBRARY_PATH)/libtypes.a: mossco_variable_types.F90
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libtypes.a mossco_variable_types.F90
	@echo "Created types library  $(MOSSCO_LIBRARY_PATH)/libtypes.a" 

$(MOSSCO_LIBRARY_PATH)/libsolver.a: solver_library.o
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libsolver.a solver_library.o
	@echo "Created solver library  $(MOSSCO_LIBRARY_PATH)/libsolver.a" 

clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libsolver.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libtypes.a
	@rm -f $(MOSSCO_MODULE_PATH)/solver_library.mod
	@rm -f $(MOSSCO_MODULE_PATH)/mossco_variable_types.mod
