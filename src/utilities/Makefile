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
export MOSSCO_DIR=$(subst /src/utilities$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

.PHONY: libsolver

# Make targets

all: libsolver libtypes libmossco_state

libsolver: prefix $(MOSSCO_LIBRARY_PATH)/libsolver.a(solver_library.o)
libtypes:  prefix $(MOSSCO_LIBRARY_PATH)/libtypes.a(mossco_variable_types.o)
libmossco_state:  prefix $(MOSSCO_LIBRARY_PATH)/libmossco_state.a(mossco_state.o)

#@todo there should be a generic rule for the mods
$(MOSSCO_MODULE_PATH)/solver_library.mod: solver_library.F90 
$(MOSSCO_MODULE_PATH)/mossco_variable_types.mod: mossco_variable_types.o
$(MOSSCO_MODULE_PATH)/mossco_state.mod: mossco_state.o

clean: extraclean
extraclean:
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libsolver.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libtypes.a
	@-rm -f $(MOSSCO_MODULE_PATH)/solver_library.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_variable_types.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_state.mod
