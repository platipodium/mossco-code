# This Makefile is part of MOSSCO
# 
# @copyright (C) 2013, 2014, 2015 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
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

.PHONY: libsolver libmossco_util

LIBMOSSCO_UTIL_OBJS=							\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_dictionary.o)	\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_strings.o)
ifeq ($(MOSSCO_ESMF),true)
LIBMOSSCO_UTIL_OBJS+= 							\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_component.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_state.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_field.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_time.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_variable_types.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_grid.o)
ifeq ($(MOSSCO_NETCDF),true)
LIBMOSSCO_UTIL_OBJS+= 							\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_netcdf.o)
endif
endif

# Make targets

all: libsolver libmossco_util

libsolver: prefix $(MOSSCO_LIBRARY_PATH)/libsolver.a(solver_library.o)
libmossco_util:  prefix sha $(LIBMOSSCO_UTIL_OBJS)
mossco_state.o : mossco_field.o
mossco_field.o : mossco_strings.o
mossco_component.o : mossco_strings.o

#@todo there should be a generic rule for the mods
#$(MOSSCO_MODULE_PATH)/solver_library.mod: solver_library.F90 
#$(MOSSCO_MODULE_PATH)/mossco_variable_types.mod: mossco_variable_types.o
#$(MOSSCO_MODULE_PATH)/mossco_state.mod: mossco_state.o
#$(MOSSCO_MODULE_PATH)/mossco_field.mod: mossco_state.o
#$(MOSSCO_MODULE_PATH)/mossco_time.mod: mossco_time.o
#$(MOSSCO_MODULE_PATH)/mossco_strings.mod: mossco_strings.o
#$(MOSSCO_MODULE_PATH)/mossco_netcdf.mod: mossco_netcdf.o mossco_variable_types.o
#$(MOSSCO_MODULE_PATH)/mossco_dictionary.mod: mossco_dictionary.o

clean: extraclean
extraclean:
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libsolver.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_util.a
	@-rm -f $(MOSSCO_MODULE_PATH)/solver_library.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_variable_types.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_state.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_field.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_time.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_strings.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_grid.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_netcdf.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_dictionary.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_component.mod

