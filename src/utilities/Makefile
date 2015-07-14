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
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_strings.o)	\
	#$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_db.o)
ifeq ($(MOSSCO_ESMF),true)
LIBMOSSCO_UTIL_OBJS+= 							\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_component.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_state.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_field.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_attribute.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_config.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_time.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_variable_types.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_grid.o)

ifeq ($(MOSSCO_NETCDF),true)
LIBMOSSCO_UTIL_OBJS+= 							\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_netcdf.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_gridspec.o)
endif
endif

# SQLITE section
SQLITE_OBJS= \
  $(SQLITE_DIR)/fsqlite.o  \
  $(SQLITE_DIR)/csqlite.o

ifeq ($(MOSSCO_SQLITE),true)
  TARGETS += $(MOSSCO_LIBRARY_PATH)/libmossco_db.a(mossco_db.o)
endif

# Make targets
ifeq ($(MOSSCO_NETCDF),true)
  #TARGETS += nc_extract_variable nc_extract_variable_lasttime
endif

all: libsolver libmossco_util $(TARGETS)

libsolver: prefix $(MOSSCO_LIBRARY_PATH)/libsolver.a(solver_library.o)
libmossco_util:  prefix sha $(LIBMOSSCO_UTIL_OBJS)
mossco_state.o : mossco_field.o mossco_state.F90
mossco_field.o : mossco_strings.o mossco_field.F90
mossco_gridspec.o : mossco_strings.o mossco_gridspec.F90
mossco_component.o : mossco_strings.o mossco_state.o mossco_field.o mossco_component.F90
mossco_netcdf.o : mossco_gridspec.o mossco_netcdf.F90
mossco_attribute.o : mossco_attribute.F90
mossco_config.o : mossco_config.F90
mossco_db.o : mossco_db.F90 \
	$(MOSSCO_LIBRARY_PATH)/libmossco_db.a(fsqlite.o)

$(MOSSCO_LIBRARY_PATH)/libmossco_db.a(fsqlite.o): $(SQLITE_DIR)/fsqlite.f90
	make -C $(SQLITE_DIR)
	$(AR) cruvs $(MOSSCO_LIBRARY_PATH)/libmossco_db.a $(SQLITE_DIR)/fsqlite.o
	$(AR) cruvs $(MOSSCO_LIBRARY_PATH)/libmossco_db.a $(SQLITE_DIR)/csqlite.o
	cp  $(SQLITE_DIR)/*mod $(MOSSCO_MODULE_PATH)/

nc_extract_variable: nc_extract_variable.c
	$(MOSSCO_CCOMPILER) -I $(MOSSCO_NETCDF_INCLUDE) -c $<
	$(MOSSCO_CCOMPILER) -o $@ nc_extract_variable.o -L $(MOSSCO_NETCDF_LIBPATH) $(MOSSCO_NETCDF_LIBS)

nc_extract_variable_lasttime: nc_extract_variable_lasttime.c
	$(MOSSCO_CCOMPILER) -I $(MOSSCO_NETCDF_INCLUDE) -c $<
	$(MOSSCO_CCOMPILER) -o $@ nc_extract_variable_lasttime.o -L $(MOSSCO_NETCDF_LIBPATH) $(MOSSCO_NETCDF_LIBS)

clean: extraclean
extraclean:
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libsolver.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_util.a
	@-rm -f $(MOSSCO_MODULE_PATH)/solver_library.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_variable_types.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_state.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_field.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_attribute.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_time.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_strings.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_grid.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_netcdf.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_config.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_gridspec.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_dictionary.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_component.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_db.mod
	$(RM) *.o nc_extract_variable_lasttime nc_extract_variable
