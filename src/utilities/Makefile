# This Makefile is part of MOSSCO
#
# @copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/utilities$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

.PHONY: libsolver libmossco_util libmossco_db $(LIBMOSSCO_UTIL_OBJS) libatt

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
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_memory.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_logging.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_parameter.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_config.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_time.o)		\
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_variable_types.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_grid.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_mesh.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_geom.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_loc.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_routehandle.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_locstream.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_util.a(ESMF_GridCreate3D.o)

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
  TARGETS += libmossco_db
endif

# Make targets
ifeq ($(MOSSCO_NETCDF),true)
  #TARGETS += nc_extract_variable nc_extract_variable_lasttime
endif

all: libsolver libmossco_util $(TARGETS)

libsolver: prefix $(MOSSCO_LIBRARY_PATH)/libsolver.a(solver_library.o)
libmossco_util:  prefix sha $(LIBMOSSCO_UTIL_OBJS)
libmossco_db: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_db.a(mossco_db.o)


# Depends on multiple
mossco_component.o : mossco_component.F90 mossco_state.o
mossco_netcdf.o : mossco_netcdf.F90 mossco_state.o mossco_time.o \
  mossco_variable_types.o mossco_gridspec.o versions
mossco_field.o : mossco_field.F90 mossco_strings.o mossco_attribute.o \
  mossco_geom.o mossco_loc.o

mossco_state.o : mossco_state.F90 mossco_field.o
mossco_logging.o : mossco_logging.F90 mossco_state.o

$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_logging.o) : mossco_logging.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_state.o): mossco_state.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_field.o) : mossco_field.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_component.o) : mossco_component.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_netcdf.o): mossco_netcdf.o

# Depends on ESMF_GridCreate3D, mesh/grid/locstream
mossco_geom.o : mossco_geom.F90 mossco_mesh.o mossco_grid.o mossco_locstream.o \
  ESMF_GridCreate3D.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_geom.o) : mossco_geom.o

# Depends on mossco_strings and its dependents
mossco_config.o : mossco_config.F90 mossco_strings.o
mossco_loc.o: mossco_loc.F90 mossco_strings.o
mossco_locstream.o : mossco_locstream.F90 mossco_strings.o
mossco_grid.o : mossco_grid.F90 mossco_strings.o
mossco_mesh.o : mossco_mesh.F90 mossco_strings.o
mossco_routehandle.o : mossco_routehandle.F90 mossco_strings.o
mossco_gridspec.o : mossco_gridspec.F90 mossco_strings.o

$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_config.o) : mossco_config.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_loc.o) : mossco_loc.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_gridspec.o) : mossco_gridspec.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_locstream.o) : mossco_locstream.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_grid.o) : mossco_grid.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_mesh.o) : mossco_mesh.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_routehandle.o) : mossco_routehandle.o

# Depends on mossco_memory
mossco_strings.o : mossco_strings.F90 mossco_memory.o

# Libraries that are independent of others
mossco_attribute.o : mossco_attribute.F90
ESMF_GridCreate3D.o : ESMF_GridCreate3D.F90
mossco_dictionary.o : mossco_dictionary.F90
mossco_memory.o : mossco_memory.F90
mossco_parameter.o : mossco_parameter.F90
mossco_variable_types.o : mossco_variable_types.F90
mossco_time.o : mossco_time.F90
mossco_db.o : mossco_db.F90

$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_strings.o) : mossco_strings.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_memory.o) : mossco_memory.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_config.o) : mossco_config.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_parameter.o) : mossco_parameter.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_attribute.o) : mossco_attribute.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_dictionary.o) : mossco_dictionary.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_time.o) : mossco_time.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_variable_types.o) : mossco_variable_types.o
$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(ESMF_GridCreate3D.o) : ESMF_GridCreate3D.o

$(MOSSCO_LIBRARY_PATH)/libmossco_db.a(fsqlite.o): $(SQLITE_DIR)/fsqlite.f90
	make -C $(SQLITE_DIR)
	$(AR) cruvs $(MOSSCO_LIBRARY_PATH)/libmossco_db.a $(SQLITE_DIR)/fsqlite.o
	$(AR) cruvs $(MOSSCO_LIBRARY_PATH)/libmossco_db.a $(SQLITE_DIR)/csqlite.o
	cp  $(SQLITE_DIR)/*mod $(MOSSCO_MODULE_PATH)/

$(MOSSCO_LIBRARY_PATH)/libmossco_util.a(mossco_db.o) : mossco_db.o \
  $(MOSSCO_LIBRARY_PATH)/libmossco_db.a(fsqlite.o)

nc_extract_variable: nc_extract_variable.c
	$(MOSSCO_CCOMPILER) -I $(MOSSCO_NETCDF_INCLUDE) -c $<
	$(MOSSCO_CCOMPILER) -o $@ nc_extract_variable.o -L $(MOSSCO_NETCDF_LIBPATH) $(MOSSCO_NETCDF_LIBS)

nc_extract_variable_lasttime: nc_extract_variable_lasttime.c
	$(MOSSCO_CCOMPILER) -I $(MOSSCO_NETCDF_INCLUDE) -c $<
	$(MOSSCO_CCOMPILER) -o $@ nc_extract_variable_lasttime.o -L $(MOSSCO_NETCDF_LIBPATH) $(MOSSCO_NETCDF_LIBS)

versions:  ../include/git-sha.h
../include/git-sha.h:
	make -C ../include/

clean: extraclean
extraclean:
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libsolver.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_util.a
	@-rm -f $(MOSSCO_MODULE_PATH)/solver_library.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_variable_types.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_logging.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_state.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_field.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_attribute.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_time.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_memory.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_strings.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_grid.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_mesh.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_geom.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_loc.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_routehandle.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_locstream.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_netcdf.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_parameter.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_config.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_gridspec.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_dictionary.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_component.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/mossco_db.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/ESMF_GridCreate3D.mod
	$(RM) *.o nc_extract_variable_lasttime nc_extract_variable
