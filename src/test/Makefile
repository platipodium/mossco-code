# This Makefile is part of MOSSCO
#
# @copyright (C) 2013, 2014, 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht
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
export MOSSCO_DIR=$(subst /src/test$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

TESTS = $(shell cat TESTS)

LIBS := -lesmf  $(LIBS)
LDFLAGS += $(MOSSCO_F90LDFLAGS) $(LIBRARY_PATHS)

.PHONY: libmossco_technical libmossco_constant libmossco_info libmossco_util libmossco_db

.PHONY: all tests $(TESTS)
all: tests

tests: $(TESTS)

test_StateGet test_ArrayGet test_Clock test_DerivedTypes test_ESMF_Initialize test_FieldBounds test_FieldGet test_Grid test_GridCreate test_MPI_Initialize  test_Regrid2d test_Ugrid test_Regrid3d test_RegridLocStream test_Regrid2d_GridToMesh test_Regrid2d_GridToMesh2 test_GetArea test_precision:
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@  $(LDFLAGS)  $(LIBS)

test_mossco_db: test_mossco_db.F90 libmossco_db
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) $(SQLITE_LIBS)

test_Redist: test_Redist.F90 libmossco_technical
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) -lmossco_technical -lmossco_util $(LIBS)

test_Coupler: test_Coupler.F90 libmossco_mediator
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) -lmossco_mediator $(SQLITE_LIBS) -lmossco_util $(LIBS)

libmossco_mediator:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators $@

test_mossco_strings: test_mossco_strings.F90 libmossco_util
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) -lmossco_util $(LIBS)

test_mossco_config: test_mossco_config.F90 libmossco_util
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) -lmossco_util $(LIBS)

test_Concurrency: libmossco_constant libmossco_info
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) -lconstant -lmossco_info -lmossco_util $(LIBS)

test_ClockDestroy: libmossco_technical
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) -lmossco_technical -lmossco_util $(LIBS)

test_Dictionary test_mossco_netcdf: libmossco_util
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) -lmossco_util $(LIBS)

test_netcdf:
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) $(LIBS)

test_Solver: libsolver
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) -lsolver $(LIBS)

test_FabmDependencies:
	$(F90) $(CPPFLAGS) $(FABM_CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) $(FABM_LDFLAGS) $(LIBS)

test_pointer: test_pointer.F90
	$(F90) $< -o $@

test_ConfigTable: test_ConfigTable.F90
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) -lsolver $(LIBS)

libsolver libmossco_util libmossco_db:
	make -C $(MOSSCO_DIR)/src/utilities $@

libmossco_technical libmossco_info libmossco_constant:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

clean: extraclean

extraclean:
	@- rm -f $(TESTS) PET*
	@- rm -rf *.dSYM test_netcdf.nc
