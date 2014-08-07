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
export MOSSCO_DIR=$(subst /src/test$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lesmf  $(LIBS)
LDFLAGS += $(MOSSCO_LDFLAGS) $(LIBRARY_PATHS) 
TEST_SRC = $(shell ls  *.c *.F90)
TEST_OBJ = ${TEST_SRC%.}.o


.PHONY: libmossco_empty libmossco_constant libmossco_info

all: test_FieldGet test_ArrayGet test_Clock test_Netcdf test_Grid test_ClockDestroy test_Concurrency test_Concurrency test_FieldBounds test_ESMF_Initialize

tests:
	echo TEST_SRC=$(TEST_SRC)
	echo TEST_OBJ=$(TEST_OBJ)


test_ESMF_Initialize: test_ESMF_Initialize.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)  $(LIBS)

test_Concurrency: test_Concurrency.o libmossco_constant libmossco_info
	$(F90) $(F90FLAGS) -o $@  $< $(LDFLAGS) -lconstant -lmossco_info $(LIBS)

test_FieldBounds: test_FieldBounds.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS) $(LIBS)

test_Grid: test_Grid.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS) $(LIBS)

test_Ugrid: test_Ugrid.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS) $(LIBS)

test_ArrayGet: test_ArrayGet.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS) $(LIBS)

test_FieldGet: test_FieldGet.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS) $(LIBS)

test_GridCreate: test_GridCreate.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS) $(LIBS)

test_ClockDestroy:  test_ClockDestroy.o libmossco_empty 
	$(F90) $(F90FLAGS) -o $@  $<  $(LDFLAGS) -lempty $(LIBS)

libmossco_empty libmossco_info libmossco_constant:
	$(MAKE) -C ../components $@

test_Clock: test_Clock.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS) $(LIBS)

test_DerivedTypes: test_DerivedTypes.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS) $(LIBS)

test_Netcdf: test_Netcdf.o 
	$(F90) $(F90FLAGS) -o $@  $^ -lmossco_util $(LDFLAGS) $(LIBS)

test_Solver: test_Solver.o 
	$(F90) $(F90FLAGS) -o $@  $^ -lsolver $(LDFLAGS) $(LIBS)

test_Dictionary: test_Dictionary.o 
	$(F90) $(F90FLAGS) -o $@  $^ -lmossco_util $(LDFLAGS) $(LIBS)

test_FabmDependencies: test_FabmDependencies.o 
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS) -L$(FABM_LIBRARY_PATH) -lfabm_prod $(LIBS)

test_Netcdf.o test_Dictionary.o: libmossco_util

libmossco_util:
	make -C $(MOSSCO_DIR)/src/utilities libmossco_util

clean: extraclean

extraclean:
	@- rm -f test_FieldGet test_ArrayGet test_Clock test_GridCreate PET* test_Grid test_ClockDestroy
