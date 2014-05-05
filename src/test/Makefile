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
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)
TEST_SRC = $(shell ls  *.c *.F90)
TEST_OBJ = ${TEST_SRC%.}.o

tests:
	echo TEST_SRC=$(TEST_SRC)
	echo TEST_OBJ=$(TEST_OBJ)

all: test_FieldGet test_ArrayGet test_Clock test_GridCreate test_Netcdf test_Grid test_ClockDestroy


esmf_initialize_test: ESMF_Initialize_test.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)

test_Grid: test_Grid.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)

test_ArrayGet: test_ArrayGet.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)

test_FieldGet: test_FieldGet.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)

test_GridCreate: test_GridCreate.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)

test_ClockDestroy: test_ClockDestroy.o ../components/empty_component.o
	$(F90) $(F90FLAGS) -o $@  $^  $(LDFLAGS)

../empty_component.o:
	$(MAKE) -C ../components libmossco_empty

test_Clock: test_Clock.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)

test_Netcdf: test_Netcdf.o 
	$(F90) $(F90FLAGS) -o $@  $^ -lmossco_util $(LDFLAGS)

test_Dictionary: test_Dictionary.o 
	$(F90) $(F90FLAGS) -o $@  $^ -lmossco_util $(LDFLAGS)

test_Netcdf.o test_Dictionary.o: libmossco_util

libmossco_util:
	make -C $(MOSSCO_DIR)/src/utilities libmossco_util

clean: extraclean

extraclean:
	@- rm -f test_FieldGet test_ArrayGet test_Clock test_GridCreate PET* test_Grid test_ClockDestroy
