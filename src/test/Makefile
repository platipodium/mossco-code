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
TEST_SRC = $(shell ls test_*.F90)
TEST_OBJ = ${TEST_SRC:.F90=.o}
TEST_BIN = ${TEST_SRC:.F90=}

.PHONY: libmossco_empty libmossco_constant libmossco_info

.PHONY: all $(TEST_BIN)
all: $(TEST_BIN)

tests:
	@echo TEST_SRC=$(TEST_SRC)
	@echo TEST_OBJ=$(TEST_OBJ)
	@echo TEST_BIN=$(TEST_BIN)

test_ArrayGet test_Clock test_DerivedTypes test_ESMF_Initialize test_FieldBounds test_FieldGet test_Grid test_GridCreate test_MPI_Initialize test_Redist test_Regrid2d test_Ugrid test_Regrid3d:
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@  $(LDFLAGS)  $(LIBS)

test_Concurrency: libmossco_constant libmossco_info
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) -lconstant -lmossco_info $(LIBS)

test_ClockDestroy: libmossco_empty 
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) -lempty $(LIBS)

test_Dictionary test_Netcdf: libmossco_util
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) -lmossco_util $(LIBS)

test_Solver: libsolver
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) -lsolver $(LIBS)

test_FabmDependencies:
	$(F90) $(CPPFLAGS) $(F90FLAGS) $@.F90 -o $@ $(LDFLAGS) -L$(FABM_LIBRARY_PATH) $(FABM_LIBS) $(LIBS)

libsolver libmossco_util:
	make -C $(MOSSCO_DIR)/src/utilities $@

libmossco_empty libmossco_info libmossco_constant:
	$(MAKE) -C ../components $@

clean: extraclean

extraclean:
	@- rm -f $(TEST_BIN) PET*
