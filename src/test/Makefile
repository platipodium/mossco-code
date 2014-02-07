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
LDFLAGS += $(LIBRARY_PATHS) $(LIBS) -lnetcdff

all: test_FieldGet test_ArrayGet test_Clock

test_ArrayGet: test_ArrayGet.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)

test_FieldGet: test_FieldGet.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)

test_Clock: test_Clock.o
	$(F90) $(F90FLAGS) -o $@  $^ $(LDFLAGS)

clean:
	rm *.o

distclean: clean
	rm testfieldget test_ArrayGet
