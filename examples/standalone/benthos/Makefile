# This Makefile is part of MOSSCO
# 
# @copyright (C) 2013, 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
# @author Hassan Nasermoaddeli
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/standalone/benthos$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lmossco_benthos  $(LIBS)

F90FLAGS += $(INCLUDES)
MOSSCO_LDFLAGS += $(MOSSCO_F90LDFLAGS) $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec benthos libmossco_benthos

all: exec
exec: libmossco_benthos benthos

libmossco_benthos:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

benthos: BioDriver.o
	$(F90) -o $@ $< $(MOSSCO_LDFLAGS) $(LDFLAGS)
	@echo "Created example $(CURDIR)/$@" 

clean: extraclean

extraclean: 
	@rm -f benthos
