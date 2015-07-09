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
export MOSSCO_DIR=$(subst /examples/standalone/erosed$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lmossco_erosed -lmossco_benthos $(LIBS)

F90FLAGS += $(INCLUDES)
LDFLAGS += $(MOSSCO_F90LDFLAGS) $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec erosed libmossco_erosed

all: exec
exec: libmossco_erosed libmossco_benthos erosed

libmossco_erosed libmossco_benthos:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

erosed: delft_erosed_example.o
	$(F90) -o $@ $< $(LDFLAGS)
	@echo "Created example binary erosed in  $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/standalone/erosed to run the example." > $(MOSSCO_BIN_PATH)/erosed.txt

clean: extraclean

extraclean: 
	@rm -f erosed
