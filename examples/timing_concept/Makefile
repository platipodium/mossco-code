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
export MOSSCO_DIR ?= $(subst /examples/timing_concept$,,$(PWD))

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lesmf $(LIBS) -lempty -lconstant

.PHONY: all exec libempty libconstant

all: exec

exec: main.o toplevel_component.o
	$(F90) $(F90FLAGS) -o timing_concept toplevel_component.o main.o mossco_time.o $(LDFLAGS) $(LIBS)
	@echo "Created example binary timing_concept in $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/timing_concept/ to run the example." > $(MOSSCO_BIN_PATH)/timing_concept.txt
	
main.o: toplevel_component.o main.F90 libmossco_util
toplevel_component.o: toplevel_component.F90 libempty mossco_time.o libconstant

main.F90: ../common/main.F90
	ln -f $< $@

libconstant:
	$(MAKE) -C $(MOSSCO_DIR)/src/components libconstant

libempty:
	$(MAKE) -C $(MOSSCO_DIR)/src/components libempty

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

clean: extraclean

extraclean: 
	@rm -f timing_concept
