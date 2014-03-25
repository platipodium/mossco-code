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
export MOSSCO_DIR=$(subst /examples/esmf/empty$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS :=  -lmossco_util -lesmf $(LIBS)

.PHONY: all exec

all: exec
exec: empty

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

empty: main.o toplevel_component.o
	$(F90) $(F90FLAGS) -o $@ $^ $(LDFLAGS) $(LIBS)
	@echo "Created example $(MOSSCO_DIR)/examples/esmf/$@"
	
main.o: toplevel_component.o main.F90 libmossco_util
toplevel_component.o: toplevel_component.F90
main.F90: ../../common/main.F90
	ln -f  $< $@

clean: extraclean

extraclean: 
	@rm -f empty main.F90
