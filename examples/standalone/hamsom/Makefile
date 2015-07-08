# This Makefile is part of MOSSCO
#
# @copyright (C) 2013, 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
# @author Markus Kreus, IHF, Uni-Hamburg
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/standalone/hamsom/src$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lmossco_hamsom $(LIBS)

LDFLAGS += $(MOSSCO_F90LDFLAGS) $(LIBS)

.PHONY: all exec hamsom-standalone libmossco_hamsom

all: exec
exec: libmossco_hamsom hamsom-standalone

#%.o: %.f90
#	$(F90) -c $(HAMSOM_CPPFLAGS) $(CPPFLAGS) $(HAMSOM_FFLAGS) $(F90FLAGS) $(LDFLAGS) $<

libmossco_hamsom:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

hamsom-standalone: F90FLAGS += $(HAMSOM_FFLAGS)
hamsom-standalone: CPPFLAGS += $(HAMSOM_CPPFLAGS)
hamsom-standalone: hamsom-standalone.o
	$(F90) $^ $(LDFLAGS) -o $@
	@echo "Created example binary $(PWD)/$@"

#	$(F90)  $(HAMSOM_CPPFLAGS) $(CPPFLAGS) $(HAMSOM_FFLAGS) $(F90FLAGS) $(LDFLAGS) -c $< -o $@

clean: extraclean

extraclean:
	@rm -f hamsom-standalone
