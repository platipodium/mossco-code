# This Makefile is part of MOSSCO
#
# @copyright (C) 2013, 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
# @author Richard Hofmeister, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/standalone/omexdia_p$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lmossco_sediment -lsolver -lmossco_util $(LIBS)

LDFLAGS += $(MOSSCO_LDFLAGS) $(LIBS)
LDFLAGS += $(FABM_LDFLAGS)

.PHONY: all exec omexdia_p libmossco_sediment

all: exec
exec: libmossco_sediment omexdia_p

libmossco_sediment:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

omexdia_p: main.o
	$(F90) $^ $(LDFLAGS) -o $@
	@echo "Created example binary $(PWD)/$@"

clean: extraclean

extraclean:
	@rm -f output.dat omexdia_p
