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
export MOSSCO_DIR=$(subst /examples/standalone/tracer/src$,,$(PWD))
endif

#export TRACER_FORCING_ONLINE=false

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lmossco_tracer $(LIBS)

LDFLAGS += $(MOSSCO_LDFLAGS) $(LIBS)

.PHONY: all exec tracer-standalone libmossco_tracer

all: exec
exec: libmossco_tracer tracer-standalone

libmossco_tracer:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

tracer-standalone: tracer-standalone.o
	$(F90) $^ $(LDFLAGS) -o $@
	@echo "Created example binary $(PWD)/$@"

clean: extraclean

extraclean:
	@rm -f tracer-standalone
