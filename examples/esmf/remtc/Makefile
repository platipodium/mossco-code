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
export MOSSCO_DIR=$(subst /examples/esmf/remtc$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lremtc -lmossco_util $(LIBS)
LDFLAGS += $(MOSSCO_LDFLAGS) $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec remtc libmossco_util libremtc

all: exec
exec: libmossco_util libremtc remtc

remtc: toplevel_component.o ../../common/main.o
	$(F90) $(F90FLAGS) $^ $(LDFLAGS) -o $@
	@echo "Created example $(PWD)/$@"

libremtc:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/remtc $@
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

clean: extraclean

extraclean:
	@rm -f remtc main.F90
