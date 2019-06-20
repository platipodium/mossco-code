# This Makefile is part of MOSSCO
#
# @copyright (C) 2018 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/standalone/vertical_macrobenthos$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lmossco_macrobenthos -lsolver $(LIBS)

LDFLAGS += $(MOSSCO_F90LDFLAGS) $(LIBS)

.PHONY: all exec vertical-macrobenthos libmossco_macrobenthos

all: exec
exec: libmossco_macrobenthos vertical_macrobenthos

libmossco_macrobenthos: libmossco_util libsolver
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

vertical_macrobenthos: main.o
	$(F90) $^ $(LDFLAGS) -o $@
	@echo "Created example binary $(CURDIR)/$@"

clean: extraclean

extraclean:
	@rm -f vertical_macrobenthos

libsolver libmossco_util:
	make -C $(MOSSCO_DIR)/src/utilities $@
