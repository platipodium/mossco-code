# This Makefile is part of MOSSCO
#
# @copyright (C) 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/test$,,$(CURDIR))
endif

EXTRA_DIST=README.md


#PY ?= $(which python)
PY = python
CCPY = $(MOSSCO_DIR)/examples/generic/create_coupling.py

YAML=$(wildcard *yaml)

all: $(YAML) $(CCPY)
	@- echo $(YAML)
	@- for T in $(YAML) ; do \
	  echo $(PY) $(CCPY) $$T; \
	  $(PY) $(CCPY) $$T; \
	done
	
	
clean: extraclean

extraclean: 
	@- $(RM) -f Makefile.coupling toplevel_component.F90
