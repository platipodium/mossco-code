# This Makefile is part of MOSSCO
#
# @copyright (C) 2014, 2015, 2016 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/generic$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

YAMLS = $(shell ls *.yaml)
EXAMPLES_ALL = ${YAMLS:.yaml=}
EXAMPLES = $(shell cat EXAMPLES)

.PHONY: all examples exec clean

all: exec

examples:
	set -e; for i in $(EXAMPLES); do python create_coupling.py $$i.yaml && $(MAKE) -f Makefile.coupling; done

examples_all:
	set -e; for i in $(EXAMPLES_ALL); do python create_coupling.py $$i.yaml && $(MAKE) -f Makefile.coupling; done

Makefile.coupling: create_coupling.py
	python $<

exec: Makefile.coupling
	$(MAKE) -f Makefile.coupling $@

clean: extraclean

links: $(shell ls *--*.yaml)
	python create_short_examples.py

extraclean: Makefile.coupling
	$(MAKE) -f Makefile.coupling $@
	$(RM) Makefile.coupling toplevel_component.F90 toplevel_component.o $(EXAMPLES)
