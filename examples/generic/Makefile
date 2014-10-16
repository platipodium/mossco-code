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
export MOSSCO_DIR=$(subst /examples/generic$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

YAMLS = $(shell ls *.yaml)
EXAMPLES = ${YAMLS:.yaml=}

.PHONY: all examples exec clean

all: exec

examples:
	set -e; for i in $(YAMLS); do python create_coupling.py $$i && $(MAKE) -f Makefile.coupling; done

Makefile.coupling: create_coupling.py coupling_system.yaml
	python $<

exec: Makefile.coupling
	$(MAKE) -f Makefile.coupling $@

clean: extraclean

extraclean: Makefile.coupling
	$(MAKE) -f Makefile.coupling $@
	$(RM) Makefile.coupling toplevel_component.F90 $(EXAMPLES)

################################################
# Recreate all standalone examples

#	python $<
clm: create_coupling.py clm_coupling.yaml
	python $^

benthos: create_coupling.py benthos_coupling.yaml
	python $^

erosed: create_coupling.py erosed_coupling.yaml
	python $^

fabm0d: create_coupling.py fabm0d_coupling.yaml
	python $^

fabm_sediment: create_coupling.py fabm_sediment_coupling.yaml
	python $^
	
gotm: create_coupling.py gotm_coupling.yaml
	python $^

gotmfabm: create_coupling.py gotmfabm_coupling.yaml
	python $^

getm: create_coupling.py getm_coupling.yaml
	python $^

simplewave: create_coupling.py simplewave_coupling.yaml
	python $^

empty: create_coupling.py empty_coupling.yaml
	python $^

constant: create_coupling.py constant_coupling.yaml
	python $^

	
	
	


