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

.PHONY: all exec clean

all: Makefile.coupling toplevel_coupling.F90 exec

toplevel_coupling.F90 Makefile.coupling: create_coupling.py coupling_system.yaml
	python $<

exec: Makefile.coupling
	$(MAKE) -f Makefile.coupling all

clean: extraclean

extraclean: Makefile.coupling
	$(MAKE) -f makefile.coupling extraclean

################################################
# @todo recreate all standalone examples

#clm	fabm0d gotmfabm simplewave empty getm remtc benthos	erosed gotm	sediment: create_coupling.py #$@_coupling.yaml
#	python $<
clm: create_coupling.py clm_coupling.yaml
	python $^
	
	
	


