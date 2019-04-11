# This Makefile is part of MOSSCO
#
# @copyright (C)2019 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/esmf/particle$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS :=  -lmossco_particle -lmossco_util -lesmf $(LIBS)
LDFLAGS += $(MOSSCO_F90LDFLAGS)

.PHONY: all exec particle

all: exec
exec: libmossco_particle particle particle_scrip.nc

particle:
	mossco -rb particle
	@echo "Created example $(CURDIR)/$@"

libmossco_particle:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

particle_scrip.nc:
	ncgen -7 -b particle_scrip.cdl

particle_ugrid.nc: # currently not used
	ncgen -7 -b particle_ugrid.cdl

clean: extraclean

extraclean:
	@rm -rf particle particle.dSYM *.o PET* *nc *vtk
