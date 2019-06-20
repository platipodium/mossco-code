# This Makefile is part of MOSSCO
#
# @copyright (C) 2018, 2019 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
# @author Richard Hofmeister <richard.hofmeister@hzg.de>
# @license Dual licensed under GPL v3+ and Apache 2.0
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
# In addition to the main part of MOSSCO, the coupling to  SCHISM is dual-licensed
# under the Apache 2.0 license, consult the file

ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/esmf/schism$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

# add SCHISM, PARMETIS
ifndef SCHISM_DIR
$(error SCHISM_DIR has to be set in environment)
endif
ifndef PARMETIS_DIR
$(error PARMETIS_DIR has to be set in environment)
endif

ifeq ($(wildcard $(SCHISM_DIR)/lib/libhydro.a),)
$(error You need to compile schism externally.)
endif

#ifndef SCHISM_ESMF_DIR
ifneq ($(wildcard $(SCHISM_DIR)/../esmf/schism_esmf_component.F90),)
export SCHISM_ESMF_DIR=$(SCHISM_DIR)/../esmf
else
ifneq ($(wildcard $(SCHISM_DIR)/../../schism-esmf/schism_esmf_component.F90),)
export SCHISM_ESMF_DIR=$(SCHISM_DIR)/../../schism-esmf
else
$(error Cannot find schism ESMF directory)
endif
endif
#endif

$(info Use as SCHISM ESMF directory $(SCHISM_ESMF_DIR))

ifeq ($(wildcard $(SCHISM_ESMF_DIR)/schism_esmf_component.F90),)
$(error Cannot find schism_esmf_component.)
endif

LIBS+= -L$(SCHISM_DIR)/lib -lhydro -lcore -lfabm_schism -lfabm
LIBS+= -L$(PARMETIS_DIR) -lparmetis -lmetis $(PARMETIS_DIR)/libmetis.a
LIBS+= -L$(MOSSCO_DIR)/lib -lmossco -lmossco_fabm
F90FLAGS+= -DUSE_FABM
F90FLAGS+= -I$(SCHISM_DIR)/include
F90FLAGS+= -I$(MOSSCO_DIR)/include
LDFLAGS += $(MOSSCO_F90LDFLAGS) $(LIBRARY_PATHS) $(LIBS)


.PHONY: all clean distclean config

all:  schism_mossco config outputs

schism_mossco: schism_esmf_component.o toplevel_component.o main.o
	echo objcopy --redefine-sym xerbla_=schism_lap_xerbla_ $SCHISM_DIR/lib/libhydro.a
	$(F90) $(CPPFLAGS) $^ -o $@ $(LDFLAGS) $(LIBS)

toplevel_component.o: schism_esmf_component.o toplevel_component.F90

schism_esmf_component.o: schism_driver_interfaces.o schism_esmf_component.F90

outputs:
	mkdir -p outputs

# Generic and cleanup rules

%.o: %.F90
	$(F90) $(CPPFLAGS) $(F90FLAGS) -c $<

%.mod: %.F90
	$(F90) $(CPPFLAGS) $(F90FLAGS) -c $<

clean:
	@$(RM) *.o *.mod PET* fort.* *output.nc *.vtk total*.dat flux.dat
	@$(RM) -rf *dSYM *.out core.* *.stderr *.stdout fort.* mirror.out
	@$(RM) -f *.xml

config:  grid_input.cfg wind_input.cfg wind.nc

extraclean: clean
	@$(RM) schism_mossco schism_driver_interfaces.F90 schism_esmf_component.F90 main.F90 wind.nc

# Copy/link files from schism_esmf and mossco systems

main.F90:
	test -l main.F90 | ln -sf $(MOSSCO_DIR)/examples/common/main.F90 .

schism_driver_interfaces.F90:
	test -l schism_driver_interfaces.F90 | ln -sf $(SCHISM_ESMF_DIR)/schism_driver_interfaces.F90 .

schism_esmf_component.F90:
	test -l schim_esmf_component.F90 | ln -sf $(SCHISM_ESMF_DIR)/schism_esmf_component.F90 .

wind.nc: wind.cdl
	ncgen -o $@ $<
