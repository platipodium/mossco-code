# This Makefile is part of MOSSCO
#
# @copyright (C) 2021-2022 Helmholtz-Zentrum Hereon
# @copyright (C) 2018-2021 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hereon.de>
# @author Richard Hofmeister
# @license Dual licensed under GPL v3+ and Apache 2.0
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# $MOSSCO_DIR/doc/license/LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for
# the full license terms.
#
# In addition to the main part of MOSSCO licensed exclusively under GPL, 
# the coupling to SCHISM is dual-licensed and also available 
# under the Apache 2.0 license, consult the file
# $MOSSCO_DIR/doc/license/LICENSE.APACHE for the full license terms

ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/esmf/schism$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

ifndef SCHISM_BUILD_DIR
$(error SCHISM_BUILD_DIR has to be set in environment)
endif

ifeq ($(wildcard $(SCHISM_BUILD_DIR)/lib/libhydro.a),)
$(error You need to compile schism externally.)
endif

ifndef SCHISM_ESMF_DIR
$(error SCHISM_ESMF_DIR has to be set in environment)
endif

ifeq ($(wildcard $(SCHISM_ESMF_DIR)/src/schism/schism_esmf_cap.F90),)
$(error Cannot find $(SCHISM_ESMF_DIR)/src/schism/schism_esmf_cap.F90)
endif

#ifdef USE_SED
#ifeq ( $(USE_SED),"ON")
DEFINES+= -DUSE_SED
#endif
#endif

ifdef USE_FABM
ifeq ( $(USE_FABM),"ON")
DEFINES+= -DUSE_FABM
endif
endif

# Build library dependencies for core SCHISM, then metis and optionally fabm,
# then add MOSSCO libs depending on installation directory
LIBS+= -L$(SCHISM_BUILD_DIR)/lib -lhydro -lcore
LIBS+= -lparmetis -lmetis

ifeq ($(USE_FABM),"ON")
ifneq ($(wildcard $(SCHISM_BUILD_DIR)/lib/libfabm.a),)
LIBS+= -lfabm_schism -lfabm
F90FLAGS+= "-DUSE_FABM -fallow-argument-mismatch"
$(info Using SCHISM compiled with FABM library)
else
$(info Using SCHISM compiled without FABM library)
endif
endif

# Build library dependencies for core SCHISM, then metis and optionally fabm,
# then add MOSSCO libs depending on installation directory
LIBS+= -L$(SCHISM_BUILD_DIR)/lib -lhydro -lcore
LIBS+= -lparmetis -lmetis
ifdef USE_SED
LIBS+= -lsediment
endif

ifneq ($(wildcard $(MOSSCO_INSTALL_PREFIX)/lib/libmossco.a),)
LIBS+= -L$(MOSSCO_INSTALL_PREFIX)/lib -lmossco 
else
LIBS+= -L$(MOSSCO_DIR)/lib -lmossco 
endif

ifneq ($(wildcard $(MOSSCO_INSTALL_PREFIX)/lib/libmossco_fabm.a),)
LIBS+= -lmossco_fabm
else
ifneq ($(wildcard $(MOSSCO_DIR)/lib/libmossco_fabm.a),)
LIBS+= -lmossco_fabm
endif
endif

F90FLAGS+= -I$(SCHISM_BUILD_DIR)/include
F90FLAGS+= -I$(MOSSCO_DIR)/include
F90FLAGS+= $(DEFINES)
MOSSCO_LDFLAGS += $(MOSSCO_F90LDFLAGS) $(LIBRARY_PATHS) $(LIBS)

.PHONY: all clean distclean config

all:  schism_mossco config

schism_mossco: schism_esmf_cap.o toplevel_component.o main.o
	echo objcopy --redefine-sym xerbla_=schism_lap_xerbla_ ${SCHISM_BUILD_DIR}/lib/libhydro.a
	$(F90) $(CPPFLAGS) $^ -o $@ $(MOSSCO_LDFLAGS) $(LDFLAGS) $(LIBS)

toplevel_component.o: schism_esmf_cap.o toplevel_component.F90

schism_esmf_cap.o: schism_bmi.o schism_esmf_util.o schism_esmf_cap.F90

# Generic and cleanup rules

%.o: %.F90
	$(F90) $(CPPFLAGS) $(F90FLAGS) -c $<

%.mod: %.F90
	$(F90) $(CPPFLAGS) $(F90FLAGS) -c $<

clean:
	@$(RM) *.o *.mod PET* fort.* *output.nc *.vtk total*.dat flux.dat
	@$(RM) -rf *dSYM *.out core.* *.stderr *.stdout fort.* mirror.out
	@$(RM) -f *.xml

config:  grid_input.cfg wind_input.cfg wind.nc schism_ugrid.nc

extraclean: clean
	@$(RM) schism_mossco schism_bmi.F90 schism_esmf_util.F90 schism_esmf_cap.F90 main.F90 wind.nc

# Copy/link files from schism_esmf and mossco systems

main.F90:
	test -f main.F90 | ln -sf $(MOSSCO_DIR)/examples/common/main.F90 .

schism_esmf_util.F90:
	test -f schism_esmf_util.F90 | ln -sf $(SCHISM_ESMF_DIR)/src/schism/schism_esmf_util.F90 .

schism_esmf_cap.F90:
	test -f schism_esmf_cap.F90 | ln -sf $(SCHISM_ESMF_DIR)/src/schism/schism_esmf_cap.F90 .

schism_bmi.F90:
	test -f schism_bmi.F90 | ln -sf $(SCHISM_ESMF_DIR)/src/schism/schism_bmi.F90 .

schism_ugrid.nc: schism_ugrid.cdl
	ncgen -o $@ $<

wind.nc: wind.cdl
	ncgen -o $@ $<
