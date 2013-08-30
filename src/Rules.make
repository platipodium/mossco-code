# This Makefile snippet is part of MOSSCO; definition of MOSSCO-wide make rules
# 
# Copyright (C) 2013 Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#


# 1. Checking that we're using GNU make 
#    Of course, this command already requires gmake, so a better solution is required here
ifeq ($(shell make --version | grep -c GNU),0)
$(error GNU make is required)
endif 

# 2. Importing all FABM-related environment variables and checking that the environment is sane
#    At the moment, we require that FABMDIR, FABMHOST, and FORTRAN_COMPILER are set and that
#    the fabm library is installed in the production version (libfabm_prod)
# 
MOSSCO_FABM=false

ifndef FABMDIR
$(error FABMDIR needs to be defined)
endif

#!> @todo remove FABMHOST here and move it to makefiles where FABM is remade
ifndef FABMHOST
export FABMHOST=mossco
$(warning FABMHOST set to FABMHOST=mossco)
endif

ifndef FORTRAN_COMPILER
FABM_AVAILABLE_COMPILERS=$(shell ls -1 $(FABMDIR)/compilers/compiler.* | cut -d'.' -f2)
FABM_AVAILABLE_COMPILERS:=$(patsubst %compiler.,,$(FABM_AVAILABLE_COMPILERS))
$(error FORTRAN_COMPILER needs to be set to one of the compilers in $(FABMDIR)/compilers: $(FABM_AVAILABLE_COMPILERS))
endif

ifeq ($(FABM_F2003),true)
else
$(error Please recompile FABM with FABM_F2003=true)
endif

ifndef FABM_F2003
export FABM_F2003=true
$(warning FABM_F2003 automatically set to FABM_F2003=$(FABM_F2003))
endif

FABM_MODULE_PATH=$(FABMDIR)/modules/$(FABMHOST)/$(FORTRAN_COMPILER)
FABM_INCLUDE_PATH=$(FABMDIR)/include
FABM_LIBRARY_PATH=$(FABMDIR)/lib/$(FABMHOST)/$(FORTRAN_COMPILER)
FABM_LIBRARY_FILE=$(shell ls $(FABM_LIBRARY_PATH) )
MOSSCO_FABM=true

export MOSSCO_FABM

# 3. (optional) Importing all GOTM-related environment variables and checking that the environment is sane
# At the moment, we require that GOTMDIR, FABM, and FORTRAN_COMPILER are set and that
# the gotm library is installed in the production version (libgotm_prod)

MOSSCO_GOTM=false
ifdef GOTMDIR
ifeq ("x$(GOTMDIR)",x) 
$(error the GOTMDIR variable is empty)
endif
ifeq ($(wildcard $(GOTMDIR)),) 
$(error the directory GOTMDIR=$(GOTMDIR) does not exist)
endif

ifndef FABM 
export FABM=true
$(warning FABM automatically set to FABM=$(FABM) for GOTM in $(GOTMDIR))
endif

GOTM_MODULE_PATH=$(GOTMDIR)/modules/$(FORTRAN_COMPILER)
GOTM_INCLUDE_PATH=$(GOTMDIR)/include
GOTM_LIBRARY_PATH=$(GOTMDIR)/lib/$(FORTRAN_COMPILER)
GOTM_LIBRARY_FILE=$(shell ls $(GOTM_LIBRARY_PATH) )
MOSSCO_GOTM=true
endif
export MOSSCO_GOTM

# 4. ESMF stuff, only if ESMFMKFILE is declared.  We need to work on an intelligent system that prevents
#    the components and mediators to be built if ESMF is not found in your system
#
ifndef ESMFMKFILE
ifndef MOSSCO_ESMF
$(error Compiling without ESMF support. Comment this line 90 in Rules.make if you want to proceed)
export MOSSCO_ESMF=false
endif
else
include $(ESMFMKFILE)
export MOSSCO_ESMF=true
ifdef ESMF_DIR
MOSSCO_OS=$(shell $(ESMF_DIR)/scripts/esmf_os)
else
MOSSCO_OS=$(shell uname -s)
endif
ifneq ("x$(ESMF_NETCDF)","x")
export MOSSCO_NETCDF_LIBPATH=$(ESMF_NETCDF_LIBPATH)
endif
export MOSSCO_OS
endif

## 5. DELFT
MOSSCO_DELFT=false
ifdef DELFT_DIR
ifneq ("x$(DELFT_DIR)",x)
ifneq ($(wildcard $(DELFT_DIR)),) 
MOSSCO_DELFT=true
endif
endif
endif

# 6. MOSSCO declarations. The MOSSCO_DIR and the build prefix are set, as well as the bin/mod/lib paths relative
#    to the PREFIX
#
ifndef MOSSCO_DIR
ifdef MOSSCODIR
MOSSCO_DIR=$(MOSSCODIR)
else
MOSSCO_DIR=$(subst /src$,,$(PWD))
endif
endif
export MOSSCO_DIR

ifeq ($(wildcard $(MOSSCO_DIR)),) 
$(error the directory MOSSCO_DIR=$(MOSSCO_DIR) does not exist)
endif

ifdef PREFIX
MOSSCO_PREFIX=$(PREFIX)
else
MOSSCO_PREFIX=$(MOSSCO_DIR)
endif
export MOSSCO_PREFIX

export MOSSCO_MODULE_PATH=$(MOSSCO_PREFIX)/modules/$(FORTRAN_COMPILER)
export MOSSCO_LIBRARY_PATH=$(MOSSCO_PREFIX)/lib/$(FORTRAN_COMPILER)
export MOSSCO_BIN_PATH=$(MOSSCO_PREFIX)/bin

# 4. Putting everything together.  This section could need some cleanup, but does work for now
#

# determine the compiler used by FABM
FABM_F90COMPILER=$(shell grep 'FC=' $(FABMDIR)/compilers/compiler.$(FORTRAN_COMPILER) | cut -d"=" -f2)

ifndef F90
ifdef ESMF_F90COMPILER
F90=$(ESMF_F90COMPILER)
$(warning F90 automatically determined from ESMF_F90COMPILER variable: F90=$(F90))
else
F90=$(shell grep 'FC=' $(FABMDIR)/compilers/compiler.$(FORTRAN_COMPILER) | cut -d"=" -f2)
$(warning F90 automatically determined from FABM environment: F90=$(F90))
endif
endif

ifneq ($(F90),$(FABM_F90COMPILER))
ifndef MOSSCO_COMPILER
$(warning F90=$(F90) different from compiler used by FABM ($(FABM_F90COMPILER)))
endif
endif
export MOSSCO_COMPILER=$(F90)
export F90

ifeq ($(MOSSCO_FABM),true)
INCLUDES  = -I$(FABM_INCLUDE_PATH) -I$(FABM_MODULE_PATH) -I$(FABMDIR)/src/drivers/$(FABMHOST)
endif
INCLUDES += $(ESMF_F90COMPILEPATHS)
INCLUDES += -I$(MOSSCO_MODULE_PATH)
ifeq ($(MOSSCO_GOTM),true)
INCLUDES += -I$(GOTM_MODULE_PATH)
endif


#!> @todo expand existing F90FLAGS var but check for not duplicating the -J entry
ifeq ($(FORTRAN_COMPILER),GFORTRAN)
F90FLAGS = -J$(MOSSCO_MODULE_PATH)
EXTRA_CPP=
else
ifeq ($(FORTRAN_COMPILER),IFORT)
F90FLAGS = -module $(MOSSCO_MODULE_PATH)
EXTRA_CPP= -stand f03
endif
endif
export F90FLAGS

ifndef HAVE_LD_FORCE_LOAD
HAVE_LD_FORCE_LOAD=$(shell ld -v 2>&1 | grep -c LLVM)
ifeq ($(HAVE_LD_FORCE_LOAD),1)
HAVE_LD_FORCE_LOAD=true
else
HAVE_LD_FORCE_LOAD=false
endif
export HAVE_LD_FORCE_LOAD
endif 

LIBRARY_PATHS  = -L$(FABM_LIBRARY_PATH) 
LIBRARY_PATHS += $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) 
LIBRARY_PATHS += -L$(MOSSCO_LIBRARY_PATH)
ifneq ($(MOSSCO_NETCDF_LIBPATH),)
LIBRARY_PATHS += -L$(MOSSCO_NETCDF_LIBPATH)
endif
export LIBRARY_PATHS

LIBS = -lfabm_prod
LIBS += $(ESMF_F90LINKLIBS)
LIBS += $(MOSSCO_NETCDF_LIBS)
export LIBS

export CPPFLAGS = $(DEFINES) $(EXTRA_CPP) $(INCLUDES) $(ESMF_F90COMPILECPPFLAGS) -I.

export LDFLAGS = $(ESMF_F90LINKOPTS)

# Make targets
.PHONY: default all clean doc info prefix

default: prefix all

clean:
	@rm -f *.o *.mod

distclean: clean
	@rm -f *.swp

prefix:
	@mkdir -p $(MOSSCO_LIBRARY_PATH)
	@mkdir -p $(MOSSCO_MODULE_PATH)
	@mkdir -p $(MOSSCO_BIN_PATH)

info:
	@echo SHELL = $(SHELL)
	@echo MAKE = $(MAKE)
	@echo FORTRAN_COMPILER = $(FORTRAN_COMPILER)
	@echo F90 = $(F90)
	@echo HAVE_LD_FORCE_LOAD = $(HAVE_LD_FORCE_LOAD)
	@echo FABMHOST = $(FABMHOST)
	@echo FABMDIR = $(FABMDIR)
	@echo INCDIRS = $(INCDIRS)
	@echo F90FLAGS = $(F90FLAGS)
	@echo CPPFLAGS = $(CPPFLAGS)
	@echo LDFLAGS = $(LDFLAGS)
	@echo LINKDIRS = $(LINKDIRS)
	@env | grep MOSSCO_ | sort 

# Common rules
#ifndef EXTRA_CPP

%.o: %.F90
	@echo "Compiling $<"
	$(F90) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@	
%.o: %.f90
	@echo "Compiling $<"
	$(F90) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@
%.mod: %.f90
	@echo "Compiling $<"
	$(F90) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@
#%.o: %.f90
#	@echo "Compiling $<"
#	$(F90) $(CPPFLAGS)  -c $< -o $@
#else
#%.f90: %.F90
#	$(CPP) $(CPPFLAGS) $< -o $@
#	$(F90_to_f90)
#%.o: %.f90
#	$(F90) $(F90FLAGS) $(EXTRA_FFLAGS) -c $< -o $@
#endif
