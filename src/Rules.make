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

# More useful output from Make and count of iterations of Rules.make
#OLD_SHELL := $(SHELL)
#SHELL = $(warning Building $@$(if $<, (from $<))$(if $?, ($? newer)))$(OLD_SHELL)

MOSSCO_INSTALL_PREFIX ?= /opt/mossco


# Filter out all MAKELEVELS that are not 1 or 0 to avoid unneccessary execution
# of the Preamlbe section of this Rules.make in repeated calls.  In most circumstances,
# Rules.make is executed at MAKELEVEL 1, unless directly called in $(MOSSCODIR)/src
#ifneq (,$(filter $(MAKELEVEL),0 1))
ifeq (1,1)
# 2. Importing all FABM-related environment variables and checking that the environment is sane
#    At the moment, we require that MOSSCO_FABMDIR and FORTRAN_COMPILER are set and that
#    the fabm library is installed in the production version (libfabm_prod)
# 
MOSSCO_FABM=false

ifndef MOSSCO_FABMDIR
external_FABMDIR = $(MOSSCO_DIR)/external/fabm-git
ifneq ($(wildcard $(external_FABMDIR)/src/Makefile),)
export MOSSCO_FABMDIR=$(external_FABMDIR)
endif
endif

ifdef MOSSCO_FABMDIR
export FABMDIR=$(MOSSCO_FABMDIR)
MOSSCO_FABM=true
else
ifdef FABMDIR
MOSSCO_FABM=true
$(warning Assuming you have a working FABM in ${FABMDIR}, proceed at your own risk or set the environment variable $$MOSSCO_FABMDIR explicitly to enable the build system to take  care of the FABM build) 
endif
endif

export MOSSCO_FABM

ifeq ($(MOSSCO_FABM),true)

#!> @todo remove FABMHOST here and move it to makefiles where FABM is remade
ifdef FABMHOST
ifneq ($(FABMHOST),mossco)
$(warning FABMHOST changed from $(FABMHOST) to mossco)
endif
endif
export FABMHOST=mossco

ifndef FORTRAN_COMPILER
FABM_AVAILABLE_COMPILERS=$(shell ls -1 $(FABMDIR)/compilers/compiler.* | cut -d'.' -f2)
FABM_AVAILABLE_COMPILERS:=$(patsubst %compiler.,,$(FABM_AVAILABLE_COMPILERS))
$(error FORTRAN_COMPILER needs to be set to one of the compilers in $(FABMDIR)/compilers: $(FABM_AVAILABLE_COMPILERS))
endif

FABM_MODULE_PATH=$(FABMDIR)/modules/$(FABMHOST)/$(FORTRAN_COMPILER)
FABM_INCLUDE_PATH=$(FABMDIR)/include
FABM_LIBRARY_PATH=$(FABMDIR)/lib/$(FABMHOST)/$(FORTRAN_COMPILER)

endif

# 3. (optional) Importing all GOTM-related environment variables and checking that the environment is sane
# At the moment, we require that GOTMDIR, FABM, and FORTRAN_COMPILER are set and that
# the gotm library is installed in the production version (libgotm_prod)

MOSSCO_GOTM=false

ifndef MOSSCO_GOTMDIR
external_GOTMDIR = $(MOSSCO_DIR)/external/gotm-git
ifneq ($(wildcard $(external_GOTMDIR)/src/Makefile),)
export MOSSCO_GOTMDIR=$(external_GOTMDIR)
endif
endif

ifdef MOSSCO_GOTMDIR
export GOTMDIR=$(MOSSCO_GOTMDIR)
MOSSCO_GOTM=true
else
ifdef GOTMDIR
MOSSCO_GOTM=true
$(warning Assuming you have a working GOTM in ${GOTMDIR}, proceed at your own risk or set the environment variable $$MOSSCO_GOTMDIR explicitly to enable the build system to take  care of the GOTM build) 
endif
endif

ifdef GOTMDIR
MOSSCO_GOTM=true
endif

export MOSSCO_GOTM

ifeq ($(MOSSCO_GOTM),true)

GOTM_MODULE_PATH=$(GOTMDIR)/modules/$(FORTRAN_COMPILER)
GOTM_INCLUDE_PATH=$(GOTMDIR)/include
GOTM_LIBRARY_PATH=$(GOTMDIR)/lib/$(FORTRAN_COMPILER)
ifeq ($(MOSSCO_FABM),true)
DEFINES += -D_GOTM_MOSSCO_FABM_
MOSSCO_GOTM_FABM=true
endif
endif

### GETM

MOSSCO_GETM=false

ifndef MOSSCO_GETMDIR
external_GETMDIR = $(MOSSCO_DIR)/external/getm-git
ifneq ($(wildcard $(external_GETMDIR)/src/Makefile),)
export MOSSCO_GETMDIR=$(external_GETMDIR)
endif
endif

ifdef MOSSCO_GETMDIR
export GETMDIR=$(MOSSCO_GETMDIR)
MOSSCO_GETM=true
else
ifdef GETMDIR
MOSSCO_GETM=true
$(warning Assuming you have a working GETM in ${GETMDIR}, proceed at your own risk or set the environment variable $$MOSSCO_GETMDIR explicitly to enable the build system to take  care of the GETM build) 
endif
endif

ifdef GETMDIR
MOSSCO_GETM=true
endif

export MOSSCO_GETM

ifeq ($(MOSSCO_GETM),true)
GETM_MODULE_PATH=$(GETMDIR)/modules/$(FORTRAN_COMPILER)
GETM_INCLUDE_PATH=$(GETMDIR)/include
GETM_LIBRARY_PATH=$(GETMDIR)/lib/$(FORTRAN_COMPILER)
GETM_LINKDIRS = -L$(GETM_LIBRARY_PATH) -L$(GOTM_LIBRARY_PATH)
GETM_LIBS = -lgetm_prod	-loutput_prod -lmeteo_prod
ifneq ($(GETM_NO_3D),true)
GETM_LIBS += -l3d_prod
endif
GETM_LIBS += -l2d_prod -ldomain_prod -linput_prod -lncdfio_prod -lfutils_prod
ifeq ($(MOSSCO_GETM_FABM),true)
GETM_LINKDIRS += -L$(FABMDIR)/lib/gotm/$(FORTRAN_COMPILER)
GETM_LIBS += -lgotm_fabm_prod -lfabm_prod
endif
GETM_LIBS += -lturbulence_prod -lutil_prod
endif


# 4. ESMF stuff, only if ESMFMKFILE is declared.  We need to work on an intelligent system that prevents
#    the components and mediators to be built if ESMF is not found in your system
#
ifndef ESMFMKFILE
ifndef MOSSCO_ESMF
$(error Compiling without ESMF support. Comment this line in Rules.make if you want to proceed)
$(warning Compiling without ESMF support. Finding of compilers/libraries that are usually set up by ESMF might fail.)
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

## 5. EROSED
MOSSCO_EROSED=false

ifndef EROSED_DIR
external_EROSED_DIR = $(MOSSCO_DIR)/external/erosed-svn
ifneq ($(wildcard $(external_EROSED_DIR)),)
export EROSED_DIR=$(external_EROSED_DIR)
endif
endif

ifdef EROSED_DIR
MOSSCO_EROSED=true
endif

export MOSSCO_EROSED

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

# 7. Putting everything together.  This section could need some cleanup, but does work for now
#

# determine the compiler used by FABM
ifeq (${MOSSCO_FABM},true)
FABM_F90COMPILER=$(shell grep 'FC=' $(FABMDIR)/compilers/compiler.$(FORTRAN_COMPILER) | cut -d"=" -f2)
FABM_F90COMPILER_VERSION:=$(shell $(FABM_F90COMPILER) --version | head -1)
endif

ifndef F90
ifdef ESMF_F90COMPILER
export F90=$(ESMF_F90COMPILER)
F90_VERSION:=$(shell $(F90) --version | head -1)
else
ifeq ($MOSSCO_FABM,true)
export F90=$(shell grep 'FC=' $(FABMDIR)/compilers/compiler.$(FORTRAN_COMPILER) | cut -d"=" -f2)
F90_VERSION:=$(shell $(F90) --version | head -1)
$(warning F90 automatically determined from FABM environment: F90=$(F90))
else
ifeq ($MOSSCO_GOTM,true)
export F90=$(shell grep 'FC=' $(GOTMDIR)/compilers/compiler.$(FORTRAN_COMPILER) | cut -d"=" -f2)
F90_VERSION:=$(shell $(F90) --version | head -1)
$(warning F90 automatically determined from GOTM environment: F90=$(F90))
endif
endif
endif
endif

ifneq ($(F90_VERSION),$(FABM_F90COMPILER_VERSION))
MPICH_F90COMPILER=$(shell $(F90) -compile_info 2> /dev/null | cut -d' ' -f1)
#MPICH_F90COMPILER_VERSION:=$(shell $(MPICH_F90COMPILER) --version | head -1)
ifneq ($(MPICH_F90COMPILER_VERSION),$(FABM_F90COMPILER_VERSION))
ifndef MOSSCO_COMPILER
$(warning F90=$(F90) different from compiler used by FABM ($(FABM_F90COMPILER)))
endif
endif
endif
export MOSSCO_COMPILER=$(F90)
export F90
export F90_VERSION
export FABM_F90COMPILER_VERSION
export MPICH_F90COMPILER_VERSION

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
else
ifeq ($(FORTRAN_COMPILER),PGF90)
F90FLAGS = -module $(MOSSCO_MODULE_PATH)
EXTRA_CPP=
else
$(error I don't know where to place modules with this compiler)
endif
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

LIBRARY_PATHS += $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) 
LIBRARY_PATHS += -L$(MOSSCO_LIBRARY_PATH)
ifneq ($(MOSSCO_NETCDF_LIBPATH),)
LIBRARY_PATHS += -L$(MOSSCO_NETCDF_LIBPATH)
endif
export LIBRARY_PATHS

LIBS += $(ESMF_F90LINKLIBS)
LIBS += $(MOSSCO_NETCDF_LIBS)
export LIBS

CPPFLAGS = $(DEFINES)  -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR)
export CPPFLAGS += $(EXTRA_CPP) $(INCLUDES) $(ESMF_F90COMPILECPPFLAGS) -I.

LDFLAGS += $(ESMF_F90LINKOPTS)
LDFLAGS += $(LIBRARY_PATHS)
#export LDFLAGS
endif

# Make targets
.PHONY: default all clean doc info prefix libfabm_external libgotm_external libgetm_external

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
	@echo HAVE_LD_FORCE_LOAD = $(HAVE_LD_FORCE_LOAD)
	@echo INCDIRS = $(INCDIRS)
	@echo CPPFLAGS = $(CPPFLAGS)
	@echo LDFLAGS = $(LDFLAGS)
	@echo LIBS = $(LIBS)
	@echo LINKDIRS = $(LINKDIRS)
	@echo FORTRAN_COMPILER = $(FORTRAN_COMPILER)
	@env | grep ^F90 | sort 
ifeq ($(MOSSCO_FABM),true)
	@env | grep ^FABM | sort 
endif
ifeq ($(MOSSCO_GOTM),true)
	@env | grep ^GOTM | sort 
endif
ifeq ($(MOSSCO_GETM),true)
	@env | grep ^GETM | sort 
endif
	@env | grep ^MOSSCO_ | sort 


# External libraries

libfabm_external: 
ifdef MOSSCO_FABMDIR
	@echo Recreating the FABM library in $(FABM_LIBRARY_PATH)
	$(MAKE) -C $(FABMDIR)/src
endif

# KK-TODO: think about compiling gotm without updating its exe
libgotm_external:
ifdef MOSSCO_GOTMDIR
ifeq ($(MOSSCO_GOTM_FABM),true)
	@echo Recreating the GOTM library without FABM in $(GOTM_LIBRARY_PATH)
	(unset FABM ; $(MAKE) -C $(GOTMDIR)/src)
endif
endif

libgetm_external: prefix
ifdef MOSSCO_GETMDIR
ifeq ($(MOSSCO_GETM_FABM),true)
ifdef MOSSCO_FABMDIR
	@echo Recreating the FABM library in $(FABMDIR)/lib/gotm/$(FORTRAN_COMPILER)
	$(MAKE) -C $(FABMDIR)/src gotm
endif
ifdef MOSSCO_GOTMDIR
	@echo Recreating the GOTM library in $(GOTM_LIBRARY_PATH)
	(export FABM=true ; $(MAKE) -C $(GOTMDIR)/src)
endif
	@echo Recreating the GETM library in $(GETM_LIBRARY_PATH)
	(export FABM=true ; $(MAKE) -C $(GETMDIR)/src)
else
ifdef MOSSCO_GOTMDIR
	@echo Recreating the GOTM library without FABM in $(GOTM_LIBRARY_PATH)
	(unset FABM ; $(MAKE) -C $(GOTMDIR)/src)
endif
	@echo Recreating the GETM library without FABM in $(GETM_LIBRARY_PATH)
	(unset FABM ; $(MAKE) -C $(GETMDIR)/src)
endif
endif
	#$(AR) Trus $(MOSSCO_LIBRARY_PATH)/libgetm_external.a $(GETM_LIBRARY_PATH)/lib*_prod.a
	
	
#install:
#	@test -d  $(MOSSCO_INSTALL_PREFIX) || mkdir -p $(MOSSCO_INSTALL_PREFIX) || $(warning No permission to create #$(MOSSCO_INSTALL_PREFIX))
#	@mkdir -p $(MOSSCO_INSTALL_PREFIX)/lib
#	@mkdir -p $(MOSSCO_INSTALL_PREFIX)/include
#	@cp $(MOSSCO_LIBRARY_PATH)/*.*  $(MOSSCO_INSTALL_PREFIX)/lib
#	@cp $(MOSSCO_MODULE_PATH)/*.mod  $(MOSSCO_INSTALL_PREFIX)/include

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

help:
	@if [ -f README ] ; then cat README ; fi
