# This Makefile snippet is part of MOSSCO; definition of MOSSCO-wide make rules
# 
# Copyright (C) 2013, 2014 Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#

# 0. Execute the preamble only if we are calling Rules make for the first time
# this is detected by the presence of the variabl MOSSCO_PREFIX
# All variables that should be passed to submakes need to be exported, 
# including all variables that appear in the rules at the end of this file

ifndef MOSSCO_PREFIX

# 1. Checking that we're using GNU make 
#    Of course, this command already requires gmake, so a better solution is required here
ifeq ($(shell make --version | grep -c GNU),0)
  $(error GNU make is required)
endif 

MOSSCO_INSTALL_PREFIX ?= /opt/mossco

# Filter out all MAKELEVELS that are not 1 or 0 to avoid unneccessary execution
# of the preamble section of this Rules.make in repeated calls.  In most circumstances,
# Rules.make is executed at MAKELEVEL 1, unless directly called in $(MOSSCODIR)/src
#ifneq (,$(filter $(MAKELEVEL),0 1))

# 2. ESMF stuff, only if ESMFMKFILE is declared. 
#
ifndef ESMFMKFILE
  FORTRAN_COMPILER ?= $(shell echo $(F90) | tr a-z A-Z)
  FORTRAN_COMPILER ?= $(shell echo $(FC) | tr a-z A-Z)
  ifeq ("$(FORTRAN_COMPILER)","F77")
    $(error MOSSCO needs a F2003 fortran compiler, your environment says $$FC=$(FC))
  endif
  #$(error Compiling without ESMF support. Comment this line in Rules.make if you want to proceed at your own risk)
  MOSSCO_ESMF=false
else
  ifeq ($(wildcard $(ESMFMKFILE)),)
    $(error The file you specified as ESMFMKFILE=$(ESMFMKFILE) does not exist)
  endif
  include $(ESMFMKFILE)
  MOSSCO_ESMF=true
  ESMF_COMM = $(strip $(shell grep "\# ESMF_COMM:" $(ESMFMKFILE) | cut -d':' -f2-))
  ifeq ("$(ESMF_COMM)","mpiuni")
    export MOSSCO_MPI ?= false
  else
    export MOSSCO_MPI ?= true
    ifeq ($(ESMF_COMM),openmpi)
      ESMF_FC:=$(shell $(ESMF_F90COMPILER) --showme:command 2> /dev/null)
      ifeq ($(ESMF_FC),)
        $(error $(ESMF_F90COMPILER) is *not* based on $(ESMF_COMM)!)
      endif
    endif
    ifeq ($(ESMF_COMM),mpich2)
      ESMF_FC:=$(shell $(ESMF_F90COMPILER) -compile_info 2> /dev/null | cut -d' ' -f1 | cut -d'-' -f1)
      ifeq ($(ESMF_FC),)
        $(error $(ESMF_F90COMPILER) is *not* based on $(ESMF_COMM)!)
      endif
    endif
    ifeq ($(ESMF_FC),)
      ESMF_FC:=$(shell $(ESMF_F90COMPILER) -compile_info 2> /dev/null | cut -d' ' -f1 | cut -d'-' -f1)
    endif
  endif
  ESMF_NETCDF = $(strip $(shell grep "\# ESMF_NETCDF:" $(ESMFMKFILE) | cut -d':' -f2-))
  ifneq ("$(ESMF_NETCDF)","")
    export MOSSCO_NETCDF ?= true
  else
    export MOSSCO_NETCDF ?= false
  endif
  ifdef ESMF_F90COMPILER
    export MOSSCO_F03COMPILER=$(ESMF_F90COMPILER)
    export F90 = $(ESMF_F90COMPILER)
    export FC  = $(ESMF_F90COMPILER)
    export F77 = $(ESMF_F77COMPILER)
    ifeq ($(ESMF_FC),)
      ESMF_FC:=$(ESMF_F90COMPILER)
    endif
    ESMF_FORTRAN_COMPILER = $(shell echo $(notdir $(ESMF_FC)) | tr a-z A-Z | cut -d"-" -f1)
    ifdef FORTRAN_COMPILER
      ifneq ("$(ESMF_FORTRAN_COMPILER)","$(FORTRAN_COMPILER)")
        $(warning Overwriting FORTRAN_COMPILER=$(FORTRAN_COMPILER) with $(ESMF_FORTRAN_COMPILER))
      endif
    endif
    export FORTRAN_COMPILER = $(ESMF_FORTRAN_COMPILER)
    MOSSCO_F03VERSION=$(shell $(F90) --version | head -1)
  endif
endif
export MOSSCO_ESMF

ifeq ($(MOSSCO_ESMF),true)
  ifeq ($(ESMF_FC),)
    $(error  Your compiler $(F90)'s version could not be determined ($(MOSSCO_F03VERSION)))
  endif
endif

# 3. Checking for the either FABM, GOTM, or GETM.  Set the MOSSCO_XXXX variables
#    of these three components to process them later
MOSSCO_FABM=false

#Note (KK): undefine does not work for gnu make <3.8.2
FABM_PREFIX=

ifdef MOSSCO_FABM_PREFIX
  export FABM_PREFIX=$(MOSSCO_FABM_PREFIX)
endif

ifdef MOSSCO_FABM_BINARY_DIR
  export FABM_BINARY_DIR=$(MOSSCO_FABM_BINARY_DIR)
  export FABM_PREFIX=$(shell grep CMAKE_INSTALL_PREFIX $(MOSSCO_FABM_BINARY_DIR)/CMakeCache.txt | cut -d "=" -f2)
  #export FABMDIR=$(shell grep fabm_SOURCE_DIR $(MOSSCO_FABM_BINARY_DIR)/CMakeCache.txt | cut -d "=" -f2)
endif

ifeq ($(FABM_PREFIX),)
  ifndef MOSSCO_FABMDIR
    external_FABMDIR = $(MOSSCO_DIR)/external/fabm-git
    ifneq ($(wildcard $(external_FABMDIR)/src/Makefile),)
      export MOSSCO_FABMDIR=$(external_FABMDIR)
    endif
  endif
  ifdef MOSSCO_FABMDIR
    export FABMDIR=$(MOSSCO_FABMDIR)
  endif
  ifdef FABMDIR
    export FABM_BINARY_DIR=$(MOSSCO_DIR)/external/fabm-build
    export FABM_PREFIX=$(MOSSCO_DIR)/external/fabm-install
  endif
endif

ifneq ($(FABM_PREFIX),)
  MOSSCO_FABM=true
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
endif

# 3b. GOTM
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

# 3c. GETM
MOSSCO_GETM=false

ifndef MOSSCO_GETMDIR
  external_GETMDIR = $(MOSSCO_DIR)/external/getm-git
  ifneq ($(wildcard $(external_GETMDIR)/src/Makefile),)
    export MOSSCO_GETMDIR=$(external_GETMDIR)
  endif
endif

ifdef MOSSCO_GETMDIR
  export GETMDIR=$(MOSSCO_GETMDIR)
else
  ifdef GETMDIR
    $(warning Assuming you have a working GETM in ${GETMDIR}, proceed at your own risk or set the environment variable $$MOSSCO_GETMDIR explicitly to enable the build system to take  care of the GETM build)
  endif
endif

ifdef GETMDIR
  MOSSCO_GETM=true
  ifdef MOSSCO_GETMDIR
    # We have full control over GETM compilation
    ifeq ($(MOSSCO_MPI),true)
      export GETM_PARALLEL?=true
      ifeq ($(ESMF_COMM),openmpi)
        export MPI=OPENMPI
      else
        export MPI=MPICH2
      endif
    else
      export GETM_PARALLEL=false
    endif
  endif
endif


export MOSSCO_GETM

# 4. Dealing with compiler matching of ESMF and FABM/GOTM/GETM, if one of those
# is found, we require  that FORTRAN_COMPILER is set and that
# the libraries are installed in the production version (libfabm_prod)
# @todo adjust this generic for FABM/GOTM/GETM

ifneq (,$(filter $(MOSSCO_GOTM),$(MOSSCO_FABM),$(MOSSCO_GETM) true))
  ifndef FORTRAN_COMPILER
    FABM_AVAILABLE_COMPILERS=$(shell ls -1 $(FABMDIR)/compilers/compiler.* | cut -d'.' -f2)
    FABM_AVAILABLE_COMPILERS:=$(patsubst %compiler.,,$(FABM_AVAILABLE_COMPILERS))
    $(error FORTRAN_COMPILER needs to be set to one of the compilers in $(FABMDIR)/compilers: $(FABM_AVAILABLE_COMPILERS))
  endif
endif

ifeq ($(MOSSCO_FABM),true)
  export FABM_MODULE_PATH=$(FABM_PREFIX)/include
  export FABM_INCLUDE_PATH=$(FABM_PREFIX)/include
  export FABM_LIBRARY_PATH=$(FABM_PREFIX)/lib
  export FABM_LIBS=-lfabm
endif

ifeq ($(MOSSCO_GOTM),true)
  export GOTM_MODULE_PATH=$(GOTMDIR)/modules/$(FORTRAN_COMPILER)
  export GOTM_INCLUDE_PATH=$(GOTMDIR)/include
  export GOTM_LIBRARY_PATH=$(GOTMDIR)/lib/$(FORTRAN_COMPILER)
  GOTM_LIBS:=-lgotm_prod -lairsea_prod -lmeanflow_prod -lseagrass_prod -loutput_prod
  GOTM_LIBS+=-lobservations_prod -linput_prod -lturbulence_prod -lutil_prod
  export GOTM_LIBS
  export GOTM_LDFLAGS = -L$(GOTM_LIBRARY_PATH) $(GOTM_LIBS)
  ifeq ($(MOSSCO_FABM),true)
    DEFINES += -D_GOTM_MOSSCO_FABM_
    export MOSSCO_GOTM_FABM=true
  endif
endif

ifeq ($(MOSSCO_GETM),true)
  GETM_LIBRARY_PATH=$(GETMDIR)/lib/$(FORTRAN_COMPILER)
  GETM_LINKDIRS = -L$(GETM_LIBRARY_PATH) -L$(GOTM_LIBRARY_PATH)
  GETM_LIBS := -lgetm_prod  -loutput_prod -lmeteo_prod
  ifneq ($(GETM_NO_3D),true)
    GETM_LIBS += -l3d_prod
  endif
  GETM_LIBS += -l2d_prod -ldomain_prod -linput_prod -lncdfio_prod -lfutils_prod
  ifeq ($(MOSSCO_GETM_FABM),true)
    GETM_LINKDIRS += -L$(FABMDIR)/lib/gotm/$(FORTRAN_COMPILER)
    GETM_LIBS += -lgotm_fabm_prod $(FABM_LIBS)
  endif
  GETM_LIBS += -lturbulence_prod -lutil_prod

  ifeq ($(GETM_PARALLEL),true) # Compile for parallel execution
    DEFINES += -DGETM_PARALLEL
  endif
  export GETM_LIBRARY_PATH
  export GETM_CPPFLAGS = -I$(GETMDIR)/include -I$(GETMDIR)/modules/$(FORTRAN_COMPILER)
  export GETM_LDFLAGS = $(GETM_LINKDIRS) $(GETM_LIBS)
endif
export MOSSCO_GETM

# 5. CLM stuff, this is relevant since you need to store 7 GB of data for each year and might not have access to the data
MOSSCO_CLM=false
ifdef CLMDIR
  ifneq ($(wildcard $(CLMDIR)),)
    MOSSCO_CLM=true
  else
    $(error You specified none or an invalid path for CLMDIR=$CLMDIR)
  endif
endif
export MOSSCO_CLM

## 6. EROSED
MOSSCO_EROSED=false

ifndef EROSED_DIR
  external_EROSED_DIR = $(MOSSCO_DIR)/external/erosed-svn
  ifneq ($(wildcard $(external_EROSED_DIR)),)
    EROSED_DIR=$(external_EROSED_DIR)
  endif
  export EROSED_DIR
endif

ifdef EROSED_DIR
  MOSSCO_EROSED=true
  DEFINES += -DMOSSCO_EROSED
endif
export MOSSCO_EROSED

# 7. MOSSCO declarations. The MOSSCO_DIR and the build prefix are set, as well as the bin/mod/lib paths relative
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

MOSSCO_PREFIX=$(MOSSCO_DIR)
ifdef PREFIX
  ifneq ($(wildcard $(PREFIX)),)
    MOSSCO_PREFIX=$(PREFIX)
  endif
endif
export MOSSCO_PREFIX

export MOSSCO_MODULE_PATH=$(MOSSCO_PREFIX)/modules/$(FORTRAN_COMPILER)
export MOSSCO_LIBRARY_PATH=$(MOSSCO_PREFIX)/lib/$(FORTRAN_COMPILER)
export MOSSCO_BIN_PATH=$(MOSSCO_PREFIX)/bin

# 7. Putting everything together. 
# This is the list of ESMF-supported compilers:
# absoft absoftintel cce default g95 gfortran gfortranclang intel intelcl intelgcc
# lahey nag nagintel pathscale pgi pgigcc sxcross xlf xlfgcc

# determine the compiler used by FABM/GOTM/GETM
ifdef FORTRAN_COMPILER

  ifeq (${FORTRAN_COMPILER},PGF90)
    # pgfortran 14.1-0 has a problem compiling benthos_component.F90 (UK 22.05.2014)
    # see $MOSSCO_DIR/src/components/Makefile
    export MOSSCO_BENTHOS=false
  else
    export MOSSCO_BENTHOS=true
  endif

  ifeq (${MOSSCO_GETM},true)
    GETM_F90COMPILER=$(shell grep 'FC=' $(GETMDIR)/compilers/compiler.$(FORTRAN_COMPILER) | cut -d"=" -f2)
    GETM_F90COMPILER_VERSION:=$(shell $(GETM_F90COMPILER) --version | head -1)
    ifndef F90
      export F90=$(GETM_F90COMPILER)
      $(warning F90 automatically determined from GETM environment: F90=$(F90))
    endif
  endif

  ifeq (${MOSSCO_GOTM},true)
    GOTM_F90COMPILER=$(shell grep 'FC=' $(GOTMDIR)/compilers/compiler.$(FORTRAN_COMPILER) | cut -d"=" -f2)
    GOTM_F90COMPILER_VERSION:=$(shell $(GOTM_F90COMPILER) --version | head -1)
    ifndef F90
      export F90=$(GOTM_F90COMPILER)
      $(warning F90 automatically determined from GOTM environment: F90=$(F90))
    endif
  endif

  export F90 ?= $(shell echo $(FORTRAN_COMPILER) | tr A-Z a-z)

endif

# Include directories
INCLUDES += $(ESMF_F90COMPILEPATHS)
INCLUDES += -I$(MOSSCO_MODULE_PATH)
INCLUDES += -I$(MOSSCO_DIR)/src/include
ifeq (${MOSSCO_FABM},true)
INCLUDES  += -I$(FABM_INCLUDE_PATH) -I$(FABM_MODULE_PATH) -I$(FABMDIR)/src/drivers/$(FABMHOST)
endif
ifeq ($(MOSSCO_GOTM),true)
INCLUDES += -I$(GOTM_MODULE_PATH) -I$(GOTM_INCLUDE_PATH)
endif

#!> @todo expand existing F90FLAGS var but check for not duplicating the -J entry
F90FLAGS = $(ESMF_F90COMPILEOPTS)
ifeq ($(FORTRAN_COMPILER),GFORTRAN)
F90FLAGS += -O3 -J$(MOSSCO_MODULE_PATH)
#F90FLAGS += -ffast-math -march=native -fstack-arrays -fno-protect-parens
# -flto crashes on darwin
EXTRA_CPP= 
else
ifeq ($(FORTRAN_COMPILER),IFORT)
F90FLAGS += -module $(MOSSCO_MODULE_PATH)
EXTRA_CPP= -stand f03
else
ifeq ($(FORTRAN_COMPILER),PGF90)
F90FLAGS += -module $(MOSSCO_MODULE_PATH)
EXTRA_CPP=
else
$(error I don't know where to place modules for FORTRAN_COMPILER=$(FORTRAN_COMPILER).)
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

export ESMF_LIBRARY_PATH=$(ESMF_F90LINKPATHS)
export ESMF_LINKOPTS=$(ESMF_F90LINKRPATHS)
export ESMF_LIBS=$(ESMF_F90ESMFLINKLIBS)
export ESMF_LDFLAGS = $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(ESMF_F90ESMFLINKLIBS)

LIBRARY_PATHS += $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) 
LIBRARY_PATHS += -L$(MOSSCO_LIBRARY_PATH)
export LIBRARY_PATHS

export LIBS := $(ESMF_F90ESMFLINKLIBS)

CPPFLAGS = $(DEFINES)  
CPPFLAGS += -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) -DESMF_VERSION_MINOR=$(ESMF_VERSION_MINOR)
ifeq ("x$(MOSSCO_MPI)","xtrue")
CPPFLAGS += -DMOSSCO_MPI
endif
export CPPFLAGS += $(EXTRA_CPP) $(INCLUDES) $(ESMF_F90COMPILECPPFLAGS) -I.

MOSSCO_LDFLAGS += $(ESMF_F90LINKOPTS)
MOSSCO_LDFLAGS += $(LIBRARY_PATHS)
export MOSSCO_LDFLAGS

endif # End of MAKELEVEL 1 preamble


# Make targets
.PHONY: default all clean doc info prefix libfabm_external libgotm_external libgetm_external
.PHONY: distclean distupdate

# Following GNU standards, "all" should be the default target in every Makefile.
# Therefore we need to define it as a dependency for the first target in this file,
# which is included in the beginning of each Makefile.
default: all

clean:
	@rm -f *.o *.mod *.swp
	@rm -f PET?.*

# changed behaviour: distclean should clean all mossco code regardless of where you call it from
distclean:
	$(MAKE) -C $(MOSSCO_DIR) clean

distupdate:
	$(MAKE) -C $(MOSSCO_DIR) update

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
	@echo FC = $(FC)
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
	@echo STATIC = $(STATIC)
endif
	@env | grep ^MOSSCO_ | sort 


# External libraries

ifeq ($(MOSSCO_FABM),true)

$(FABM_PREFIX)/lib/libfabm.a:
	$(MAKE) -C $(MOSSCO_DIR) libfabm_external

libfabm_external: fabm_build fabm_install

fabm_build:
ifndef MOSSCO_FABM_BINARY_DIR
	@mkdir -p $(FABM_BINARY_DIR)
	(cd $(FABM_BINARY_DIR) && cmake $(FABMDIR)/src -DCMAKE_INSTALL_PREFIX=$(FABM_PREFIX) -DFABM_HOST=$(FABMHOST))
endif

fabm_install:
ifdef FABM_BINARY_DIR
	@echo Recreating the FABM library in $(FABM_PREFIX)
	$(MAKE) -sC $(FABM_BINARY_DIR) install
endif

libfabm_clean:
	@echo Cleaning the FABM library in $(FABM_PREFIX)
ifndef MOSSCO_FABM_BINARY_DIR
	$(RM) -rf $(FABM_BINARY_DIR)
endif
ifndef MOSSCO_FABM_PREFIX
	$(RM) -rf $(FABM_PREFIX)
endif

endif

libgotm_external:
ifdef MOSSCO_GOTMDIR
	@echo Recreating the GOTM library without FABM in $(GOTM_LIBRARY_PATH)
	(unset FABM ; $(MAKE) -C $(GOTMDIR)/src ../VERSION makedirs subdirs features)
	(unset FABM ; $(MAKE) -C $(GOTMDIR)/src/gotm $(GOTM_LIBRARY_PATH)/libgotm_prod.a\(gotm.o\))
endif

libgetm_external: prefix
ifeq ($(MOSSCO_GETM_FABM),true)
ifdef MOSSCO_GOTMDIR
	@echo Recreating the GOTM library in $(GOTM_LIBRARY_PATH)
	(export FABM=true ; $(MAKE) -C $(GOTMDIR)/src ../VERSION makedirs subdirs features)
	(export FABM=true ; $(MAKE) -C $(GOTMDIR)/src/gotm $(GOTM_LIBRARY_PATH)/libgotm_prod.a\(gotm.o\))
endif
ifdef MOSSCO_GETMDIR
	@echo Recreating the GETM library in $(GETM_LIBRARY_PATH)
	(export FABM=true ; $(MAKE) -C $(GETMDIR)/src)
endif
else
ifdef MOSSCO_GOTMDIR
	@echo Recreating the GOTM library without FABM in $(GOTM_LIBRARY_PATH)
	(unset FABM ; $(MAKE) -C $(GOTMDIR)/src ../VERSION makedirs subdirs features)
	(unset FABM ; $(MAKE) -C $(GOTMDIR)/src/gotm $(GOTM_LIBRARY_PATH)/libgotm_prod.a\(gotm.o\))
endif
ifdef MOSSCO_GETMDIR
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

.PHONY: mossco_clean
mossco_clean: distclean
# Note (KK): These distcleans might be redundant, but might also operate outside MOSSCO_DIR.
ifdef MOSSCO_GOTMDIR
	$(MAKE) -C $(MOSSCO_GOTMDIR) distclean
endif
ifdef MOSSCO_GETMDIR
	$(MAKE) -C $(MOSSCO_GETMDIR) distclean
endif

# Common rules
#ifndef EXTRA_CPP







# Portable rules form ESMF Userguide

.SUFFIXES: .f90 .F90 .c .C
.f90:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) \
	$(ESMF_F90COMPILEFREENOCPP) $<
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) \
	$(ESMF_F90LINKRPATHS) -o $@ $*.o $(ESMF_F90ESMFLINKLIBS)
.F90:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) \
	$(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $< $(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) \
	$(ESMF_F90LINKRPATHS) -o $@ $*.o $(ESMF_F90ESMFLINKLIBS)
.c:
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) \
	$(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) \
	$(ESMF_CXXCOMPILECPPFLAGS) $<
	$(ESMF_CXXLINKER) $(ESMF_CXXLINKOPTS) $(ESMF_CXXLINKPATHS) \
	$(ESMF_CXXLINKRPATHS) -o $@ $*.o $(ESMF_CXXESMFLINKLIBS)
.cc .C:
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) \
	$(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<


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

sha:
	@-git log | head -1 | awk '{print "character(len=40), parameter :: MOSSCO_GIT_SHA_KEY = \""$$2"\"" }' \
	> $(MOSSCO_DIR)/src/include/git-sha.h
 
 help:

help:
	@if [ -f README ] ; then cat README ; fi
