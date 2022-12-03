# This Makefile snippet is part of MOSSCO; definition of MOSSCO-wide
# make rules
#
# @copyright (C) 2021-2022 Helmholtz-Zentrum Hereon
# @copyright (C) 2013-2021 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hereon.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

# 0. Execute the preamble only if we are calling Rules make for the first time
# this is detected by the presence of the variable MOSSCO_PREFIX
# All variables that should be passed to submakes need to be exported,
# including all variables that appear in the rules at the end of this file

ifndef MOSSCO_PREFIX

# 1. Checking that we're using GNU make
#    Of course, this command already requires gmake, so a better solution is required here
ifeq ($(shell make --version | grep -c GNU),0)
  $(error GNU make is required)
endif

AWK:=$(shell which gawk 2> /dev/null)
ifeq ($(strip $(AWK)),)
AWK:=$(shell which awk 2> /dev/null)
endif
ifneq ($(strip $(AWK)),)
export AWK:=$(basename $(AWK))
$(info Using awk ... $(AWK))
endif

CMAKE:=$(shell which cmake3 2> /dev/null)
ifeq ($(strip $(CMAKE)),)
CMAKE:=$(shell which cmake 2> /dev/null)
endif
ifneq ($(strip $(CMAKE)),)
export CMAKE:=$(basename $(CMAKE))
$(info Using cmake ... $(CMAKE))
else
$(error Cannot find cmake)
endif

export MOSSCO_OBJC:=false
OBJC=$(shell which objconv 2> /dev/null)
ifeq ($(strip $(OBJC)),)
OBJC=$(shell which gobjcopy 2> /dev/null)
endif
ifeq ($(strip $(OBJC)),)
OBJC=$(shell which objcopy 2> /dev/null)
endif
ifneq ($(strip $(OBJC)),)
MOSSCO_OBJC:=$(shell basename $(OBJC))
$(info Using objcopy ... $(MOSSCO_OBJC))
else
$(info Using objcopy ... no)
endif

export MOSSCO_GIT:=false
ifneq ($(wildcard $(shell which git)),)
MOSSCO_GIT:=true
export MOSSCO_GIT_VERSION:=$(shell git --version |cut -f3 -d" ")
export MOSSCO_GIT_VERSION_MAJOR:=$(shell git --version |cut -f3 -d" "|cut -f1 -d.)
ifeq ($(MOSSCO_GIT_VERSION_MAJOR),1)
  $(warning Consider upgrading git to version 2)
endif
else
  $(warning Consider installing git)
endif
$(info Using git ... $(shell which git) ($(MOSSCO_GIT_VERSION)))

# System-dependent flags
ifeq ($(shell hostname),rznp0023)
  export ARFLAGS:=rv
  export AR:=ar
  $(warning use changed ARFLAGS=rvU)
endif

ifeq ($(shell hostname),KSEZ8002)
  export ARFLAGS:=rvU
  export AR:=ar
  $(warning use changed ARFLAGS=rvU)
endif
$(info Using ar ... $(AR) -$(ARFLAGS))

ifeq ($(MOSSCO_INSTALL_PREFIX),)
  export MOSSCO_INSTALL_PREFIX:=$(MOSSCO_DIR)
endif

# Filter out all MAKELEVELS that are not 1 or 0 to avoid unneccessary execution
# of the preamble section of this Rules.make in repeated calls.  In most circumstances,
# Rules.make is executed at MAKELEVEL 1, unless directly called in $(MOSSCODIR)/src
#ifneq (,$(filter $(MAKELEVEL),0 1))

# 2. ESMF stuff, only if ESMFMKFILE is declared.
#
ifndef ESMFMKFILE
  FORTRAN_COMPILER := $(shell echo $(F90) | tr a-z A-Z)
  FORTRAN_COMPILER := $(shell echo $(FC) | tr a-z A-Z)
  ifeq ("$(FORTRAN_COMPILER)","F77")
    $(error MOSSCO needs a F2003 Fortran compiler, your environment says $$FC=$(FC))
  endif
  #$(error Compiling without ESMF support. Comment this line in Rules.make if you want to proceed at your own risk)
  MOSSCO_ESMF:=false
else
  # Make sure ESMFMKFILE exists and read it, set MOSSCO_ESMF to true
  ifeq ($(wildcard $(ESMFMKFILE)),)
    $(error The file you specified as ESMFMKFILE=$(ESMFMKFILE) does not exist)
  endif
  include $(ESMFMKFILE)
  MOSSCO_ESMF:=true

$(info Using ESMFMKFILE ... $(ESMFMKFILE))

# Find the communicator and determine whether this is parallel device, this
# is still buggy with mpiifort and needs improvement
ESMF_COMM:=$(strip $(shell grep '^. ESMF_COMM:' $(ESMFMKFILE) | cut -d':' -f2-))
$(info Using ESMF_COMM ... $(ESMF_COMM))
ESMF_OS:=$(strip $(shell grep '^. ESMF_OS:' $(ESMFMKFILE) | cut -d':' -f2-))
$(info Using ESMF_OS ... $(ESMF_OS))
$(info Using ESMF_F90COMPILER ... $(ESMF_F90COMPILER))

  ifeq ("$(ESMF_COMM)","mpiuni")
    export MOSSCO_MPI:=false
  else
    export MOSSCO_MPI:=true
    ifeq ($(ESMF_COMM),openmpi)
      ESMF_FC:=$(shell $(ESMF_F90COMPILER) --showme:command 2> /dev/null)
      ifeq ($(ESMF_FC),)
	      ifeq ($(ESMF_F90COMPILER),mpifort)
          ESMF_FC:=$(shell mpif90 --showme:command 2> /dev/null)
        endif
      endif
      ifeq ($(ESMF_FC),)
        $(error $(ESMF_F90COMPILER) is *not* based on $(ESMF_COMM)!)
      endif
      ESMF_CC:=$(shell $(ESMF_CXXCOMPILER) --showme:command 2> /dev/null)
    endif
    ifeq ($(ESMF_COMM),intelmpi)
      ESMF_FC:=$(shell $(ESMF_F90COMPILER) -show 2> /dev/null | cut -d' ' -f1 | cut -d'-' -f1)
      ifeq ($(ESMF_FC),)
        $(error $(ESMF_F90COMPILER) is *not* based on $(ESMF_COMM)!)
      endif
    endif
    ifeq ($(ESMF_COMM),mpich2)
      ESMF_FC:=$(shell $(ESMF_F90COMPILER) -compile_info 2> /dev/null | cut -d' ' -f1 | cut -d'-' -f1)
      ifeq ($(ESMF_FC),x86_64)
        ESMF_FC:=$(shell $(ESMF_F90COMPILER) -compile_info 2> /dev/null | cut -d' ' -f1 | cut -d'-' -f4)
      endif
      ifeq ($(ESMF_FC),)
        $(error $(ESMF_F90COMPILER) is *not* based on $(ESMF_COMM)!)
      endif
      ESMF_CC:=$(shell $(ESMF_CXXCOMPILER) -compile_info 2> /dev/null | cut -d' ' -f1 | cut -d'-' -f1)
    endif
		ifeq ($(ESMF_COMM),mpich3)
      ESMF_FC:=$(shell $(ESMF_F90COMPILER) -compile_info 2> /dev/null | cut -d' ' -f1 | cut -d'-' -f1)
      ifeq ($(ESMF_FC),x86_64)
        ESMF_FC:=$(shell $(ESMF_F90COMPILER) -compile_info 2> /dev/null | cut -d' ' -f1 | cut -d'-' -f4)
      endif
      ifeq ($(ESMF_FC),)
        $(error $(ESMF_F90COMPILER) is *not* based on $(ESMF_COMM)!)
      endif
      ESMF_CC:=$(shell $(ESMF_CXXCOMPILER) -compile_info 2> /dev/null | cut -d' ' -f1 | cut -d'-' -f1)
    endif
		ifeq ($(ESMF_COMM),mpich)
      ESMF_FC:=$(shell $(ESMF_F90COMPILER) -compile_info 2> /dev/null | cut -d' ' -f1 | cut -d'-' -f1)
      ifeq ($(ESMF_FC),x86_64)
        ESMF_FC:=$(shell $(ESMF_F90COMPILER) -compile_info 2> /dev/null | cut -d' ' -f1 | cut -d'-' -f4)
      endif
      ifeq ($(ESMF_FC),)
        $(error $(ESMF_F90COMPILER) is *not* based on $(ESMF_COMM)!)
      endif
      ESMF_CC:=$(shell $(ESMF_CXXCOMPILER) -compile_info 2> /dev/null | cut -d' ' -f1 | cut -d'-' -f1)
    endif
    ifeq ($(ESMF_FC),)
      ESMF_FC:=$(shell $(ESMF_F90COMPILER) -compile_info 2> /dev/null | cut -d' ' -f1 | cut -d'-' -f1)
    endif
    ifeq ($(ESMF_CC),)
      ESMF_CC:=$(shell $(ESMF_CXXCOMPILER) -compile_info 2> /dev/null | cut -d' ' -f1 | cut -d'-' -f1)
    endif
  endif

  $(info Using ESMF_FC ... $(ESMF_FC))
  $(info Using ESMF_CC ... $(ESMF_CC))

  ESMF_OPENMP:=$(strip $(shell grep '^. ESMF_OPENMP:' $(ESMFMKFILE) | cut -d':' -f2-))
  ifeq ("$(ESMF_OPENMP)","OFF")
    export MOSSCO_OMP:=false
  else
    export MOSSCO_OMP:=true
    $(info Using ESMF_OPENMP ... $(ESMF_OPENMP))
  endif

  ESMF_NETCDF:=$(strip $(shell grep '^. ESMF_NETCDF:' $(ESMFMKFILE) | cut -d':' -f2-))
  ifneq ("$(ESMF_NETCDF)","")
    export MOSSCO_NETCDF:=true
    export MOSSCO_NETCDF_INCLUDE := $(strip $(shell grep '^. ESMF_NETCDF_INCLUDE:' $(ESMFMKFILE) | cut -d':' -f2-))
    export MOSSCO_NETCDF_LIBS := $(strip $(shell grep '^. ESMF_NETCDF_LIBS:' $(ESMFMKFILE) | cut -d':' -f2-))
    export MOSSCO_NETCDF_LIBPATH := $(strip $(shell grep '^. ESMF_NETCDF_LIBPATH:' $(ESMFMKFILE) | cut -d':' -f2-))
    $(info Using ESMF_NETCDF_INCLUDE ... $(ESMF_NETCDF_INCLUDE))
    $(info Using ESMF_NETCDF_LIBPATH ... $(ESMF_NETCDF_LIBPATH))
  else
    export MOSSCO_NETCDF:=false
  endif

  ifdef ESMF_F90COMPILER
    export MOSSCO_F03COMPILER=$(ESMF_F90COMPILER)
    export F90 := $(ESMF_F90COMPILER)
    export FC  := $(ESMF_F90COMPILER)
    export F77 := $(ESMF_F77COMPILER)
    ifeq ($(ESMF_FC),)
      ESMF_FC  := $(ESMF_F90COMPILER)
    endif

    ESMF_FORTRAN_COMPILER:=$(shell echo $(notdir $(ESMF_FC)) | tr a-z A-Z | cut -d"-" -f1)

    ifeq ($(ESMF_FORTRAN_COMPILER),FTN)
      ifndef FORTRAN_COMPILER
        $(error FORTRAN_COMPILER needs to be defined for ftn wrapper)
      endif
      $(warning Using FORTRAN_COMPILER=$(FORTRAN_COMPILER) for ftn wrapper)
      ESMF_FORTRAN_COMPILER := $(FORTRAN_COMPILER)
    endif

    ifdef FORTRAN_COMPILER
      ifneq ("$(ESMF_FORTRAN_COMPILER)","$(FORTRAN_COMPILER)")
        $(warning Overwriting FORTRAN_COMPILER=$(FORTRAN_COMPILER) with $(ESMF_FORTRAN_COMPILER))
      endif
    endif
    export FORTRAN_COMPILER := $(ESMF_FORTRAN_COMPILER)
    ifeq ($(FORTRAN_COMPILER), XLF)
      MOSSCO_F03VERSION:=$(shell $(F90) -qversion | head -1)
    else
      MOSSCO_F03VERSION:=$(shell $(F90) --version | head -1 | awk '{print $$NF}')
    endif
  endif
  ifdef ESMF_CXXCOMPILER
    export CXX = $(ESMF_CXXCOMPILER)
  endif
endif

export MOSSCO_ESMF
export MOSSCO_F03VERSION

ifeq ($(MOSSCO_ESMF),true)
  ifeq ($(ESMF_FC),)
    $(error  Your compiler $(F90)'s version could not be determined ($(MOSSCO_F03VERSION)))
  endif
endif

ifeq ($(FORTRAN_COMPILER),MPXLF2003_R)
  FORTRAN_COMPILER=XLF
endif

MOSSCO_CCOMPILER=gcc # default
ifeq ($(ESMF_COMPILER),pgi)
  MOSSCO_CCOMPILER=pgc
endif

ifeq ($(ESMF_COMPILER),intel)
  MOSSCO_CCOMPILER=icc
endif

ifeq ($(ESMF_COMPILER),xlf)
  MOSSCO_CCOMPILER=xlc
endif

ifeq ($(ESMF_COMPILER),gfortranclang)
  MOSSCO_CCOMPILER=clang
endif

export MOSSCO_CCOMPILER

# 3. Checking for the either FABM, GOTM, GETM, SCHISM.  Set the MOSSCO_XXXX variables
#    of these three components to process them later
MOSSCO_FABM=false

#Note (KK): undefine does not work for gnu make <3.8.2
FABM_PREFIX=
FABM_BINARY_DIR=

ifdef MOSSCO_FABM_PREFIX
  export FABM_PREFIX=$(MOSSCO_FABM_PREFIX)
endif

ifdef MOSSCO_FABM_BINARY_DIR
  export FABM_BINARY_DIR=$(MOSSCO_FABM_BINARY_DIR)
  export FABM_PREFIX=$(shell grep CMAKE_INSTALL_PREFIX $(MOSSCO_FABM_BINARY_DIR)/CMakeCache.txt | cut -d "=" -f2)
  #export FABMDIR=$(shell grep fabm_SOURCE_DIR $(MOSSCO_FABM_BINARY_DIR)/CMakeCache.txt | cut -d "=" -f2)
endif

export external_FABMDIR=$(MOSSCO_DIR)/external/fabm/code
ifeq ($(FABM_PREFIX),)
  ifndef MOSSCO_FABMDIR
    ifneq ($(wildcard $(external_FABMDIR)/src/fabm.F90),)
      export MOSSCO_FABMDIR=$(external_FABMDIR)
    endif
  endif
  ifdef MOSSCO_FABMDIR
    export FABMDIR=$(MOSSCO_FABMDIR)
  endif
  ifdef FABMDIR
    export FABM_BINARY_DIR=$(MOSSCO_DIR)/external/fabm/build
    export FABM_PREFIX=$(MOSSCO_DIR)/external/fabm/install
  endif
endif

ifneq ($(FABM_PREFIX),)
  MOSSCO_FABM=true
endif
export MOSSCO_FABM

include $(MOSSCO_DIR/src/schism.mk)

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

#Note (KK): undefine does not work for gnu make <3.8.2
GOTM_PREFIX=
GOTM_BINARY_DIR=

ifdef MOSSCO_GOTM_PREFIX
  export GOTM_PREFIX=$(MOSSCO_GOTM_PREFIX)
endif

ifdef MOSSCO_GOTM_BINARY_DIR
  export GOTM_BINARY_DIR=$(MOSSCO_GOTM_BINARY_DIR)
  export GOTM_PREFIX=$(shell grep CMAKE_INSTALL_PREFIX $(MOSSCO_GOTM_BINARY_DIR)/CMakeCache.txt | cut -d "=" -f2)
  #export GOTMDIR=$(shell grep fabm_SOURCE_DIR $(MOSSCO_GOTM_BINARY_DIR)/CMakeCache.txt | cut -d "=" -f2)
endif

export external_GOTMDIR=$(MOSSCO_DIR)/external/gotm/code
ifeq ($(GOTM_PREFIX),)
  ifndef MOSSCO_GOTMDIR
    ifneq ($(wildcard $(external_GOTMDIR)/src/gotm/gotm.F90),)
      export MOSSCO_GOTMDIR=$(external_GOTMDIR)
    endif
  endif
  ifdef MOSSCO_GOTMDIR
    export GOTMDIR=$(MOSSCO_GOTMDIR)
  endif
  ifdef GOTMDIR
    export GOTM_BINARY_DIR=$(MOSSCO_DIR)/external/gotm/build
    export GOTM_PREFIX=$(MOSSCO_DIR)/external/gotm/install
  endif
endif

ifneq ($(GOTM_PREFIX),)
  MOSSCO_GOTM=true
endif
export MOSSCO_GOTM

# 3bb SCHISM
export MOSSCO_SCHISM=false

ifdef SCHISM_ESMF_DIR
  ifdef SCHISM_DIR
    MOSSCO_SCHISM=true
  endif
endif

# ifeq($(MOSSCO_SCHISM),true)
# 	export MOSSCO_SCHISM_CMAKE="-DOLDIO=ON"
# 	ifeq($(MOSSCO_FABM),true)
# 		MOSSCO_SCHISM_CMAKE="$(MOSSCO_SCHISM_CMAKE) -DUSE_FABM=ON"
# 		MOSSCO_SCHISM_CMAKE="$(MOSSCO_SCHISM_CMAKE) -DFABM_BASE=$(MOSSCO_FABMDIR)"
# 	endif
# endif

# 3c. GETM
MOSSCO_GETM=false

export external_GETMDIR=$(MOSSCO_DIR)/external/getm/code
ifndef MOSSCO_GETMDIR
  ifneq ($(wildcard $(external_GETMDIR)/src/getm/main.F90),)
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
    export GETM_ESMF=true
    ifeq ($(MOSSCO_MPI),true)
      export GETM_PARALLEL?=true
      ifeq ($(ESMF_COMM),openmpi)
        export MPI=OPENMPI
      else
      ifeq ($(ESMF_COMM),intelmpi)
        export MPI=INTELMPI
      else
        ifeq ($(ESMF_COMM),mpi)
          export MPI=MPICH
        else
          export MPI=MPICH2
        endif
      endif
      endif
    else
      export GETM_PARALLEL=false
    endif
  endif
endif

export MOSSCO_GETM

# 3d JSON library
MOSSCO_JSON=false

export external_JSONDIR=$(MOSSCO_DIR)/external/json/code

ifndef MOSSCO_JSONDIR
  ifneq ($(wildcard $(external_JSONDIR)/src/json_module.F90),)
    export MOSSCO_JSONDIR=$(external_JSONDIR)
  endif
endif

ifdef MOSSCO_JSONDIR
  export JSON_BINARY_DIR=$(MOSSCO_DIR)/external/json/build
  export JSON_PREFIX=$(MOSSCO_DIR)/external/json/install
endif

ifneq ($(JSON_JSONDIR),)
  MOSSCO_JSON=true
endif
export MOSSCO_JSON

# 4. Dealing with compiler matching of ESMF and FABM/GOTM/GETM, if one of those
# is found, we require  that FORTRAN_COMPILER is set and that
# the libraries are installed in the production version (libfabm_prod)
# @todo adjust this generic for FABM/GOTM/GETM

ifeq ($(MOSSCO_GETM),true)
  ifndef FORTRAN_COMPILER
    GETM_AVAILABLE_COMPILERS=$(shell ls -1 $(GETMDIR)/compilers/compiler.* | cut -d'.' -f2)
    GETM_AVAILABLE_COMPILERS:=$(patsubst %compiler.,,$(GETM_AVAILABLE_COMPILERS))
    $(error FORTRAN_COMPILER needs to be set to one of the compilers in $(GETMDIR)/compilers: $(GETM_AVAILABLE_COMPILERS))
  endif
endif

ifeq ($(MOSSCO_FABM),true)
  export FABM_LIBRARY_PATH=$(FABM_PREFIX)/lib
  export FABM_LIBS=-lfabm
  export FABM_CPPFLAGS = -I$(FABM_PREFIX)/include
  export FABM_LDFLAGS = -L$(FABM_LIBRARY_PATH) $(FABM_LIBS)
endif

ifeq ($(MOSSCO_GOTM),true)
  export GOTM_LIBRARY_PATH=$(GOTM_PREFIX)/lib
  GOTM_LIBS:=-lgotm -lairsea_driver -lairsea -lmeanflow -loutput_manager -lfield_manager -lyaml
  GOTM_LIBS+=-lobservations -lstokes_drift -linput_manager -lturbulence $(GOTM_PREFIX)/lib/libutil.a
  GOTM_LIBS+=-lconfig
  #export GOTM_LIBRARY_PATH=$(GOTMDIR)/lib/$(FORTRAN_COMPILER)
  #GOTM_LIBS:=-lgotm_prod -lairsea_prod -lmeanflow_prod -lseagrass_prod -loutput_prod
  #GOTM_LIBS+=-lobservations_prod -linput_prod -lturbulence_prod -lutil_prod
  ifeq ($(MOSSCO_FABM),true)
    ifeq ($(FORTRAN_COMPILER), XLF)
      DEFINES += -WF,-D_GOTM_MOSSCO_FABM_
    else
      DEFINES += -D_GOTM_MOSSCO_FABM_
    endif
    export MOSSCO_GOTM_FABM=true
  endif
  #export GOTM_CPPFLAGS = -I$(GOTMDIR)/include -I$(GOTMDIR)/modules/$(FORTRAN_COMPILER)
  export GOTM_CPPFLAGS = -I$(GOTM_PREFIX)/include
  export GOTM_LDFLAGS = -L$(GOTM_LIBRARY_PATH) $(GOTM_LIBS)
endif

ifeq ($(MOSSCO_GETM),true)
  export GETM_LIBRARY_PATH=$(GETMDIR)/lib/$(FORTRAN_COMPILER)
  GETM_LINKDIRS = -L$(GETM_LIBRARY_PATH) #-L$(GOTM_LIBRARY_PATH)
  #GETM_LIBS := -lgetm_esmf_prod -lgetm_prod  -loutput_prod -lmeteo_prod
  #ifneq ($(GETM_NO_3D),true)
  #  GETM_LIBS += -l3d_prod
  #endif
  #GETM_LIBS += -l2d_prod -lwaves_prod -lles_prod -lpool_prod -ldomain_prod -linput_prod -lncdfio_prod -lfutils_prod
  #ifeq ($(MOSSCO_GETM_FABM),true)
  #  GETM_LINKDIRS += -L$(FABM_LIBRARY_PATH)
  #  GETM_LIBS += -lgotm_fabm_prod $(FABM_LIBS)
  #endif
  #GETM_LIBS += -lturbulence -lutil -loutput_manager -lfield_manager -lyaml
  #
  # Problems with linking on arm64 prevent using the combined libgetm_all
  #ifneq ($(ESMF_OS),"Darwin")
  #  GETM_LIBS += -lgetm_all
  #else
    GETM_LIBS += -lgetm_esmf_prod -lgetm_prod -loutput_prod -lmeteo_prod -l3d_prod \
      -l2d_prod -lwaves_prod -lles_prod -lpool_prod -ldomain_prod -linput_prod \
      -lncdfio_prod -lfutils_prod -L$(GOTM_LIBRARY_PATH) $(GOTM_LIBS)
  #endif

  ifeq ($(FORTRAN_COMPILER), XLF)
    export STATIC += -WF,$(GETM_STATIC_DEFINES)
  else
    export STATIC += $(GETM_STATIC_DEFINES)
  endif
  ifeq ($(GETM_SLICE_MODEL),true) # Compile for slice model option
    export STATIC += -DSLICE_MODEL
  endif
  export GETM_CPPFLAGS = $(STATIC)
  ifeq ($(GETM_PARALLEL),true) # Compile for parallel execution
    export GETM_CPPFLAGS += -DGETM_PARALLEL
  endif
  export GETM_LIBRARY_PATH
  export GETM_CPPFLAGS += -I$(GETMDIR)/include -I$(GETMDIR)/modules/$(FORTRAN_COMPILER)
  export GETM_CPPFLAGS += -I$(GOTM_PREFIX)/include
  export GETM_LDFLAGS = $(GETM_LINKDIRS) $(GETM_LIBS)

  ifeq ($(GETM_SLICE_MODEL),true)
    DEFINES += -DGETM_SLICE_MODEL
  endif

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
  ifeq ($(FORTRAN_COMPILER), XLF)
    DEFINES += -WF,-DMOSSCO_EROSED
  else
    DEFINES += -DMOSSCO_EROSED
  endif
endif
export MOSSCO_EROSED

# 6a. TRACER
MOSSCO_TRACER=false

ifndef MOSSCO_TRACERDIR
  external_TRACERDIR = $(MOSSCO_DIR)/external/tracer-git
  ifneq ($(wildcard $(external_TRACERDIR)/src/Makefile),)
    export MOSSCO_TRACERDIR=$(external_TRACERDIR)
  endif
endif

ifdef MOSSCO_TRACERDIR
  export TRACER_DIR=$(MOSSCO_TRACERDIR)
else
  ifdef TRACER_DIR
    $(warning Assuming you have a working TRACER in ${TRACER_DIR}, proceed at your own risk or set the environment variable $$MOSSCO_TRACERDIR explicitly to enable the build system to take care of the TRACER build)
  endif
endif


ifdef TRACER_DIR
  MOSSCO_TRACER=true
  DEFINES += -D MOSSCO_TRACER
endif
export MOSSCO_TRACER

ifeq ($(TRACER_FORCING_ONLINE),true)
  DEFINES += -DForcing_Online
endif


# 6b. HAMSOM
MOSSCO_HAMSOM =false

ifndef MOSSCO_HAMSOMDIR
  external_HAMSOMDIR = $(HAMSOM_DIR)/external/hamsom-git
  ifneq ($(wildcard $(external_HAMSOMDIR)/src/makefile),)
    export MOSSCO_HAMSOMDIR=$(external_HAMSOMDIR)
  endif
endif

ifdef MOSSCO_HAMSOMDIR
  export HAMSOM_DIR=$(MOSSCO_HAMSOMDIR)
else
  ifdef HAMSOM_DIR
    $(warning Assuming you have a working HAMSOM in ${HAMSOM_DIR}, proceed at your own risk or set the environment variable $$MOSSCO_HAMSOMDIR explicitly to enable the build system to take care of the HAMSOM build)
  endif
endif

ifdef HAMSOM_DIR
  MOSSCO_HAMSOM=true
  DEFINES += -D MOSSCO_HAMSOM
endif

export MOSSCO_HAMSOM

ifeq ($(MOSSCO_HAMSOM),true)
  HAMSOM_INCLUDES=
  HAMSOM_DEFINES=

  HAMSOM_DEFINES += -DCalcHBottomOld
  HAMSOM_DEFINES += -DNOVEC
  HAMSOM_DEFINES += -Dcheck_waterlevel
  HAMSOM_DEFINES += -Dcalc_AvrCumZ
  HAMSOM_DEFINES += -Dincl_AvrW
#  HAMSOM_DEFINES += -DAvrInclBounds
#  HAMSOM_DEFINES += -DreadATZ_monthly_mean
#  HAMSOM_DEFINES += -Dbaltic_closedEB
  HAMSOM_DEFINES += -Dcheck_BoundaryZ
  HAMSOM_DEFINES += -DBndDynHeight
  HAMSOM_DEFINES += -DRadiationUV_XinpingChen
  HAMSOM_DEFINES += -DRadiationTS_XinpingChen
#  HAMSOM_DEFINES += -Dsmooth_BoundaryTS
  HAMSOM_DEFINES += -DMomentumJP
#  HAMSOM_DEFINES += -DFullSWR
  HAMSOM_DEFINES += -DRiverFilesECOHAM
  HAMSOM_DEFINES += -DRiverInterpolationOff
  HAMSOM_DEFINES += -DIncludeFreshwater

  export HAMSOM_CPPFLAGS=$(HAMSOM_DEFINES) $(HAMSOM_INCLUDES)
  export HAMSOM_FFLAGS = -xf95-cpp-input -fdefault-real-8 -fsign-zero -fno-f2c -Wno-uninitialized

endif

## 6d. SQLITE
MOSSCO_SQLITE=false

ifndef SQLITE_DIR
  external_SQLITE_DIR = $(MOSSCO_DIR)/external/flibs-cvs/src/sqlite
  ifneq ($(wildcard $(external_SQLITE_DIR)),)
    SQLITE_DIR=$(external_SQLITE_DIR)
  endif
  export SQLITE_DIR
endif

ifdef SQLITE_DIR
  ifeq ($(wildcard $(SQLITE_DIR)/../../config/config.mk),)
    SQLITE_DIR=
  endif
endif

ifdef SQLITE_DIR
  MOSSCO_SQLITE=true
  ifeq ($(FORTRAN_COMPILER), XLF)
    DEFINES += -WF,-DMOSSCO_SQLITE
  else
    DEFINES += -DMOSSCO_SQLITE
  endif
  SQLITE_LIBS = -lmossco_db -lsqlite3
endif
export MOSSCO_SQLITE SQLITE_DIR

# 7. MOSSCO declarations. The MOSSCO_DIR and the build prefix are set, as well as the bin/mod/lib paths relative
#    to the PREFIX
#
ifndef MOSSCO_DIR
  ifdef MOSSCODIR
    MOSSCO_DIR=$(MOSSCODIR)
  else
    MOSSCO_DIR=$(subst /src$,,$(CURDIR))
  endif
endif
export MOSSCO_DIR

ifeq ($(wildcard $(MOSSCO_DIR)),)
$(error the directory MOSSCO_DIR=$(MOSSCO_DIR) does not exist)
endif

MOSSCO_PREFIX?=$(MOSSCO_DIR)
export MOSSCO_PREFIX

export MOSSCO_MODULE_PATH=$(MOSSCO_PREFIX)/modules/$(FORTRAN_COMPILER)
export MOSSCO_LIBRARY_PATH=$(MOSSCO_PREFIX)/libraries/$(FORTRAN_COMPILER)

# 7. Putting everything together.
# This is the list of ESMF-supported compilers:
# absoft absoftintel cce default g95 gfortran gfortranclang intel intelcl intelgcc
# lahey nag nagintel pathscale pgi pgigcc sxcross xlf xlfgcc

# determine the compiler used by FABM/GOTM/GETM
ifdef FORTRAN_COMPILER
ifneq ($(FORTRAN_COMPILER),)

  ifeq ($(FORTRAN_COMPILER),PGF90)
    # pgfortran 14.1-0 has a problem compiling benthos_component.F90 (UK 22.05.2014)
    # see $MOSSCO_DIR/src/components/Makefile
    export MOSSCO_BENTHOS=false
  else
    export MOSSCO_BENTHOS=true
  endif

  ifeq (${MOSSCO_GETM},true)
    GETM_F90COMPILER=$(shell grep 'FC=' $(GETMDIR)/compilers/compiler.$(FORTRAN_COMPILER) | cut -d"=" -f2)

    ifeq ($(FORTRAN_COMPILER), XLF)
      GETM_F90COMPILER_VERSION:=$(shell $(GETM_F90COMPILER) -qversion | head -1)
    else
      GETM_F90COMPILER_VERSION:=$(shell $(GETM_F90COMPILER) --version | head -1)
    endif
    ifndef F90
      export F90=$(GETM_F90COMPILER)
      $(warning F90 automatically determined from GETM environment: F90=$(F90))
    endif
  endif

  export F90 ?= $(shell echo $(FORTRAN_COMPILER) | tr A-Z a-z)

endif
endif

# Include directories
INCLUDES += $(ESMF_F90COMPILEPATHS) $(ESMF_CXXCOMPILEPATHS)
INCLUDES += -I$(MOSSCO_MODULE_PATH)
INCLUDES += -I$(MOSSCO_DIR)/src/include

#!> @todo expand existing F90FLAGS var but check for not duplicating the -J entry
CFLAGS = $(MOSSCO_CFLAGS) $(ESMF_CXXCOMPILEOPTS)
#F90FLAGS += $(HAMSOM_FFLAGS)
ifeq ($(FORTRAN_COMPILER),GFORTRAN)
F90FLAGS += -O3 -J$(MOSSCO_MODULE_PATH)
#F90FLAGS += -ffast-math -march=native -fstack-arrays -fno-protect-parens
# -flto crashes on darwin
ifeq ($(MOSSCO_F03VERSION%%.*), 10)
F90FLAGS += -fallow-argument-mismatch
endif
EXTRA_CPP =
#EXTRA_CPP += -ffpe-trap=invalid,zero,overflow
#EXTRA_CPP += -ffpe-trap=invalid
else
ifeq ($(FORTRAN_COMPILER),IFORT)
F90FLAGS += -module $(MOSSCO_MODULE_PATH)
EXTRA_CPP=-DNO_ISO_FORTRAN_ENV
#EXTRA_CPP += -fpe0
else
ifeq ($(FORTRAN_COMPILER),PGFORTRAN)
F90FLAGS += -module $(MOSSCO_MODULE_PATH)
EXTRA_CPP=
else
ifeq ($(FORTRAN_COMPILER),XLF)
F90FLAGS += -qmoddir=$(MOSSCO_MODULE_PATH) -qstrict
EXTRA_CPP=-WF,-DNO_ISO_FORTRAN_ENV
else
$(error I don't know where to place modules for FORTRAN_COMPILER="$(FORTRAN_COMPILER)". You may have to set this variable or the variable ESMFMKFILE)
endif
endif
endif
endif
F90FLAGS += $(ESMF_F90COMPILEOPTS) $(MOSSCO_FFLAGS)
export F90FLAGS
export CFLAGS

ifndef HAVE_LD_FORCE_LOAD
  HAVE_LD_FORCE_LOAD=$(shell ld -v 2>&1 | grep -c LLVM)
  ifeq ($(HAVE_LD_FORCE_LOAD),1)
    HAVE_LD_FORCE_LOAD=true
  else
    HAVE_LD_FORCE_LOAD=false
  endif
  export HAVE_LD_FORCE_LOAD
endif

export ESMF_F90LIBS=$(ESMF_F90ESMFLINKLIBS)
export ESMF_F90LDFLAGS = $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(ESMF_F90ESMFLINKLIBS)

LIBRARY_PATHS += $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS)
LIBRARY_PATHS += -L$(MOSSCO_LIBRARY_PATH)
export LIBRARY_PATHS

export LIBS := $(ESMF_F90ESMFLINKLIBS)

CPPFLAGS = $(MOSSCO_CPPFLAGS) $(DEFINES)
ifeq ($(FORTRAN_COMPILER),XLF)
CPPFLAGS += -WF,-DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) -WF,-DESMF_VERSION_MINOR=$(ESMF_VERSION_MINOR)
else
CPPFLAGS += -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) -DESMF_VERSION_MINOR=$(ESMF_VERSION_MINOR)
endif
ifeq ("x$(MOSSCO_MPI)","xtrue")
ifeq ($(FORTRAN_COMPILER),XLF)
CPPFLAGS += -WF,-DMOSSCO_MPI
else
CPPFLAGS += -DMOSSCO_MPI
endif
endif
ifeq ("x$(MOSSCO_OMP)","xtrue")
ifeq ($(FORTRAN_COMPILER),XLF)
CPPFLAGS += -WF,-DMOSSCO_OMP
else
CPPFLAGS += -DMOSSCO_OMP
endif
endif
export CPPFLAGS += $(EXTRA_CPP) $(INCLUDES) $(ESMF_F90COMPILECPPFLAGS) -I.

MOSSCO_F90LDFLAGS += $(ESMF_F90LINKOPTS)
MOSSCO_F90LDFLAGS += $(LIBRARY_PATHS)
export MOSSCO_F90LDFLAGS

endif # End of MAKELEVEL 1 preamble


# Make targets
.PHONY: default all doc info prefix libfabm_external libgotm_external libgetm_external libjson_external install
.PHONY: distclean distupdate

# Following GNU standards, "all" should be the default target in every Makefile.
# Therefore we need to define it as a dependency for the first target in this file,
# which is included in the beginning of each Makefile.
default: all

# changed behaviour: distclean should clean all mossco code regardless of where you call it from
distclean:
	$(MAKE) -C $(MOSSCO_DIR) clean

distupdate:
	$(MAKE) -C $(MOSSCO_DIR) update

prefix:
	@mkdir -p $(MOSSCO_LIBRARY_PATH)
	@mkdir -p $(MOSSCO_MODULE_PATH)
	@mkdir -p $(MOSSCO_INSTALL_PREFIX)/bin
	@mkdir -p $(MOSSCO_INSTALL_PREFIX)/include
	@mkdir -p $(MOSSCO_INSTALL_PREFIX)/lib

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
ifeq ($(MOSSCO_TRACER),true)
	@env | grep ^TRACER | sort
endif
ifeq ($(MOSSCO_HAMSOM),true)
	@env | grep ^HAMSOM | sort
endif
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
ifeq ($(MOSSCO_SQLITE),true)
	@env | grep ^SQLITE | sort
endif
ifeq ($(MOSSCO_SCHISM),true)
	@env | grep ^SCHISM | sort
endif
	@env | grep ^MOSSCO_ | sort


# External libraries

libfabm_external: fabm_build fabm_install

fabm_build:
ifeq ($(MOSSCO_FABM),true)
ifndef MOSSCO_FABM_BINARY_DIR
	@mkdir -p $(FABM_BINARY_DIR)
	(cd $(FABM_BINARY_DIR) && $(CMAKE) $(FABMDIR)/src -DCMAKE_INSTALL_PREFIX=$(FABM_PREFIX) -DFABM_HOST=$(FABMHOST) -DCMAKE_Fortran_FLAGS="$(FABM_FFLAGS)")
endif
endif

fabm_install:
ifeq ($(MOSSCO_FABM),true)
ifdef FABM_BINARY_DIR
	@echo Recreating the FABM library in $(FABM_PREFIX)
	$(MAKE) -sC $(FABM_BINARY_DIR) install
endif
endif

libjson_external: json_build json_install

json_build:
ifeq ($(MOSSCO_JSON),true)
ifndef MOSSCO_JSON_BINARY_DIR
	@mkdir -p $(JSON_BINARY_DIR)
	(cd $(JSON_BINARY_DIR) && $(CMAKE) $(JSONDIR)/src -DCMAKE_INSTALL_PREFIX=$(JSON_PREFIX)
endif
endif

json_install:
ifeq ($(MOSSCO_JSON),true)
ifdef JSON_BINARY_DIR
	@echo Recreating the JSON library in $(JSON_PREFIX)
	$(MAKE) -sC $(JSON_BINARY_DIR) install
endif
endif

json_clean:
ifeq ($(MOSSCO_JSON),true)
	@echo Cleaning the JSON library in $(JSON_PREFIX)
ifndef MOSSCO_JSON_BINARY_DIR
	$(RM) -rf $(JSON_BINARY_DIR)
endif
ifndef MOSSCO_JSON_PREFIX
	$(RM) -rf $(JSON_PREFIX)
endif
endif


libgotm_external: gotm_build gotm_install
#ifdef MOSSCO_GOTMDIR
#	@echo Recreating the GOTM library without FABM in $(GOTM_LIBRARY_PATH)
#	(unset FABM ; $(MAKE) -C $(GOTMDIR)/src ../VERSION makedirs subdirs features)
#	(unset FABM ; $(MAKE) -C $(GOTMDIR)/src/gotm $(GOTM_LIBRARY_PATH)/libgotm_prod.a\(gotm.o\))
#endif

gotm_build:
ifeq ($(MOSSCO_GOTM),true)
ifndef MOSSCO_GOTM_BINARY_DIR
	@mkdir -p $(GOTM_BINARY_DIR)
	(cd $(GOTM_BINARY_DIR) && $(CMAKE) $(GOTMDIR) -DCMAKE_INSTALL_PREFIX=$(GOTM_PREFIX) -DGOTM_USE_FABM=OFF -DCMAKE_Fortran_FLAGS="$(GOTM_FFLAGS)")
endif
endif

gotm_install:
ifeq ($(MOSSCO_GOTM),true)
ifdef GOTM_BINARY_DIR
	@echo Recreating the GOTM library in $(GOTM_PREFIX)
	$(MAKE) -sC $(GOTM_BINARY_DIR) install
	cp $(GOTM_BINARY_DIR)/modules/*.mod $(GOTM_PREFIX)/include/
	#cp $(GOTM_BINARY_DIR)/*.mod $(GOTM_PREFIX)/include/
#	( for lib in gotm airsea meanflow observations input ; do \
#       $(AR) rcs $(GOTM_PREFIX)/lib/lib$$lib.a $(GOTM_BINARY_DIR)/CMakeFiles/$$lib.dir/$$lib/*.o ; \
#     done )
	( for lib in airsea meanflow observations stokes_drift ; do \
	cp $(GOTM_BINARY_DIR)/$$lib/lib$$li*.a $(GOTM_PREFIX)/lib; \
     done; cp $(GOTM_BINARY_DIR)/gotmlib/libgotm.a $(GOTM_PREFIX)/lib; \
     cp $(GOTM_BINARY_DIR)/input/libinput_manager.a $(GOTM_PREFIX)/lib)
	cp $(GOTM_BINARY_DIR)/extern/flexout/extern/yaml/modules/yaml_settings.mod $(GOTM_PREFIX)/include/
endif
endif

libgetm_external: libgotm_external
ifdef MOSSCO_GETMDIR
	#( unset FABM ; $(MAKE) -C $(GETMDIR)/src GIT FORTRAN ../VERSION makedirs subdirs )
	( unset FABM ; export GETM_NUOPC=true;  $(MAKE) -C $(GETMDIR)/src nuopc )
endif

#libtracer_external: prefix
#ifdef MOSSCO_TRACERDIR
#	@echo Recreating the TRACER library in $(MOSSCO_TRACERDIR)
#	($(MAKE) -C $(MOSSCO_TRACERDIR)/src)
#endif

#$(AR) Trus $(MOSSCO_LIBRARY_PATH)/libgetm_external.a $(GETM_LIBRARY_PATH)/lib*_prod.a

install-mossco-bin:
	@mkdir -p $(MOSSCO_INSTALL_PREFIX)/bin
	@ln -sf $(MOSSCO_DIR)/scripts/mossco.sh  $(MOSSCO_INSTALL_PREFIX)/bin/mossco
	@ln -sf $(MOSSCO_DIR)/scripts/postprocess/stitch_tiles.py  $(MOSSCO_INSTALL_PREFIX)/bin/stitch
	@echo "Executables 'mossco' and 'stitch' have been installed to $(MOSSCO_INSTALL_PREFIX)/bin. "
	@echo "Consider to add this directory to your PATH"

install-mossco-include:
	@mkdir -p $(MOSSCO_INSTALL_PREFIX)/include
	@cp $(MOSSCO_MODULE_PATH)/*.mod $(MOSSCO_INSTALL_PREFIX)/include
	@echo "Use includes with  '-I $(MOSSCO_INSTALL_PREFIX)/include'"

install: install-mossco-bin install-mossco-include install-mossco-lib

install-mossco-lib:
	@(cd $(MOSSCO_LIBRARY_PATH); for F in *.a ; do $(AR) x $$F; done )
	@(cd $(MOSSCO_LIBRARY_PATH); $(RM) -f SORTED __*; $(AR) crus libmossco.a *.o )
	@$(RM) -f $(MOSSCO_LIBRARY_PATH)/*.o
	@mkdir -p $(MOSSCO_INSTALL_PREFIX)/lib
	@mv $(MOSSCO_LIBRARY_PATH)/libmossco.a $(MOSSCO_INSTALL_PREFIX)/lib/;
ifeq ($(MOSSCO_FABM),true)
	@cp $(MOSSCO_DIR)/external/fabm/install/lib/libfabm.a $(MOSSCO_INSTALL_PREFIX)/lib/libmossco_fabm.a
	@(cd $(MOSSCO_INSTALL_PREFIX)/lib; python $(MOSSCO_DIR)/scripts/rename_fabm_symbols.py $(MOSSCO_INSTALL_PREFIX))
	@echo "Renamed symbols in fabm library __fabm_* => ___mossco_fabm_*"
	@echo "Use library with '-L $(MOSSCO_INSTALL_PREFIX)/lib -lmossco -lmossco_fabm'"
else
	@echo "Use library with '-L $(MOSSCO_INSTALL_PREFIX)/lib -lmossco'"
endif

.PHONY: mossco_clean

mossco_clean: distclean
	$(MAKE) -C $(MOSSCO_DIR)/external external_clean
#ifdef MOSSCO_TRACERDIR
#	$(MAKE) -C $(MOSSCO_TRACERDIR) distclean
#endif

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
	@echo "SUFFIX Compiling $<"
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) \
	$(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $< $(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) \
	$(ESMF_F90LINKRPATHS) -o $@ $*.o $(ESMF_F90ESMFLINKLIBS)
#.c:
#	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) \
#	$(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) \
#	$(ESMF_CXXCOMPILECPPFLAGS) $<
#	$(ESMF_CXXLINKER) $(ESMF_CXXLINKOPTS) $(ESMF_CXXLINKPATHS) \
#	$(ESMF_CXXLINKRPATHS) -o $@ $*.o $(ESMF_CXXESMFLINKLIBS)
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
%.o: %.c
	@echo "Compiling $<"
	$(MOSSCO_CCOMPILER) $(CPPFLAGS) $(DEFINES) -c $< -o $@

# %.o: %.f90
#	@echo "Compiling $<"
#	$(F90) $(CPPFLAGS)  -c $< -o $@
# else
# %.f90: %.F90
#	$(CPP) $(CPPFLAGS) $< -o $@
#	$(F90_to_f90)
# %.o: %.f90
#	$(F90) $(F90FLAGS) $(EXTRA_FFLAGS) -c $< -o $@
# endif

sha:
	make -C $(MOSSCO_DIR)/src/include

help:
	@if [ -f README ] ; then cat README ; fi


# Dummy rules to avoid searching for these (increases speed of make)
GNUmakefile:
	@echo ;

makefile:
	@echo ;
