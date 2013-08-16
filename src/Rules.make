# Makefile rules applying to the entire MOSSCO src and examples directories

# 1. Checking that we're using GNU make 
#    Of course, this command already requires gmake, so a better solution is required here
ifeq ($(shell make --version | grep -c GNU),0)
$(error GNU make is required)
endif 

# 2. Importing all FABM-related environment variables and checking that the environment is sane
#    At the moment, we require that FABMDIR, FABMHOST, and FORTRAN_COMPILER are set and that
#    the fabm library is installed in the production version (libfabm_prod)
# 
ifndef FABMDIR
$(error FABMDIR needs to be defined)
endif

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

# 3. (optional) Importing all GOTM-related environment variables and checking that the environment is sane
# At the moment, we require that GOTMDIR, FABM, and FORTRAN_COMPILER are set and that
# the gotm library is installed in the production version (libgotm_prod)

ifdef $GOTMDIR
ifndef $(FABM) 
export FABM=true
$(warning FABM automatically set to FABM=$(FABM) for GOTM in $(GOTMDIR))
endif
endif

GOTM_MODULE_PATH=$(GOTMDIR)/modules/$(FORTRAN_COMPILER)
GOTM_INCLUDE_PATH=$(GOTMDIR)/include
GOTM_LIBRARY_PATH=$(GOTMDIR)/lib/$(FORTRAN_COMPILER)
GOTM_LIBRARY_FILE=$(shell ls $(GOTM_LIBRARY_PATH) )

# 4. ESMF stuff, only if ESMFMKFILE is declared.  We need to work on an intelligent system that prevents
#    the components and mediators to be built if ESMF is not found in your system
#
ifndef ESMFMKFILE
ifndef $(MOSSCO_ESMF)
$(warning Compiling without ESMF support)
export MOSSCO_ESMF=false
endif
else
include $(ESMFMKFILE)
export MOSSCO_ESMF=true
endif

# 3. MOSSCO declarations. The MOSSCODIR and the build prefix are set, as well as the bin/mod/lib paths relative
#    to the PREFIX
#
ifndef MOSSCODIR
export MOSSCODIR=$(subst /src$,,$(PWD))
endif

ifeq ($(wildcard $(MOSSCODIR)),) 
$(error the directory MOSSCODIR=$(MOSSCODIR) does not exist)
endif

ifdef PREFIX
MOSSCOPREFIX=$(PREFIX)
else
MOSSCOPREFIX=$(MOSSCODIR)
endif
export MOSSCOPREFIX

export MOSSCO_MODULE_PATH=$(MOSSCOPREFIX)/modules/$(FORTRAN_COMPILER)
export MOSSCO_LIBRARY_PATH=$(MOSSCOPREFIX)/lib/$(FORTRAN_COMPILER)
export MOSSCO_BIN_PATH=$(MOSSCOPREFIX)/bin

# 4. Putting everything together.  This section could need some cleanup, but does work fornow
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
ifndef $(MOSSCO_COMPILER)
$(warning F90=$(F90) different from compiler used by FABM ($(FABM_F90COMPILER)))
endif
endif
export MOSSCO_COMPILER=$(F90)
export F90

INCLUDES  = -I$(FABM_INCLUDE_PATH) -I$(FABM_MODULE_PATH) -I$(FABMDIR)/src/drivers/$(FABMHOST)
INCLUDES += $(ESMF_F90COMPILEPATHS)
INCLUDES += -I$(MOSSCO_MODULE_PATH)
INCLUDES += -I$(GOTM_MODULE_PATH)

ifeq ($(FORTRAN_COMPILER),GFORTRAN)
INCLUDES += -J$(MOSSCO_MODULE_PATH)
EXTRA_CPP=
else
ifeq ($(FORTRAN_COMPILER),IFORT)
INCLUDES += -module $(MOSSCO_MODULE_PATH)
EXTRA_CPP=
endif
endif

LIBRARY_PATHS  = -L$(FABM_LIBRARY_PATH) 
LIBRARY_PATHS += $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) 
LIBRARY_PATHS += -L$(MOSSCO_LIBRARY_PATH)
export LIBRARY_PATHS

LIBS = -lfabm_prod
LIBS += $(ESMF_F90LINKLIBS)
export LIBS

export CPPFLAGS = $(DEFINES) $(INCLUDES) $(ESMF_F90COMPILECPPFLAGS)

export LDFLAGS = $(ESMF_F90LINKOPTS)

# Make targets
.PHONY: default all clean doc info

default: prefix all

clean:
	@rm -f *.o

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
	@echo FABMHOST = $(FABMHOST)
	@echo FABMDIR = $(FABMDIR)
	@echo MOSSCODIR = $(MOSSCODIR)
	@echo MOSSCO_LIBRARY_PATH = $(MOSSCO_LIBRARY_PATH)
	@echo MOSSCO_MODULE_PATH = $(MOSSCO_MODULE_PATH)
	@echo MOSSCO_BIN_PATH = $(MOSSCO_BIN_PATH)
	@echo INCDIRS = $(INCDIRS)
	@echo F90FLAGS = $(F90FLAGS)
	@echo LDFLAGS = $(LDFLAGS)
	@echo LINKDIRS = $(LINKDIRS)

# Common rules
#ifndef EXTRA_CPP
%.o: %.F90
	@echo "Compiling $<"
	$(F90) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@
#else
#%.f90: %.F90
#	$(CPP) $(CPPFLAGS) $< -o $@
#	$(F90_to_f90)
#%.o: %.f90
#	$(F90) $(F90FLAGS) $(EXTRA_FFLAGS) -c $< -o $@
#endif
