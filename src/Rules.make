# Makefile rules applying to the entire MOSSCO src and examples directories

# 1. Importing all FABM-related environment variables and checking that the environment is sane
#    At the moment, we require that FABMDIR, FABMHOST, and FORTRAN_COMPILER are set and that
#    the fabm library is installed in the production version (libfabm_prod)
# 
ifndef FABMDIR
$(error FABMDIR needs to be defined)
endif

ifndef FABMHOST
$(error FABMHOST needs to be defined as FABMHOST=mossco)
endif

ifndef FORTRAN_COMPILER
$(error FORTRAN_COMPILER needs to be set to one of the compilers in $(FABMDIR)/compilers)
endif

FABM_MODULE_PATH=$(FABMDIR)/modules/$(FABMHOST)/$(FORTRAN_COMPILER)
FABM_INCLUDE_PATH=$(FABMDIR)/include
FABM_LIBRARY_PATH=$(FABMDIR)/lib/$(FABMHOST)/$(FORTRAN_COMPILER)
FABM_LIBRARY_FILE=$(FABM_LIBRARY_PATH)/libfabm_prod.a
export FABM_F2003=true

# 2. ESMF stuff, only if ESMFMKFILE is declared.  We need to work on an intelligent system that prevents
#    the components and mediators to be built if ESMF is not found in your system
#
ifndef ESMFMKFILE
$(warning Compiling without ESMF support)
else
include $(ESMFMKFILE)
endif

# 3. MOSSCO declarations. The MOSSCODIR and the build prefix are set, as well as the bin/mod/lib paths relative
#    to the PREFIX
#
ifndef MOSSCODIR
MOSSCODIR=$(subst FABM,MOSSCO,$(FABMDIR))
export MOSSCODIR:=$(subst fabm,mossco,$(MOSSCODIR))
$(warning MOSSCODIR=$(MOSSCODIR) automatically determined from FABMDIR)
endif

ifeq ($(wildcard $(MOSSCODIR)),) 
$(error the directory MOSSCODIR=$(MOSSCODIR) does not exist)
endif

ifdef PREFIX
MOSSCOPREFIX=$(PREFIX)
else
MOSSCOPREFIX=$(MOSSCODIR)
endif

MOSSCO_MODULE_PATH=$(MOSSCOPREFIX)/modules/$(FABMHOST)/$(FORTRAN_COMPILER)
MOSSCO_LIBRARY_PATH=$(MOSSCOPREFIX)/lib/$(FABMHOST)/$(FORTRAN_COMPILER)
MOSSCO_BIN_PATH=$(MOSSCOPREFIX)/bin

# 4. Putting everything together.  This section could need some cleanup, but does work fornow
#
ifndef F90
ifdef ESMF_F90COMPILER)
F90=$(ESMF_F90COMPILER)
$(warning F90 automatically determined from ESMF_F90COMPILER variable: F90=$(F90))
else
ifdef FCA
F90=$(FC)
$(warning F90 automatically determined from FC variable: F90=$(F90))
else
F90=$(shell grep 'FC=' $(FABMDIR)/compilers/compiler.$(FORTRAN_COMPILER) | cut -d"=" -f2)
$(warning F90 automatically determined from FABM environment: F90=$(F90))
endif
endif
endif
export F90

INCLUDES  = -I$(FABM_INCLUDE_PATH) -I$(FABM_MODULE_PATH) -I$(FABMDIR)/src/drivers/$(FABMHOST)
INCLUDES += $(ESMF_F90COMPILEPATHS)
INCLUDES += -I$(MOSSCO_MODULE_PATH)
ifeq ($(FORTRAN_COMPILER),GFORTRAN)
INCLUDES += -J$(MOSSCO_MODULE_PATH)
else
ifeq ($(FORTRAN_COMPILER),IFORT)
INCLUDES += -module $(MOSSCO_MODULE_PATH)
endif
endif

LIBRARY_PATHS  = -L$(FABM_LIBRARY_PATH) 
LIBRARY_PATHS += $(ESMF_F90LINKPATHS) $(ESMF_F90ESMFLINKRPATHS) 
LIBRARY_PATHS += -L$(MOSSCO_LIBRARY_PATH)
LIBS = -lfabm_prod
LIBS += $(ESMF_F90LINKLIBS)

CPPFLAGS = $(DEFINES) $(INCLUDES) $(ESMF_F90COMPILECPPFLAGS)
LDFLAGS = $(ESMF_F90LINKOPTS)

# Make targets
.PHONY: default all clean doc info

default: prefix all

clean:
	@rm -f *.o

veryclean: clean
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
#ifeq  ($(can_do_F90),true)
%.o: %.F90
	@ echo "Compiling $<"
	$(F90) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@
#else
#%.f90: %.F90
#	$(CPP) $(CPPFLAGS) $< -o $@
	#$(F90_to_f90)
#%.o: %.f90
	#$(F90) $(F90FLAGS) $(EXTRA_FFLAGS) -c $< -o $@
#endif
