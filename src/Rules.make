# Makefile rules applying to the entire MOSSCO src and examples directories

SHELL=/bin/sh

# 1. Importing all FABM-related environment variables and checking that the environment is sane
ifndef FABMDIR
$(error FABMDIR needs to be defined)
endif

ifndef FABMHOST
$(error FABMHOST needs to be defined as FABMHOST=mossco)
endif

ifndef FORTRAN_COMPILER
$(error FORTRAN_COMPILER needs to be set to the FABM compatible $(FABMDIR) compiler.FORTRAN_COMPILER file)
endif

FABM_MODULE_PATH=$(FABMDIR)/modules/$(FABMHOST)/$(FORTRAN_COMPILER)
FABM_INCLUDE_PATH=$(FABMDIR)/include
FABM_LIBRARY_PATH=$(FABMDIR)/lib/$(FABMHOST)/$(FORTRAN_COMPILER)
FABM_LIBRARY_FILE=$(FABM_LIBRARY_PATH)/libfabm_prod.a
export FABM_F2003=true

# ESMF stuff, only if ESMFMKFILE is declared
ifndef ESMFMKFILE
$(warning Compiling without ESMF support)
else
include $(ESMFMKFILE)
endif

# MOSSCO declarations

ifndef MOSSCODIR
export MOSSCODIR=$(HOME)/opt/src/mossco-code
endif

ifeq ($(wildcard $(MOSSCODIR)),) 
$(error MOSSCODIR needs to be defined in src/Rules.make)
endif

ifdef PREFIX)
MOSSCOPREFIX=$(PREFIX)
else
MOSSCOPREFIX=$(MOSSCODIR)
endif

MOSSCO_MODULE_PATH=$(MOSSCOPREFIX)/modules/$(FABMHOST)/$(FORTRAN_COMPILER)
MOSSCO_LIBRARY_PATH=$(MOSSCOPREFIX)/lib/$(FABMHOST)/$(FORTRAN_COMPILER)
MOSSCO_BIN_PATH=$(MOSSCOPREFIX)/bin

#if [ -a $(MOSSCO_LIBRARY_PATH)] ; then : else mkdir -p  $(MOSSCO_LIBRARY_PATH); fi
#ifeq ($(wildcard $(MOSSCO_LIBRARY_PATH)),) 
#	mkdir -p $(MOSSCO_LIBRARY_PATH)
#endif

# Putting it together
INCLUDES  = -I$(FABM_INCLUDE_PATH) -I$(FABM_MODULE_PATH) -I$(FABMDIR)/src/drivers/mossco
INCLUDES += $(ESMF_F90COMPILEPATHS)
INCLUDES += -I$(MOSSCO_MODULE_PATH)
#ifeq (FORTRAN_COMPILER,GFORTRAN)
INCLUDES += -J$(MOSSCO_MODULE_PATH)
#endif

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
