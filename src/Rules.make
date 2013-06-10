# Makefile rules applying to the entire MOSSCO src and examples directories

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

# MOSSCO declarations
export MOSSCODIR=$(PWD)/..
MOSSCO_MODULE_PATH=$(MOSSCODIR)/src/modules/$(FABMHOST)/$(FORTRAN_COMPILER)
MOSSCO_LIBRARY_PATH=$(MOSSCODIR)/src/lib/$(FABMHOST)/$(FORTRAN_COMPILER)

# Putting it together
INCLUDES = -I$(FABM_INCLUDE_PATH) -I$(FABM_MODULE_PATH) -I$(FABMDIR)/src/drivers/mossco -I$(MOSSCO_MODULE_PATH)
ifeq (FORTRAN_COMPILER,GFORTRAN)
INCLUDES += -J$(MOSSCO_MODULE_PATH)
endif

LIBRARY_PATHS = -L$(FABM_LIBRARY_PATH) -L$(MOSSCO_LIBRARY_PATH)
LIBS = -lfabm_prod

# Make targets
.PHONY: default all clean doc

