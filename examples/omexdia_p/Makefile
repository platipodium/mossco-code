# FABM stuff
ifndef FABMDIR
$(error FABMDIR needs to be defined)
endif

ifndef FABMHOST
$(error FABMHOST needs to be defined as FABMHOST=mossco)
endif

ifndef FORTRAN_COMPILER
$(error FORTRAN_COMPILER needs to be set to the FABM compatible compiler.FORTRAN_COMPILER file)
endif

FABM_MODULE_PATH=$(FABMDIR)/modules/$(FABMHOST)/$(FORTRAN_COMPILER)
FABM_INCLUDE_PATH=$(FABMDIR)/include
FABM_LIBRARY_PATH=$(FABMDIR)/lib/$(FABMHOST)/$(FORTRAN_COMPILER)
FABM_LIBRARY_FILE=$(FABM_LIBRARY_PATH)/libfabm_prod.a
export FABM_F2003=true

# MOSSCO declarations

export MOSSCODIR=$(PWD)/../..
MOSSCO_MODULE_PATH=$(MOSSCODIR)/src/modules/$(FABMHOST)/$(FORTRAN_COMPILER)
MOSSCO_LIBRARY_PATH=$(MOSSCODIR)/src/lib/$(FABMHOST)/$(FORTRAN_COMPILER)

# Putting it together
INCLUDES = -I$(FABM_INCLUDE_PATH) -I$(FABM_MODULE_PATH) -I$(FABMDIR)/src/drivers/mossco -I$(MOSSCO_MODULE_PATH)
ifeq (FORTRAN_COMPILER,GFORTRAN)
INCLUDES += -J$(MOSSCO_MODULE_PATH)
endif

LIBRARY_PATHS = -L$(FABM_LIBRARY_PATH) -L$(MOSSCO_LIBRARY_PATH)
LIBS += -lsediment -lfabm_prod

F90FLAGS += $(INCLUDES)
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: all sedimentlib fabmdriver exec default

default: all

all: exec

fabmlib: $(FABM_LIBRARY_FILE)

$(FABM_LIBRARY_FILE):
	( export FABMHOST=mossco ; $(MAKE) -C $(FABMDIR)/src/ )

fabmdriver: fabmlib $(MOSSCO_LIBRARY_PATH)/libsediment.a

$(MOSSCO_LIBRARY_PATH)/libsediment.a: $(MOSSCODIR)/src/drivers/fabm_sediment_driver.F90
	( export FABMHOST=mossco ; $(MAKE) -C $(MOSSCODIR)/src/drivers)
	mkdir -p $(MOSSCO_LIBRARY_PATH)
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libsediment.a $(MOSSCODIR)/src/drivers/fabm_sediment_driver.o
	ranlib $(MOSSCO_LIBRARY_PATH)/libsediment.a 

exec: fabmdriver
	$(FC) $(F90FLAGS) $(LDFLAGS) -o omexdia_p_test main.F90 

