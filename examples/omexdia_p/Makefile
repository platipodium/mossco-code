ifndef FABMDIR
$(error FABMDIR needs to be defined)
endif

export MOSSCODIR=`pwd`/../..
export FABM_F2003=true

MODULE_DIR = $(MOSSCODIR)/src/modules/$(FABMHOST)/$(FORTRAN_COMPILER)
INCLUDE_DIRS = -I$(MODULE_DIR) -I$(FABMDIR)/include -I$(FABMDIR)/src/drivers/mossco -I$(FABMDIR)/modules/$(FABMHOST)/$(FORTRAN_COMPILER) -I$(MODULE_DIR) -J$(MODULE_DIR)
LIB_DIRS = -L$(FABMDIR)/lib/$(FABMHOST)/$(FORTRAN_COMPILER) -L$(MOSSCODIR)/lib
LIBS += -lsediment -lfabm_prod

F90FLAGS += $(INCLUDE_DIRS)
LDFLAGS += $(LIB_DIRS) $(LIBS)

.PHONY: all sedimentlib fabmdriver exec default

default: all

all: exec

fabmlib:
	( export FABMHOST=mossco ; $(MAKE) -C $(FABMDIR)/src/ )

fabmdriver: fabmlib #$(MOSSCODIR)/src/lib/libsediment.a
	( export FABMHOST=mossco ; $(MAKE) -C $(MOSSCODIR)/src/drivers)
	mkdir -p $(MOSSCODIR)/src/lib
	ar cruv  $(MOSSCODIR)/src/lib/libsediment.a $(MOSSCODIR)/src/drivers/fabm_sediment_driver.o

exec: fabmdriver
	$(FC) $(F90FLAGS) $(LDFLAGS) -o omexdia_p_test main.F90 

