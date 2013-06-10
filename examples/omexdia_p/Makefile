include ../../src/Rules.make

LIBS += -lsediment

F90FLAGS += $(INCLUDES)
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: sedimentlib fabmdriver exec

default: all

all: exec

fabmlib: $(FABM_LIBRARY_FILE)

$(FABM_LIBRARY_FILE):
	( export FABMHOST=mossco ; $(MAKE) -C $(FABMDIR)/src/ )

fabmdriver: fabmlib $(MOSSCO_LIBRARY_PATH)/libsediment.a

$(MOSSCO_LIBRARY_PATH)/libsediment.a: 
	( export FABMHOST=mossco ; $(MAKE) -C $(MOSSCODIR)/src/drivers)
	mkdir -p $(MOSSCO_LIBRARY_PATH)
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libsediment.a $(MOSSCODIR)/src/drivers/fabm_sediment_driver.o
	ranlib $(MOSSCO_LIBRARY_PATH)/libsediment.a 

exec: fabmdriver
	$(FC) $(F90FLAGS) $(LDFLAGS) -o omexdia_p_test main.F90 

info:
	echo MOSSCODIR=$(MOSSCODIR)
