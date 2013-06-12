include ../../src/Rules.make

LIBS := -lsediment $(LIBS)

F90FLAGS += $(INCLUDES)
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: libsediment fabmdriver exec

all: exec

fabmlib: $(FABM_LIBRARY_FILE)

$(FABM_LIBRARY_FILE):
	( export FABMHOST=mossco ; $(MAKE) -C $(FABMDIR)/src/ )

fabmdriver: fabmlib libsediment

libsediment: prefix $(MOSSCO_MODULE_PATH)/fabm_sediment_driver.mod $(MOSSCODIR)/src/drivers/fabm_sediment_driver.o
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libsediment.a $(MOSSCODIR)/src/drivers/fabm_sediment_driver.o

$(MOSSCODIR)/src/drivers/fabm_sediment_driver.o: 
	$(MAKE) -C $(MOSSCODIR)/src/drivers)

exec: fabmdriver main.F90
	$(F90) $(F90FLAGS) -c -o omexdia_p_test.o main.F90 
	$(F90) -o omexdia_p_test omexdia_p_test.o $(LDFLAGS)

clean: extraclean

extraclean: 
	@rm -f output.dat omexdia_p_test
