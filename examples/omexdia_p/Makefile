include ../../src/Rules.make

LIBS := -lsediment -lsolver $(LIBS)

F90FLAGS += $(INCLUDES)
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: libsediment fabmdriver exec

all: exec

fabmlib: $(FABM_LIBRARY_FILE)

$(FABM_LIBRARY_FILE):
	(export FABMHOST=mossco ; $(MAKE) -C $(FABMDIR) )
	(export FABMHOST=mossco ; $(MAKE) -C $(FABMDIR)/src )

fabmdriver: fabmlib libsediment libsolver

libsediment: prefix $(MOSSCO_MODULE_PATH)/fabm_sediment_driver.mod $(MOSSCODIR)/src/drivers/fabm_sediment_driver.o
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libsediment.a $(MOSSCODIR)/src/drivers/fabm_sediment_driver.o
	@echo "Created sediment library  $(MOSSCO_LIBRARY_PATH)/libsediment.a" 

$(MOSSCODIR)/src/drivers/fabm_sediment_driver.o $(MOSSCO_MODULE_PATH)/fabm_sediment_driver.mod: 
	$(MAKE) -C $(MOSSCODIR)/src/drivers

libsolver:
	$(MAKE) -C $(MOSSCODIR)/src/utilities

exec: fabmdriver main.F90
	$(F90) $(F90FLAGS) -c -o omexdia_p_test.o main.F90 
	$(F90) -o omexdia_p_test omexdia_p_test.o $(LDFLAGS)
	@echo "Created example binary omexdia_p_test in  $(PWD)" 
	@echo "Please change to $(MOSSCODIR)/examples/omexdia_p to run the example." > $(MOSSCO_BIN_PATH)/omexdia_p.txt

clean: extraclean

extraclean: 
	@rm -f output.dat omexdia_p_test
