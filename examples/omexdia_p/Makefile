include ../../src/Rules.make

LIBS := -lsediment $(LIBS)

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

exec: fabmdriver main.F90
	$(F90) $(F90FLAGS) -c -o omexdia_p_test.o main.F90 
	$(F90) -o omexdia_p_test omexdia_p_test.o $(LDFLAGS)

clean: extraclean

extraclean: 
	@rm -f output.dat omexdia_p_test
