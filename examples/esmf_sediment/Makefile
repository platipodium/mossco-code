ifndef MOSSCODIR
MOSSCODIR=$(subst /examples/esmf_sediment$,,$(PWD))
endif

include $(MOSSCODIR)/src/Rules.make

LIBS := -lesmf -lsediment -lsolver $(LIBS)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec libsediment

all: exec

exec: main.o libsediment esmf_toplevel_component.o
	$(F90) $(F90FLAGS) -o esmf_sediment_example esmf_toplevel_component.o main.o $(LDFLAGS)
	@echo "Created example binary esmf_example_test in  $(PWD)" 
	@echo "Please change to $(MOSSCODIR)/examples/esmf_sediment to run the example." > $(MOSSCO_BIN_PATH)/esmf_sediment.txt

libsediment: prefix $(MOSSCO_MODULE_PATH)/esmf_fabm_sediment_component.mod $(MOSSCODIR)/src/components/esmf_fabm_sediment_component.o
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libsediment.a $(MOSSCODIR)/src/components/esmf_fabm_sediment_component.o
	@echo "Created sediment library  $(MOSSCO_LIBRARY_PATH)/libsediment.a" 
	
$(MOSSCO_MODULE_PATH)/esmf_toplevel_component.mod: $(MOSSCODIR)/src/components/esmf_fabm_sediment_component.F90
	$(MAKE) -C $(MOSSCODIR)/src/components

main.o: esmf_toplevel_component.o
esmf_toplevel_component.o: esmf_toplevel_component.F90 $(MOSSCO_MODULE_PATH)/esmf_fabm_sediment_component.mod
$(MOSSCO_MODULE_PATH)/esmf_fabm_sediment_component.mod:
	$(MAKE) -C $(MOSSCODIR)/src/components

clean: extraclean

extraclean: 
	@rm -f esmf_sediment_example
