include ../../src/Rules.make

LIBS := -lesmf $(LIBS)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: 

default: all

all: exec


exec: main.o 
	$(F90) $(F90FLAGS) -o esmf_sediment_example main.o esmf_fabm_seidment_component.o $(LDFLAGS)


main.o: esmf_toplevel_component.o
esmf_toplevel_component.o: esmf_toplevel_component.F90 $(MOSSCO_MODULE_PATH)/esmf_fabm_sediment_component.mod
$(MOSSCO_MODULE_PATH)/esmf_fabm_sediment_component.mod:
	make -C $(MOSSCODIR)/src/components


