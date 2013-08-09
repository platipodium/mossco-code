include ../../src/Rules.make

LIBS := -lesmf -lgotm $(LIBS)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS) -L$(GOTMDIR)/lib/GFORTRAN -lgotm_prod -lairsea_prod -lmeanflow_prod -lseagrass_prod -lturbulence_prod -lutil_prod -lgotm_fabm_prod -lutil_prod -loutput_prod -lobservations_prod  -linput_prod $(FABMDIR)/lib/gotm/GFORTRAN/libfabm_prod.a -lnetcdff -lnetcdf_c++ -lnetcdf 


.PHONY: all exec libgotm

all: exec



libgotm: prefix $(MOSSCO_MODULE_PATH)/esmf_gotm_component.mod $(MOSSCODIR)/src/components/esmf_gotm_component.o
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libgotm.a $(MOSSCODIR)/src/components/esmf_gotm_component.o
	@echo "Created gotm library  $(MOSSCO_LIBRARY_PATH)/libgotm.a" 

exec: main.o libgotm
	$(F90) $(F90FLAGS) -o esmf_gotm esmf_toplevel_component.o main.o $(LDFLAGS)
	@echo "Created example binary esmf_example_test in  $(PWD)" 
	@echo "Please change to $(MOSSCODIR)/examples/esmf_gotm to run the example." > $(MOSSCO_BIN_PATH)/esmf_gotm.txt

$(MOSSCO_MODULE_PATH)/esmf_gotm_component.mod: $(MOSSCODIR)/src/components/esmf_gotm_component.F90
	$(MAKE) -C $(MOSSCODIR)/src/components

main.o: esmf_toplevel_component.o
esmf_toplevel_component.o: esmf_toplevel_component.F90

clean: extraclean

extraclean: 
	@rm -f esmf_gotm_example
