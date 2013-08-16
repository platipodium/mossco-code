ifndef MOSSCODIR
export MOSSCODIR=$(subst /examples/esmf_fabm0d$,,$(PWD))
endif

include $(MOSSCODIR)/src/Rules.make

EXTRA_DIST=README

LIBS := -lesmf $(LIBS)
NC_LIBS += $(shell nf-config --flibs)

LDFLAGS += $(LIBRARY_PATHS) $(LIBS) -lfabm0d $(FABMDIR)/src/drivers/0d/fabm0d.o -L$(GOTMDIR)/lib/GFORTRAN -lairsea_prod -linput_prod -lutil_prod $(FABMDIR)/lib/0d/GFORTRAN/libfabm_prod.a 

.PHONY: all exec libfabm0d

all: exec

libfabm0d: prefix $(MOSSCO_MODULE_PATH)/esmf_fabm_0d_component.mod $(MOSSCODIR)/src/components/esmf_fabm_0d_component.o
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libfabm0d.a $(MOSSCODIR)/src/components/esmf_fabm_0d_component.o
	@echo "Created fabm0d library  $(MOSSCO_LIBRARY_PATH)/libfabm0d.a" 

exec: main.o libfabm0d
	$(F90) $(F90FLAGS) -o esmf_fabm0d esmf_toplevel_component.o main.o $(LDFLAGS)
	@echo "Created example binary esmf_fabm0d in  $(PWD)" 
	@echo "Please change to $(FABMDIR)/testcases/0d to run the example." > $(MOSSCO_BIN_PATH)/esmf_fabm0d.txt

$(MOSSCO_MODULE_PATH)/esmf_fabm_0d_component.mod: $(MOSSCODIR)/src/components/esmf_fabm_0d_component.F90
	$(MAKE) -C $(MOSSCODIR)/src/components

main.o: esmf_toplevel_component.o
esmf_toplevel_component.o: esmf_toplevel_component.F90

clean: extraclean

extraclean: 
	@rm -f esmf_fabm0d
	@rm -f PET*.*
