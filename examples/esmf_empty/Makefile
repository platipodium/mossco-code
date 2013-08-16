ifndef MOSSCODIR
export MOSSCODIR=$(subst /examples/esmf_empty$,,$(PWD))
endif

include $(MOSSCODIR)/src/Rules.make

LIBS := -lesmf
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec

all: exec

exec: main.o esmf_toplevel_component.o
	$(F90) $(F90FLAGS) -o esmf_empty esmf_toplevel_component.o main.o $(LDFLAGS)
	@echo "Created example binary esmf_empty in $(PWD)" 
	@echo "Please change to $(MOSSCODIR)/examples/esmf_empty/ to run the example." > $(MOSSCO_BIN_PATH)/esmf_empty.txt
	
main.o: esmf_toplevel_component.o
esmf_toplevel_component.o: esmf_toplevel_component.F90

clean: extraclean

extraclean: 
	@rm -f esmf_empty