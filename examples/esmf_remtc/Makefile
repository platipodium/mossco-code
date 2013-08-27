ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/esmf_remtc$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lesmf -locean -latmosphere $(LIBS)
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: all exec

all: exec

exec: main.o esmf_toplevel_component.o libocean libatmosphere
	$(F90) $(F90FLAGS) -o esmf_remtc esmf_toplevel_component.o main.o $(LDFLAGS)
	@echo "Created example binary esmf_remtc in $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/esmf_remtc/ to run the example." > $(MOSSCO_BIN_PATH)/esmf_remtc.txt

libocean: $(MOSSCO_LIBRARY_PATH)/libocean.a
libatmosphere: $(MOSSCO_LIBRARY_PATH)/libatmosphere.a

$(MOSSCO_LIBRARY_PATH)/libatmosphere.a:
	$(MAKE) -C $(MOSSCO_DIR)/src/components libatmosphere
	
$(MOSSCO_LIBRARY_PATH)/libocean.a:
	$(MAKE) -C $(MOSSCO_DIR)/src/components libocean
	
main.o: esmf_toplevel_component.o
esmf_toplevel_component.o: esmf_toplevel_component.F90

clean: extraclean

extraclean: 
	@rm -f esmf_remtc
