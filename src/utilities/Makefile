include ../Rules.make

EXTRA_DIST=README

.PHONY: libsolver libsediment

# Make targets

all: libsolver

libsolver: libsediment $(MOSSCO_MODULE_PATH)/solver_library.mod solver_library.o
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libsolver.a solver_library.o
	@echo "Created solver library  $(MOSSCO_LIBRARY_PATH)/libsolver.a" 

libsediment:
	$(MAKE) -C $(MOSSCODIR)/src/drivers libsediment


$(MOSSCO_MODULE_PATH)/solver_library.mod: solver_library.o

clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libsolver.a
	@rm -f $(MOSSCO_MODULE_PATH)/solver_library.mod
