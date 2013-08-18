ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/utilities$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

.PHONY: libsolver

# Make targets

all: libsolver

libsolver: prefix $(MOSSCO_MODULE_PATH)/solver_library.mod solver_library.o
	ar cruvs $(MOSSCO_LIBRARY_PATH)/libsolver.a solver_library.o
	@echo "Created solver library  $(MOSSCO_LIBRARY_PATH)/libsolver.a" 

$(MOSSCO_MODULE_PATH)/solver_library.mod: solver_library.o

clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libsolver.a
	@rm -f $(MOSSCO_MODULE_PATH)/solver_library.mod
