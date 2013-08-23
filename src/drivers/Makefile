ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/drivers$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

DELFT_FOREIGN_SOURCES=$(DELFT_DIR)/modules/precision_basics.f90 $(DELFT_DIR)/modules/precision.f90 $(wildcard $(DELFT_DIR)/source/*.f90) $(DELFT_DIR)/include/sedparams.inc

DELFT_OBJS    = $(DELFT_LOCAL_SOURCES:.f90=.o) 

.PHONY: libsediment libfabm liberosed libdelft localsources

# Make targets

ALL_TARGETS =

ifeq ($(MOSSCO_FABM),true)
ALL_TARGETS=libsediment
endif

ifeq ($(MOSSCO_DELFT),true)
ALL_TARGETS += localsources liberosed
endif

all: $(ALL_TARGETS)


# FABM/sediment section

libsediment: prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a
libfabm: $(FABM_LIBRARY_PATH)/$(FABM_LIBRARY_FILE)

$(MOSSCO_LIBRARY_PATH)/libsediment.a: libsolver libfabm fabm_sediment_driver.o
	$(AR) crus $@ fabm_sediment_driver.o

$(FABM_LIBRARY_PATH)/$(FABM_LIBRARY_FILE):
	@echo Recreating the FABM library $(FABM_LIBRARY_FILE)
	$(MAKE) -C $(FABMDIR)
	$(MAKE) -C $(FABMDIR)/src
	
libsolver: $(MOSSCO_LIBRARY_PATH)/libsolver.a

$(MOSSCO_LIBRARY_PATH)/libsolver.a:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities libsolver

# Delft section
localsources: $(DELFT_FOREIGN_SOURCES)
	for F in $(DELFT_FOREIGN_SOURCES) ; do ln -sf $$F; done

libdelft:  prefix $(MOSSCO_LIBRARY_PATH)/libmossco_delft.a
liberosed: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a

$(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a: libdelft delft_erosed_driver.o
	$(AR) crus $@ delft_erosed_driver.o

$(MOSSCO_LIBRARY_PATH)/libmossco_delft.a: $(DELFT_OBJS)
	$(AR) crus $(MOSSCO_LIBRARY_PATH)/libmossco_delft.a $(DELFT_OBJS) 

# Generic section


clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libsediment.a
	@rm -f $(MOSSCO_MODULE_PATH)/fabm_sediment_driver.mod
	@rm -f $(MOSSCO_MODULE_PATH)/delft_erosed_driver.mod
