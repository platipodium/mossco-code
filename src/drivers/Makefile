include ../Rules.make

EXTRA_DIST=README

# Make targets

all: libsediment

libsediment: prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a

$(MOSSCO_LIBRARY_PATH)/libsediment.a: libfabm fabm_sediment_driver.o
	$(AR) cruvs $@ fabm_sediment_driver.o

libfabm: $(FABM_LIBRARY_FILE)

$(FABM_LIBRARY_FILE):
	$(MAKE) -C $(FABMDIR)
	$(MAKE) -C $(FABMDIR)/src

clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libsediment.a
	@rm -f $(MOSSCO_MODULE_PATH)/fabm_sediment_driver.mod
