EXTRA_DIST=README

include ../Rules.make
#--------------------------------------
# Make targets

all: libsediment

libsediment: libsediment.a

libsediment.a: libfabm fabm_sediment_driver.o
	$(AR) cruvs $@ fabm_sediment_driver.o
	mkdir -p $(MOSSCO_LIBRARY_PATH)
	mkdir -p $(MOSSCO_MODULE_PATH)
	mv $@ $(MOSSCO_LIBRARY_PATH)

libfabm:
	( export FABMHOST=$(FABMHOST) ; $(MAKE) -C $(FABMDIR)/src/ )

