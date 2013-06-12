EXTRA_DIST=README

include ../Rules.make
#--------------------------------------
# Make targets

default: all

all: libsediment

libsediment: libsediment.a

libsediment.a: libfabm fabm_sediment_driver.o
	$(AR) cruvs $@ fabm_sediment_driver.o
	mkdir -p $(MOSSCO_LIBRARY_PATH)
	#mkdir -p $(MOSSCO_MODULE_PATH)
	mv $@ $(MOSSCO_LIBRARY_PATH)
	#mv fabm_sediment_driver.mod $(MOSSCO_MODULE_PATH)

libfabm:
	( export FABMHOST=$(FABMHOST) ; $(MAKE) -C $(FABMDIR)/src/ )

info:
	@echo SHELL = $(SHELL)
	@echo MAKE = $(MAKE)
	@echo FORTRAN_COMPILER = $(FORTRAN_COMPILER)
	@echo F90 = $(F90)
	@echo FABMHOST = $(FABMHOST)
	@echo FABMDIR = $(FABMDIR)
	@echo MOSSCODIR = $(MOSSCODIR)
	@echo INCDIRS = $(INCDIRS)
	@echo F90FLAGS = $(F90FLAGS)

clean:
	@rm -f *.o *.mod

