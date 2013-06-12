include ../Rules.make

EXTRA_DIST=README

#--------------------------------------
# Make targets
# add checks for libfabm and libsediment

default: all

all: esmf_fabm_sediment_component.o 

esmf_fabm_sediment_component.o: esmf_fabm_sediment_component.F90

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
	@echo LDFLAGS = $(LDFLAGS)
	@echo LINKDIRS = $(LINKDIRS)

clean:
	@rm -f *.o *.mod 

