EXTRA_DIST=README

ifndef FABMDIR
$(error FABMDIR needs to be defined)
endif

ifndef FORTRAN_COMPILER
$(error FORTRAN_COMPILER needs to be defined)
endif

MOSSCODIR=`pwd`/..
MOSSCOLIBDIR=$(MOSSCODIR)/lib/$(FABMHOST)/$(FORTRAN_COMPILER)
MOSSCOMODDIR=$(MOSSCODIR)/modules/$(FABMHOST)/$(FORTRAN_COMPILER)
MOSSCOBINDIR=$(MOSSCODIR)/bin

# TODO: test for existence of lib/bin dirs and create them

FABMLIBDIR=$(FABMDIR)/lib/$(FABMHOST)/$(FORTRAN_COMPILER)
FABMMODDIR=$(FABMDIR)/modules/$(FABMHOST)/$(FORTRAN_COMPILER)

INCDIRS   += -I$(FABMDIR)/include -I$(FABMMODDIR) -I$(MOSSCOMODDIR)
LINKDIRS  += -L$(FABMLIBDIR)

CPPFLAGS   = $(DEFINES) $(INCDIRS)
FFLAGS     = $(DEFINES) $(INCDIRS) -J$(MOSSCOMODDIR)
F90FLAGS   = $(FFLAGS)
LDFLAGS   += $(FFLAGS) $(LINKDIRS)
LIBS      += -lfabm_prod

.PHONY: default all
.SUFFIXES: .F90

#--------------------------------------
# Make targets

default: all

all: libsediment

libsediment: libsediment.a

libsediment.a: libfabm fabm_sediment_driver.o
	$(AR) cruvs $@ fabm_sediment_driver.o
	mkdir -p $(MOSSCOLIBDIR)
	mv $@ $(MOSSCOLIBDIR)
	#mkdir -p $(MOSSCOMODDIR)
  	#mv fabm_sediment_driver.mod $(MOSSCOMODDIR)

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
	@rm -f *.o *.mod *.swp

#-------------------------
# Generic rules 	

%.o: %.F90
	$(F90) $(F90FLAGS) -c $< -o $@
