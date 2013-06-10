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

include $(ESMFMKFILE)
F90=$(ESMF_F90COMPILER)
FC=$(F90)

FABMLIBDIR=$(FABMDIR)/lib/$(FABMHOST)/$(FORTRAN_COMPILER)
FABMMODDIR=$(FABMDIR)/modules/$(FABMHOST)/$(FORTRAN_COMPILER)

INCDIRS   += -I$(FABMDIR)/include -I$(FABMMODDIR) -I$(MOSSCOMODDIR) -I$(FABMDIR)/src/drivers/$(FABMHOST) $(ESMF_F90COMPILEPATHS)
LINKDIRS  += -L$(FABMLIBDIR) $(EMSF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) 

CPPFLAGS   = $(DEFINES) $(INCDIRS) $(ESMF_F90COMPILECPPFLAGS)
F90FLAGS    = $(DEFINES) $(INCDIRS) $(ESMF_F90COMPILEOPTS) 
LDFLAGS   += $(ESMF_F90LINKOPTS) $(LINKDIRS) 
LIBS      += -lfabm_prod -lsediment $(ESMF_F90ESMFLINKLIBS)

include $(ESMFMKFILE)

.PHONY: default all
.SUFFIXES: .F90

#--------------------------------------
# Make targets
# add checks for libfabm and libsediment

default: all

all:

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
	@rm -f *.o *.mod *.swp

#-------------------------
# Generic rules 	

%.o: %.F90
	$(F90) $(F90FLAGS) -c $< -o $@
