# Makefile to create a MOSSCO-coupled SCHISM application
# @author Richard Hofmeister <richard.hofmeister@hzg.de>
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
# @copyright Helmholtz-Zentrum Geesthacht
# @license Apache License 2.0

# include your ESMF Makefile (esmf.mk)
ifndef ESMFMKFILE
	$(error ESMFMKFILE has to be set in environment)
endif
include $(ESMFMKFILE)

# add SCHISM, PARMETIS and MOSSCO directories
ifndef SCHISM_DIR
$(error SCHISM_DIR has to be set in environment)
endif
ifndef PARMETIS_DIR
$(error PARMETIS_DIR has to be set in environment)
endif
ifndef MOSSCO_DIR
$(error MOSSCO_DIR has to be set in environment)
endif

F90=$(ESMF_F90COMPILER)
LIBS=$(ESMF_F90ESMFLINKLIBS)
CPPFLAGS=$(ESMF_F90COMPILEOPTS)
F90FLAGS=$(ESMF_F90COMPILEPATHS)
LDFLAGS+=$(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS)

LIBS+= -L$(SCHISM_DIR)/lib -lhydro -lcore
LIBS+= -L$(PARMETIS_DIR) -lparmetis -lmetis
LIBS+= -L$(MOSSCO_DIR)/lib -lmossco 
F90FLAGS+= -I$(SCHISM_DIR)/include
F90FLAGS+= -I$(MOSSCO_DIR)/include

.PHONY: all clean distclean config

all:  schism_mossco config

schism_mossco:  schism_esmf_component.o toplevel_component.o main.o
	$(F90) $(CPPFLAGS) $^ -o $@ $(LDFLAGS) $(LIBS)

toplevel_component.o: schism_esmf_component.o

schism_esmf_component.o: schism_driver_interfaces.mod

# Generic and cleanup rules

%.o: %.F90
	$(F90) $(CPPFLAGS) $(F90FLAGS) -c $<

%.mod: %.F90
	$(F90) $(CPPFLAGS) $(F90FLAGS) -c $<

clean:
	@$(RM) *.o *.mod PET* fort.* *output.nc
	@$(RM) -rf *dSYM

config:  grid_input.cfg wind_input.cfg wind.nc

distclean: clean
	@$(RM) schism_mossco schism_driver_interfaces.F90 schism_esmf_component.F90 main.F90

# Copy/link files from schism_esmf and mossco systems

main.F90:
	ln -sf $(MOSSCO_DIR)/examples/common/main.F90 .

schism_driver_interfaces.F90:
	ln -sf ../esmf/schism_driver_interfaces.F90 .

schism_esmf_component.F90:
	ln -sf ../esmf/schism_esmf_component.F90 .

wind.nc: wind.cdl
	ncgen -o $@ $<
