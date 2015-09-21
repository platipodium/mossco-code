# This Makefile is part of MOSSCO
#
# @copyright (C) 2013, 2014, 2015 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
# @author Richard Hofmeister <richard.hofmeister@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
ifndef MOSSCO_DIR
  export MOSSCO_DIR=$(subst /src/drivers$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

TARGETS = libmossco_benthos libmossco_simplewave
ifeq ($(MOSSCO_NETCDF),true)
  TARGETS+=libmossco_clm
  #TARGETS+=libmossco_river
endif

ifeq ($(MOSSCO_FABM),true)
  TARGETS+=libmossco_sediment libmossco_fabmpelagic #libmossco_fabm0d
  ifeq ($(MOSSCO_GOTM),true)
    ifeq ($(MOSSCO_NETCDF),true)
      TARGETS+=libmossco_fabmgotm
    endif
  endif
endif

ifeq ($(MOSSCO_GETM),true)
  TARGETS+=libmossco_getm
endif

ifeq ($(MOSSCO_EROSED),true)
  TARGETS += libmossco_erosed
  CPPFLAGS += -I$(EROSED_DIR)/include
endif

ifeq ($(MOSSCO_TRACER),true)
  TARGETS += libmossco_tracer
  CPPFLAGS += -I$(TRACER_DIR)/src
endif

ifeq ($(MOSSCO_HAMSOM),true)
  TARGETS += libmossco_hamsom
  CPPFLAGS += -I$(HAMSOM_DIR)/src
endif

.PHONY: $(TARGETS)

# Make targets
# Default is to make all of the targets
all: $(TARGETS)

libmossco_simplewave: prefix \
	$(MOSSCO_LIBRARY_PATH)/libmossco_simplewave.a(simplewave_driver.o)

libmossco_river: prefix \
	$(MOSSCO_LIBRARY_PATH)/libmossco_river.a(river_driver.o)

# FABM/sediment section

libmossco_sediment: prefix libmossco_util libsolver libfabm_external \
	$(MOSSCO_LIBRARY_PATH)/libmossco_sediment.a(fabm_sediment_driver.o)

libmossco_fabm0d: prefix libsolver libfabm_external libgotm_external \
	$(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a(fabm0d_driver.o)

libmossco_fabmgotm: prefix libmossco_util libsolver libfabm_external libgotm_external \
	$(MOSSCO_LIBRARY_PATH)/libmossco_fabmgotm.a(fabm_gotm_driver.o)

libmossco_clm: prefix libmossco_clm_driver
libmossco_clm_driver: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_clm.a(clm_driver.o)
libmossco_fabmpelagic: prefix libmossco_util libsolver libfabm_external \
	$(MOSSCO_LIBRARY_PATH)/libmossco_fabmpelagic.a(fabm_pelagic_driver.o)

libmossco_getm: prefix libgetm_external \
	$(MOSSCO_LIBRARY_PATH)/libmossco_getm.a(getm_driver.o)

fabm_sediment_driver.o: fabm_sediment_driver.F90
	@echo "Compiling $<"
	$(F90) $(FABM_CPPFLAGS) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

fabm_pelagic_driver.o: fabm_pelagic_driver.F90
	@echo "Compiling $<"
	$(F90) $(FABM_CPPFLAGS) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

fabm0d_driver.o: fabm0d_driver.F90
	@echo "Compiling $<"
	$(F90) $(GOTM_CPPFLAGS) $(FABM_CPPFLAGS) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

fabm_gotm_driver.o: fabm_gotm_driver.F90
	@echo "Compiling $<"
	$(F90) $(GOTM_CPPFLAGS) $(FABM_CPPFLAGS) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

getm_driver.o: getm_driver.F90
	@echo "Compiling $<"
	$(F90) $(GETM_CPPFLAGS) $(GOTM_CPPFLAGS) $(CPPFLAGS) $(F90FLAGS) -c $< -o $@

# Benthos section
libmossco_benthos: prefix \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(benthoseffect_class.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(biotypes.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(microphytobenthos_class.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(macrofauna_class.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(mbalthica_class.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(macrofauna_interface.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(criticalsheareffect.o) \
	$(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(erodibilityeffect.o)

microphytobenthos_class.o: criticalsheareffect.o benthoseffect_class.o biotypes.o erodibilityeffect.o
mbalthica_class.o: biotypes.o macrofauna_class.o criticalsheareffect.o erodibilityeffect.o
macrofauna_class.o: benthoseffect_class.o biotypes.o
benthoseffect_class.o: biotypes.o
macrofauna_interface.o: mbalthica_class.o biotypes.o
criticalsheareffect.o: macrofauna_class.o biotypes.o
erodibilityeffect.o: macrofauna_class.o biotypes.o

# utilities
libmossco_util libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

# EROSED section
EROSED_OBJS= \
  $(EROSED_DIR)/modules/precision_basics.o  \
  $(EROSED_DIR)/modules/precision.o    \
  $(EROSED_DIR)/modules/mathconsts.o\
  $(EROSED_DIR)/source/erosand.o      \
  $(EROSED_DIR)/source/eromud.o      \
  $(EROSED_DIR)/source/vanRijn84.o    \
  $(EROSED_DIR)/source/shld.o      \
  $(EROSED_DIR)/source/sand_mud.o\
  $(EROSED_DIR)/source/compbsskin.o\
  $(EROSED_DIR)/source/bedbc1993.o\
  $(EROSED_DIR)/source/soursin_3d.o

libmossco_erosed_dependencies: prefix              \
  $(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a(biotypes.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/modules/precision_basics.o)  \
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/modules/precision.o)    \
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/source/mathconsts.o)\
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/source/erosand.o)    \
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/source/eromud.o)    \
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/source/vanRijn84.o)    \
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/source/shld.o)    \
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/source/sand_mud.o)\
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/source/compbsskin.o)\
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/source/bedbc1993.o)\
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a($(EROSED_DIR)/source/soursin_3d.o)


libmossco_erosed: prefix biotypes.o libmossco_erosed_dependencies       \
  $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a(erosed_driver.o)

# TRACER section
# TRACER_OBJS= \
#   $(TRACER_DIR)/src/grid_NWCS20D.o  \
#   $(TRACER_DIR)/src/avr2geo.o  \
#   $(TRACER_DIR)/src/utils.o  \
#   $(TRACER_DIR)/src/forcing_NWCS20D.o  \
#   $(TRACER_DIR)/src/particles.o  \
#   $(TRACER_DIR)/src/traort4.o  \
#   $(TRACER_DIR)/src/output.o \
#   tracer_driver.o

libmossco_tracer_dependencies:              \
  $(MOSSCO_LIBRARY_PATH)/libmossco_tracer.a($(TRACER_DIR)/src/grid_NWCS20D.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_tracer.a($(TRACER_DIR)/src/avr2geo.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_tracer.a($(TRACER_DIR)/src/utils.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_tracer.a($(TRACER_DIR)/src/forcing_NWCS20D.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_tracer.a($(TRACER_DIR)/src/particles.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_tracer.a($(TRACER_DIR)/src/traort4.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_tracer.a($(TRACER_DIR)/src/output.o) \

# libtracer_external: prefix libmossco_tracer_dependencies       \
#   $(MOSSCO_LIBRARY_PATH)/libmossco_tracer.a($(TRACER_OBJS))

# libmossco_tracer: prefix libmossco_tracer_dependencies       \
#   $(MOSSCO_LIBRARY_PATH)/libmossco_tracer.a(tracer_driver.o)

# libmossco_tracer: prefix libmossco_tracer_dependencies  \
#   $(MOSSCO_LIBRARY_PATH)/libmossco_tracer.a(tracer_driver.o $(TRACER_OBJS))

libmossco_tracer: prefix libmossco_tracer_dependencies  \
  $(MOSSCO_LIBRARY_PATH)/libmossco_tracer.a(tracer_driver.o)

tracer_driver.o: tracer_driver.F90
	@echo "Compiling $<"
	$(F90)  $(TRACER_CPPFLAGS) $(CPPFLAGS) $(TRACER_FFLAGS) $(F90FLAGS) -c $< -o $@

# HAMSOM section
# Save general F90FLAGS and add temporarily HAMSOM_FFLAGS to these, as the F90FLAGS variable
# is used in the generic rule for .f90 to .o in ../Rules.make

ifeq ($(MOSSCO_HAMSOM),true)
libmossco_hamsom_dependencies: F90FLAGS += $(HAMSOM_FFLAGS)
libmossco_hamsom_dependencies: CPPFLAGS += $(HAMSOM_CPPFLAGS)
libmossco_hamsom_dependencies:              \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/Modules.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/HAMSOM_3p.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/Main.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/Init.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/Topo.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/Boundary.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/River.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/CalcAv.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/CalcTS.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/EOS.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/SOR.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/Continuity.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/Momentum.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/Implicit.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/NonLinear.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/InOut.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/DataOut.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/AverageM2Out.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/AverageOut.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/InstantOut.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/Tools.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/Par.o) \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a($(HAMSOM_DIR)/src/Boundary_DynHeight_NWCS20.o)
else
libmossco_hamsom_dependencies:
endif

libmossco_hamsom: prefix libmossco_hamsom_dependencies \
  $(MOSSCO_LIBRARY_PATH)/libmossco_hamsom.a(hamsom_driver.o)

hamsom_driver.o: hamsom_driver.F90
	@echo "Compiling $<"
	$(F90)  $(HAMSOM_CPPFLAGS) $(CPPFLAGS) $(HAMSOM_FFLAGS) $(F90FLAGS) -c $< -o $@

hamsom_driver.F90: empty_driver.F90
	@echo "Creating dummy HAMSOM driver $@"
	sed 's/empty/hamsom/g' $< > $@

# Generic section


clean: extraclean
extraclean:
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_getm.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_benthos.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_clm.a
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_sediment.a
	@-rm -f $(MOSSCO_MODULE_PATH)/fabm_sediment_driver.mod
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_fabm0d.a
	@-rm -f $(MOSSCO_MODULE_PATH)/fabm0d_driver.mod
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_fabmgotm.a
	@-rm -f $(MOSSCO_MODULE_PATH)/fabm_gotm_driver.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/erosed_driver.mod
	@-rm -f $(MOSSCO_MODULE_PATH)/clm_driver.mod
	@-rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a
	$(RM) $(EROSED_OBJS)
