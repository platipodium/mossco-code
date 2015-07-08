# This Makefile is part of MOSSCO
# 
# @copyright (C) 2013, 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/common$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

# Assemble all the ESMF C++ stuff for linking and compiling
CXXCOMPILE = $(ESMF_CXXCOMPILER) $(ESMF_OPENMP_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS)
CXXLINK = $(ESMF_CXXLINKER) $(ESMF_OPENMP_CXXLINKOPTS) $(ESMF_CXXLINKOPTS) $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKRPATHS) $(ESMF_CXXLINKLIBS) $(ESMF_CXXESMFLINKLIBS)

EXTRA_DIST=README

# Determine examples to build
SUBDIRS = 

LIBS += lmossco_util

LDFLAGS += $(MOSSCO_F90LDFLAGS)

.PHONY: all clean extraclean subdirs $(SUBDIRS) libmossco_util

all: exec

exec: toplevel_component.o main.o
	$(F90) $(F90FLAGS) -o example $^ $(LDFLAGS)
	@echo "Created example binary example in  $(PWD)"	

main.o: toplevel_component.o

toplevel_component.o: coupling_system.o toplevel_component.F90  libmossco_fabm0d libsediment libmossco_gotm libocean libmossco_util

coupling_system.o: coupling_system.F90

libmossco_fabm0d libsediment:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libocean:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/remtc $@

libmossco_gotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@


subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean: extraclean

extraclean:
	@for dir in $(SUBDIRS) ; do make -C $$dir clean; done
	$(RM) main.o toplevel_component.o coupling_system.o



test.o: test.cc
	$(CXXCOMPILE) -c $<

test: test.o
	$(CXXLINK) $<
