# This Makefile is part of MOSSCO
#
# @copyright (C) 2013, 2014, 2015 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
# @author Knut Klingbeil, Institut für Ostseeforschung Warnemünde
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /external$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST =
SUBDIRS =

.PHONY: all clean extraclean subdirs $(SUBDIRS) clone update
.PHONY: fabm_clone  gotm_clone  getm_clone  erosed-svn-clone flibs-cvs-clone
.PHONY: fabm_update gotm_update getm_update erosed-svn-update flibs-cvs-update

all: clone

clone:  fabm_clone  gotm_clone  getm_clone  flibs-cvs-clone #erosed-svn-clone
update: fabm_update gotm_update getm_update flibs-cvs-update #erosed-svn-update

clean: extraclean

distclean: getm_distclean gotm_distclean fabm_clean

extraclean:

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

fabm_clone:
ifeq ($(wildcard $(external_FABMDIR)/src/fabm.F90),)
	git clone http://git.code.sf.net/p/fabm/code $(external_FABMDIR)
else
	$(MAKE) fabm_update
endif

fabm_update:
ifneq ($(wildcard $(external_FABMDIR)/src/fabm.F90),)
	( cd $(external_FABMDIR) ; git pull origin master )
endif

gotm_clone:
ifeq ($(wildcard $(external_GOTMDIR)/src/Makefile),)
	git clone http://git.code.sf.net/p/gotm/code $(external_GOTMDIR)
else
	$(MAKE) gotm_update
endif

gotm_update:
ifneq ($(wildcard $(external_GOTMDIR)/src/Makefile),)
	( cd $(external_GOTMDIR) ; git pull origin master )
endif

gotm_distclean:
ifneq ($(wildcard $(external_GOTMDIR)/src/Makefile),)
	( unset FABM ; $(MAKE) -C $(external_GOTMDIR) distclean )
endif

getm_clone:
ifeq ($(wildcard $(external_GETMDIR)/src/Makefile),)
	git clone http://git.code.sf.net/p/getm/code $(external_GETMDIR)
	( cd $(external_GETMDIR) ; git checkout -b iow origin/iow )
else
	$(MAKE) getm_update
endif

getm_update:
ifneq ($(wildcard $(external_GETMDIR)/src/Makefile),)
	( cd $(external_GETMDIR) ; git pull origin iow)
endif

getm_distclean:
ifneq ($(wildcard $(external_GETMDIR)/src/Makefile),)
	( unset FABM ; $(MAKE) -C $(external_GETMDIR) distclean )
endif

erosed-svn-clone:
ifeq ($(wildcard $(MOSSCO_DIR)/external/erosed-svn),)
	svn co --depth empty https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example $(MOSSCO_DIR)/external/erosed-svn
	$(MAKE) erosed-svn-update
	$(MAKE) flow2d3d-svn-export
endif

erosed-svn-update:
ifneq ($(wildcard $(MOSSCO_DIR)/external/erosed-svn),)
	svn update --set-depth infinity $(MOSSCO_DIR)/external/erosed-svn/include
	svn update --set-depth infinity $(MOSSCO_DIR)/external/erosed-svn/modules
	svn update --set-depth infinity $(MOSSCO_DIR)/external/erosed-svn/source
endif

flow2d3d-svn-export:
	svn export --force https://svn.oss.deltares.nl/repos/delft3d/tags/2399/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/bedbc1993.f90 $(MOSSCO_DIR)/external/erosed-svn/source
	svn export --force https://svn.oss.deltares.nl/repos/delft3d/tags/2399/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/soursin_3d.f90 $(MOSSCO_DIR)/external/erosed-svn/source
	svn export --force https://svn.oss.deltares.nl/repos/delft3d/tags/2399/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/compbsskin.f90 $(MOSSCO_DIR)/external/erosed-svn/source
	svn export --force https://svn.oss.deltares.nl/repos/delft3d/tags/2399/src/utils_lgpl/deltares_common/packages/deltares_common/src/mathconsts.f90 $(MOSSCO_DIR)/external/erosed-svn/source

flibs-cvs-clone:
ifeq ($(wildcard $(MOSSCO_DIR)/external/flibs-cvs),)
	mkdir -p $(MOSSCO_DIR)/external/flibs-cvs
	cvs -d:pserver:anonymous@flibs.cvs.sourceforge.net:/cvsroot/flibs login
	$(MAKE) flibs-cvs-update
endif

flibs-cvs-update:
	(cd $(MOSSCO_DIR)/external/flibs-cvs; \
	cvs -z3 -d:pserver:anonymous@flibs.cvs.sourceforge.net:/cvsroot/flibs checkout src )
