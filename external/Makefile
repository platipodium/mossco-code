# This Makefile is part of MOSSCO
#
# @copyright 2021-2023 Helmholtz-Zentrum hereon GmbH
# @copyright 2013-2021 Helmholtz-Zentrum Geesthacht GmbH
# @copyright 2013-2018 Institut für Ostseeforschung Warnemünde
# @author Carsten Lemmen <carsten.lemmen@hereon.de>
# @author Knut Klingbeil <knut.klingbeil@io-warnemuende.de>
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

CVS?=$(shell which cvs)
SVN?=$(shell which svn)

# Test for the version of git and decide whether to make
# a shallow copy (from version 2)
ifeq ($(MOSSCO_GIT),true)
  GIT?=$(shell which git)
  ifneq ($(MOSSCO_GIT_VERSION_MAJOR),1)
		GIT_CLONE:=$(GIT) clone --depth=1
	else
	  GIT_CLONE:=$(GIT) clone
	endif
endif

PYTHON?=$(shell which python)
PREFIX?=$(HOME)/opt

include include/*mk

ifeq ($(GIT),)
  $(error "could not find `git´ executable")
endif

ifeq ($(PYTHON),)
  $(warning "could not find `python´ executable")
endif

ifeq ($(SVN),)
  $(warning "could not find `svn´ executable")
endif

ifeq ($(CVS),)
  $(warning "could not find `cvs´ executable")
endif

.PHONY: all clean external_clean subdirs $(SUBDIRS) clone update
.PHONY: fabm gotm erosed flibs json
.PHONY: sqlite-zip sqlite


all: fabm gotm getm version
clone: all
update: all
version: fabm_version gotm_version getm_version

clean:

external_clean: fabm_distclean gotm_distclean getm_distclean

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

json:
ifeq ($(wildcard $(external_JSONDIR)/src/json_module.F90),)
	$(GIT_CLONE) https://github.com/jacobwilliams/json-fortran.git $(external_JSONDIR)
else
	( cd $(external_JSONDIR) && $(GIT) pull )
endif

erosed:
ifeq ($(wildcard $(MOSSCO_DIR)/external/erosed-svn),)
	svn co --depth empty https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example $(MOSSCO_DIR)/external/erosed-svn
endif
	$(MAKE) erosed-svn-update
	$(MAKE) flow2d3d-svn-export

erosed-svn-update:
	svn update --set-depth infinity $(MOSSCO_DIR)/external/erosed-svn/include
	svn update --set-depth infinity $(MOSSCO_DIR)/external/erosed-svn/modules
	svn update --set-depth infinity $(MOSSCO_DIR)/external/erosed-svn/source

flow2d3d-svn-export:
	svn export --force https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/2399/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/bedbc1993.f90 $(MOSSCO_DIR)/external/erosed-svn/source/bedbc1993.f90
	svn export --force https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/2399/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/soursin_3d.f90 $(MOSSCO_DIR)/external/erosed-svn/source/soursin_3d.f90
	svn export --force https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/2399/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/compbsskin.f90 $(MOSSCO_DIR)/external/erosed-svn/source/compbsskin.f90
	svn export --force https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/2399/src/utils_lgpl/deltares_common/packages/deltares_common/src/mathconsts.f90 $(MOSSCO_DIR)/external/erosed-svn/source/mathconsts.f90

sqlite: sqlite/sqlite3.c
sqlite/sqlite3.c: sqlite-zip
sqlite-zip:
	test -f sqlite3.zip || \
	wget -O sqlite3.zip http://sqlite.org/2015/sqlite-amalgamation-3081002.zip
	unzip -u -d sqlite -j sqlite3.zip
	rm -f sqlite3.zip

flibs:
ifeq ($(wildcard $(MOSSCO_DIR)/external/flibs-cvs),)
	mkdir -p $(MOSSCO_DIR)/external/flibs-cvs
	echo "/1 :pserver:anonymous@flibs.cvs.sourceforge.net:2401/cvsroot/flibs A" >> $(HOME)/.cvspass
	#cvs -d:pserver:anonymous@flibs.cvs.sourceforge.net:/cvsroot/flibs login
	$(MAKE) flibs-cvs-update
else
	(cd $(MOSSCO_DIR)/external/flibs-cvs; \
	cvs -z3 -d:pserver:anonymous@flibs.cvs.sourceforge.net:/cvsroot/flibs checkout configure.sh config src make )
endif

pyyaml:
ifeq ($(wildcard $(MOSSCO_DIR)/external/pyyaml-svn),)
	svn checkout http://svn.pyyaml.org/pyyaml/trunk pyyaml-svn
else
	svn update $(MOSSCO_DIR)/external/pyyaml-svn
endif
	mkdir -p $(PREFIX)
	(cd $(MOSSCO_DIR)/external/pyyaml-svn;  $(PYTHON) setup.py install --prefix=$(PREFIX))
	export PYTHONPATH=$(PYTHONPATH):$(PREFIX)

gitstats:
ifeq ($(wildcard $(external_GITSTATSDIR)/src),)
	$(GIT_CLONE) git://github.com/hoxu/gitstats.git $(external_GITSTATSDIR)
else
	( cd $(external_GITSTATSDIR) && $(GIT) pull )
endif
	mkdir -p $(PREFIX)
	make install PREFIX=$(PREFIX)
