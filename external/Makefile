# This Makefile is part of MOSSCO
#
# @copyright (C) 2013, 2014, 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht, Institut für Ostseeforschung Warnemünde
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
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

GIT_CLONE_TARGETS += fabm_clone  gotm_clone  #json_clone
GIT_UPDATE_TARGETS += fabm_update  gotm_update  #json_update
CVS_CLONE_TARGTES += flibs-cvs-clone #json_clone
CVS_UPDATE_TARGTES += flibs-cvs-update
SVN_CLONE_TARGETS += #erosed-svn-clone
SVN_UPDATE_TARGETS += #erosed-svn-update
DISTCLEAN_TARGETS +=  gotm_clean fabm_clean #json_clean
VERSION_TARGETS +=

EXTRA_DIST =
SUBDIRS =

CVS?=$(shell which cvs)
SVN?=$(shell which svn)
GIT?=$(shell which git)
PYTHON?=$(shell which python)
PREFIX?=$(HOME)/opt

include include/*mk

ifeq ($(GIT),)
  $(error "could not find `git´ executable")
	GIT_TARGETS=
	GIT_UPDATE_TARGETS=
	GIT_CLONE_TARGETS=
endif

ifeq ($(PYTHON),)
  $(error "could not find `git´ executable")
  PYTHON_CLONE_TARGETS=
else
  PYTHON_CLONE_TARGETS+=python-clone
endif

ifeq ($(SVN),)
  $(warning "could not find `svn´ executable")
  SVN_CLONE_TARGETS=
endif

ifeq ($(CVS),)
  $(warning "could not find `cvs´ executable")
  CVS_CLONE_TARGETS=
endif

.PHONY: all clean extraclean subdirs $(SUBDIRS) clone update
.PHONY: fabm_clone  gotm_clone  erosed-svn-clone flibs-cvs-clone json_clone
.PHONY: fabm_update gotm_update erosed-svn-update flibs-cvs-update json_update
.PHONY: sqlite-zip sqlite


all: clone update version
clone: $(GIT_CLONE_TARGETS) $(SVN_CLONE_TARGETS) $(CVS_CLONE_TARGETS) $(GIT_TARGETS)
update: $(GIT_UPDATE_TARGETS) $(SVN_UPDATE_TARGETS) $(CVS_UPDATE_TARGETS) $(GIT_TARGETS)
version: $(VERSION_TARGETS)

clean: extraclean

distclean: $(DISTCLEAN_TARGETS)

extraclean:

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

sqlite: sqlite/sqlite3.c
sqlite/sqlite3.c: sqlite-zip
sqlite-zip:
	test -f sqlite3.zip || \
	wget -O sqlite3.zip http://sqlite.org/2015/sqlite-amalgamation-3081002.zip
	unzip -u -d sqlite -j sqlite3.zip
	rm -f sqlite3.zip

fabm_clone fabm_update:
ifeq ($(wildcard $(external_FABMDIR)/src/fabm.F90),)
	$(GIT) clone https://github.com/fabm-model/fabm.git $(external_FABMDIR)
else
	( cd $(external_FABMDIR) && $(GIT) pull )
endif
ifeq ($(wildcard $(external_FABMDIR)/.git/refs/remotes/mossco/master),)
	( cd $(external_FABMDIR) && $(GIT) remote add -f mossco http://git.code.sf.net/p/mossco/fabm && $(GIT) checkout -b mossco mossco/master )
endif

json_clone json_update:
ifeq ($(wildcard $(external_JSONDIR)/src/json_module.F90),)
	$(GIT) clone https://github.com/jacobwilliams/json-fortran.git $(external_JSONDIR)
else
	( cd $(external_JSONDIR) && $(GIT) pull )
endif

gotm_clone gotm_update:
ifeq ($(wildcard $(external_GOTMDIR)/src/CMakeLists.txt),)
	$(GIT) clone https://github.com/gotm-model/code.git $(external_GOTMDIR)
else
	( cd $(external_GOTMDIR) ; $(GIT) pull )
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
	echo "/1 :pserver:anonymous@flibs.cvs.sourceforge.net:2401/cvsroot/flibs A" >> $(HOME)/.cvspass
	#cvs -d:pserver:anonymous@flibs.cvs.sourceforge.net:/cvsroot/flibs login
	$(MAKE) flibs-cvs-update
endif

flibs-cvs-update:
	(cd $(MOSSCO_DIR)/external/flibs-cvs; \
	cvs -z3 -d:pserver:anonymous@flibs.cvs.sourceforge.net:/cvsroot/flibs checkout configure.sh config src make )

pyyaml-svn-clone pyyaml-svn-update:
ifeq ($(wildcard $(MOSSCO_DIR)/external/pyyaml-svn),)
	svn checkout http://svn.pyyaml.org/pyyaml/trunk pyyaml-svn
else
	svn update $(MOSSCO_DIR)/external/pyyaml-svn
endif
	mkdir -p $(PREFIX)
	(cd $(MOSSCO_DIR)/external/pyyaml-svn;  $(PYTHON) setup.py install --prefix=$(PREFIX))
	export PYTHONPATH=$(PYTHONPATH):$(PREFIX)

gitstats-git-clone gitstats-git-update:
ifeq ($(wildcard $(external_GITSTATSDIR)/src),)
	$(GIT) clone git://github.com/hoxu/gitstats.git $(external_GITSTATSDIR)
else
	( cd $(external_GITSTATSDIR) && $(GIT) pull )
endif
	mkdir -p $(PREFIX)
	make install PREFIX=$(PREFIX)
