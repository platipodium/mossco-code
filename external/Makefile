# This Makefile is part of MOSSCO
#
# @copyright (C) 2013, 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
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
.PHONY: fabm-git-clone  gotm-git-clone  getm-git-clone  erosed-svn-clone
.PHONY: fabm-git-update gotm-git-update getm-git-update erosed-svn-update

all: clone

clone:  fabm-git-clone  gotm-git-clone  getm-git-clone  #erosed-svn-clone
update: fabm-git-update gotm-git-update getm-git-update #erosed-svn-update

clean: extraclean

extraclean:
ifneq ($(wildcard $(MOSSCO_DIR)/external/fabm-git/src/Makefile),)
	$(RM) -rf $(FABM_PREFIX)
endif
ifneq ($(wildcard $(MOSSCO_DIR)/external/gotm-git/src/Makefile),)
	$(MAKE) -C $(MOSSCO_DIR)/external/gotm-git distclean
endif
ifneq ($(wildcard $(MOSSCO_DIR)/external/getm-git/src/Makefile),)
	$(MAKE) -C $(MOSSCO_DIR)/external/getm-git distclean
endif

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: fabm-git gotm-git getm-git erosed-svn

fabm-git-clone:
ifeq ($(wildcard $(MOSSCO_DIR)/external/fabm-git/src/Makefile),)
	git clone git://git.code.sf.net/p/fabm/code $(MOSSCO_DIR)/external/fabm-git
else
	$(MAKE) fabm-git-update
endif

fabm-git-update:
ifneq ($(wildcard $(MOSSCO_DIR)/external/fabm-git/src/Makefile),)
	(cd $(MOSSCO_DIR)/external/fabm-git ; git pull origin master)
endif

gotm-git-clone:
ifeq ($(wildcard $(MOSSCO_DIR)/external/gotm-git/src/Makefile),)
	git clone git://git.code.sf.net/p/gotm/code $(MOSSCO_DIR)/external/gotm-git
else
	$(MAKE) gotm-git-update
endif

gotm-git-update:
ifneq ($(wildcard $(MOSSCO_DIR)/external/gotm-git/src/Makefile),)
	(cd $(MOSSCO_DIR)/external/gotm-git ; git pull origin master)
endif

getm-git-clone:
ifeq ($(wildcard $(MOSSCO_DIR)/external/getm-git/src/Makefile),)
	git clone git://git.code.sf.net/p/getm/code $(MOSSCO_DIR)/external/getm-git
else
	$(MAKE) getm-git-update
endif

getm-git-update:
ifneq ($(wildcard $(MOSSCO_DIR)/external/getm-git/src/Makefile),)
	(cd $(MOSSCO_DIR)/external/getm-git ; git pull origin master)
endif

erosed-svn-clone:
ifeq ($(wildcard $(MOSSCO_DIR)/external/erosed-svn),)
	svn co --depth empty https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example $(MOSSCO_DIR)/external/erosed-svn
	$(MAKE) erosed-svn-update
endif

erosed-svn-update:
ifneq ($(wildcard $(MOSSCO_DIR)/external/erosed-svn),)
	svn update --set-depth infinity $(MOSSCO_DIR)/external/erosed-svn/include
	svn update --set-depth infinity $(MOSSCO_DIR)/external/erosed-svn/modules
	svn update --set-depth infinity $(MOSSCO_DIR)/external/erosed-svn/source
endif
