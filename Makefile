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

EXTRA_DIST = README ACKNOWLEDGEMENTS AUTHORS .gitignore
SUBDIRS = doc src examples

export MOSSCO_DIR=$(CURDIR)
export MOSSCO_DATE=$(shell date "+%Y%m%d")

include $(MOSSCO_DIR)/src/Rules.make

.PHONY: default doc src info examples all clean subdirs $(SUBDIRS)

default: src
all:  examples doc
examples: src

clean:
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done 
	@rm -rf modules lib bin

distclean_all: clean
ifneq ($(wildcard $(MOSSCO_DIR)/external/fabm-git/src/Makefile),)
	$(MAKE) -C $(MOSSCO_DIR)/external/fabm-git/src $@
endif
ifneq ($(wildcard $(MOSSCO_DIR)/external/gotm-git/src/Makefile),)
	$(MAKE) -C $(MOSSCO_DIR)/external/gotm-git $@
endif
ifneq ($(wildcard $(MOSSCO_DIR)/external/getm-git/src/Makefile),)
	$(MAKE) -C $(MOSSCO_DIR)/external/getm-git $@
endif

all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

check: 
	make -C src check

update:
	git pull 

info:
	make -C src info

run: examples
	(cd examples/omexdia_p && ./omexdia_p_test)
	(cd examples/esmf_sediment && ./esmf_sediment_test)
	(cd examples/esmf_gotm && ./esmf_gotm_example)

archive:
	@git archive --format=tar.gz --prefix=mossco-$(MOSSCO_DATE)/ HEAD > $(MOSSCO_DIR)/../mossco-$(MOSSCO_DATE).tar.gz
ifdef MOSSCO_SF_USER
	rsync -e ssh -t $(MOSSCO_DIR)/../mossco-$(MOSSCO_DATE).tar.gz $(MOSSCO_SF_USER)@frs.sf.net:/home/pfs/p/mossco/Snapshots/
else
	@echo "Please set the environment variable MOSSCO_SF_USER to your sourceforge user name."
endif

.PHONY: external fabm-git gotm-git getm-git erosed-svn
external: fabm-git gotm-git getm-git #erosed-svn

fabm-git:
ifeq ($(wildcard $(MOSSCO_DIR)/external/fabm-git/src/Makefile),)
	git clone git://git.code.sf.net/p/fabm/code $(MOSSCO_DIR)/external/fabm-git
else
	(cd $(MOSSCO_DIR)/external/fabm-git ; git pull)
endif

gotm-git:
ifeq ($(wildcard $(MOSSCO_DIR)/external/gotm-git/src/Makefile),)
	git clone git://git.code.sf.net/p/gotm/code $(MOSSCO_DIR)/external/gotm-git
else
	(cd $(MOSSCO_DIR)/external/gotm-git ; git pull)
endif

getm-git:
ifeq ($(wildcard $(MOSSCO_DIR)/external/getm-git/src/Makefile),)
	git clone git://git.code.sf.net/p/getm/code $(MOSSCO_DIR)/external/getm-git
else
	(cd $(MOSSCO_DIR)/external/getm-git ; git pull)
endif

erosed-svn:
ifeq ($(wildcard $(MOSSCO_DIR)/external/erosed-svn),)
	svn co --depth empty https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example $(MOSSCO_DIR)/external/erosed-svn
endif
	svn update --set-depth infinity $(MOSSCO_DIR)/external/erosed-svn/include
	svn update --set-depth infinity $(MOSSCO_DIR)/external/erosed-svn/modules
	svn update --set-depth infinity $(MOSSCO_DIR)/external/erosed-svn/source
