# This Makefile is part of MOSSCO
#
# SPDX-FileCopyrightText 2021-2022 Helmholtz-Zentrum Hereon
# SPDX-FileCopyrightText 2014-2021 Helmholtz-Zentrum Geesthacht
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileContributor Carsten Lemmen <carsten.lemmen@hereon.de
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/include$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README
SHELL=bash

SUBDIRS=

GIT?=$(shell which git)
CVS?=$(shell which cvs)
SVN?=$(shell which svn)
SED?=$(shell which gsed 2> /dev/null)
SED?=$(shell which sed)

# Fix for ksez
ifeq ($(SED),)
  SED=sed
endif

ifeq ($(SED),)
  $(warning GIT=$(GIT) CVS=$(CVS) SVN=$(SVN) SED=$(SED))
  $(error "could not find `sed´ executable, cannot proceed")
endif


ifeq ($(GIT),)
  $(warning "could not find `git´ executable, thus versions will be incorrect.")
else
  MOSSCO_SHA=$(shell git log | head -1 | awk '{print $$2}')
endif

MOSSCO_SHA?=none
SHA_FILE=git-sha.h

TARGETS = mossco-sha

ifdef MOSSCO_SETUPDIR
ifneq ($(wildcard $(MOSSCO_SETUPDIR)/.git),)
ifneq ($(GIT),)
  MOSSCO_SETUP_SHA=$(shell (cd $(MOSSCO_SETUPDIR) ; git log -n1 ) | head -1 | awk '{print $$2}')
endif
  MOSSCO_SETUP_SHA?=none
  TARGETS += mossco-setup-sha
endif
endif
ifeq ($(MOSSCO_GOTM),true)
ifneq ($(GIT),)
  GOTM_SHA=$(shell (cd $(GOTMDIR) ; git log -n1) | head -1 | awk '{print $$2}')
endif
  GOTM_SHA?=none
  TARGETS += gotm-sha
endif
ifeq ($(MOSSCO_GETM),true)
ifneq ($(GIT),)
  GETM_SHA=$(shell (cd $(GETMDIR) ; git log -n1) | head -1 | awk '{print $$2}')
endif
  GETM_SHA?=none
  TARGETS += getm-sha
endif
ifeq ($(MOSSCO_FABM),true)
ifneq ($(GIT),)
  FABM_SHA=$(shell (cd $(FABMDIR) ; git log -n1) | head -1 | awk '{print $$2}')
endif
  FABM_SHA?=none
  TARGETS += fabm-sha
#  $(info fabm-sha settings)
endif
ifeq ($(MOSSCO_EROSED),true)
  TARGETS += erosed-rev
endif

.PHONY: sha

all: create-file $(TARGETS)

clean: extraclean

git-sha.h:
	#@-echo '#ifndef GIT_SHA_INCLUDED' > $(SHA_FILE)
	#@-echo '#define GIT_SHA_INCLUDED' >> $(SHA_FILE)

create-file:
	@-if [ ! -f $(SHA_FILE) ] ; then touch $(SHA_FILE); fi

mossco-sha: git-sha.h
	@-if [  $(shell grep -c $(MOSSCO_SHA) $(SHA_FILE))  -eq 0 ] ; then \
	  if [ $(shell grep -c MOSSCO_GIT_SHA_KEY $(SHA_FILE)) -gt 0 ] ; then \
	    $(SED) -i '/MOSSCO_GIT_SHA_KEY/d' $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: MOSSCO_GIT_SHA_KEY = "'$(MOSSCO_SHA)'"'	 >> $(SHA_FILE) ; \
	fi

mossco-setup-sha: git-sha.h
	@-if [  $(shell grep -c $(MOSSCO_SETUP_SHA) $(SHA_FILE))  -eq 0 ] ; then \
	  if [ $(shell grep -c MOSSCO_SETUP_GIT_SHA_KEY $(SHA_FILE)) -gt 0 ] ; then \
	    $(SED) -i '/MOSSCO_SETUP_GIT_SHA_KEY/d' $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: MOSSCO_SETUP_GIT_SHA_KEY = "'$(MOSSCO_SETUP_SHA)'"'	 >> $(SHA_FILE) ; \
	fi

fabm-sha:  git-sha.h
	@-if [  $(shell grep -c $(FABM_SHA) $(SHA_FILE)) -eq 0 ] ; then \
	  if [ $(shell grep -c FABM_GIT_SHA_KEY $(SHA_FILE)) -gt 0 ] ; then \
	    $(SED) -i '/FABM_GIT_SHA_KEY/d' $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: FABM_GIT_SHA_KEY = "'$(FABM_SHA)'"'	 >> $(SHA_FILE) ; \
	fi

gotm-sha:  git-sha.h
	@-if [  $(shell grep -c $(GOTM_SHA) $(SHA_FILE)) -eq 0 ] ; then \
	  if [ $(shell grep -c GOTM_GIT_SHA_KEY $(SHA_FILE)) -gt 0 ] ; then \
	    $(SED) -i '/GOTM_GIT_SHA_KEY/d' $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: GOTM_GIT_SHA_KEY = "'$(GOTM_SHA)'"'	 >> $(SHA_FILE) ; \
	fi

getm-sha:
	@-if [  $(shell grep -c $(GETM_SHA) $(SHA_FILE)) -eq 0 ] ; then \
	  if [ $(shell grep -c GETM_GIT_SHA_KEY $(SHA_FILE)) -gt 0 ] ; then \
	    $(SED) -i '/GETM_GIT_SHA_KEY/d' $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: GETM_GIT_SHA_KEY = "'$(GETM_SHA)'"'	 >> $(SHA_FILE) ; \
	fi

ifeq ($(MOSSCO_EROSED),true)
.PHONY: erosed-rev
erosed-rev: erosed.rev git-sha.h
	@$(SED) -i '/EROSED_SVN_REVISION/d' $(SHA_FILE)
	@echo 'character(len=40), parameter :: EROSED_SVN_REVISION = "'`cat erosed.rev 2> /dev/null || echo none`'"'	 >> $(SHA_FILE)

erosed.rev: $(EROSED_DIR)/source/erosed.f90
	@svn log $(EROSED_DIR)/source/erosed.f90 2> /dev/null | head -2 | tail -1 | awk '{print $$1}' > erosed.rev
	@if [ ! -s erosed.rev ] ; then rm -f erosed.rev ; fi
endif

extraclean:
	@- for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done
	@- rm -f $(SHA_FILE) erosed.rev *.swp
