# This Makefile is part of MOSSCO
#
# @copyright (C) 2014, 2015 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
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

SUBDIRS=

GIT?=$(shell which git)
CVS?=$(shell which cvs)
SVN?=$(shell which svn)

ifeq ($(GIT),)
  $(warning "could not find `git´ executable, thus versions will be incorrect.")
else
  MOSSCO_SHA=$(shell git log | head -1 | awk '{print $$2}')
endif

MOSSCO_SHA?=none
SHA_FILE=git-sha.h

TARGETS = mossco-sha

ifdef MOSSCO_SETUPDIR
ifneq ($(wildcard $(MOSSCO_SETUPDIR)),)
ifneq ($(GIT),)
  MOSSCO_SETUP_SHA=$(shell (cd $(MOSSCO_SETUPDIR) ; git log) | head -1 | awk '{print $$2}')
endif
  MOSSCO_SETUP_SHA?=none
  TARGETS += mossco-setup-sha
endif
endif
ifeq ($(MOSSCO_GOTM),true)
ifneq ($(GIT),)
  GOTM_SHA=$(shell (cd $(GOTMDIR) ; git log) | head -1 | awk '{print $$2}')
endif
  GOTM_SHA?=none
  TARGETS += gotm-sha
endif
ifeq ($(MOSSCO_GETM),true)
ifneq ($(GIT),)
  GETM_SHA=$(shell (cd $(GETMDIR) ; git log) | head -1 | awk '{print $$2}')
endif
  GETM_SHA?=none
  TARGETS += getm-sha
endif
ifeq ($(MOSSCO_FABM),true)
ifneq ($(GIT),)
  FABM_SHA=$(shell (cd $(FABMDIR) ; git log) | head -1 | awk '{print $$2}')
endif
  FABM_SHA?=none
  TARGETS += fabm-sha
#  $(info fabm-sha settings)
endif
ifeq ($(MOSSCO_EROSED),true)
ifneq ($(SVN),)
  EROSED_REV=$(shell (cd $(EROSED_DIR) ; svn log) | head -2 | tail -1 | awk '{print $$1}')
endif
  EROSED_REV?=none
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
	    cat $(SHA_FILE) | sed  '/MOSSCO_GIT_SHA_KEY/d'  > $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: MOSSCO_GIT_SHA_KEY = "'$(MOSSCO_SHA)'"'	 >> $(SHA_FILE) ; \
	fi

mossco-setup-sha: git-sha.h
	@-if [  $(shell grep -c $(MOSSCO_SETUP_SHA) $(SHA_FILE))  -eq 0 ] ; then \
	  if [ $(shell grep -c MOSSCO_SETUP_GIT_SHA_KEY $(SHA_FILE)) -gt 0 ] ; then \
	    cat $(SHA_FILE) | sed  '/MOSSCO_SETUP_GIT_SHA_KEY/d'  > $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: MOSSCO_SETUP_GIT_SHA_KEY = "'$(MOSSCO_SETUP_SHA)'"'	 >> $(SHA_FILE) ; \
	fi

fabm-sha:  git-sha.h
	@-if [  $(shell grep -c $(FABM_SHA) $(SHA_FILE)) -eq 0 ] ; then \
	  if [ $(shell grep -c FABM_GIT_SHA_KEY $(SHA_FILE)) -gt 0 ] ; then \
	    cat $(SHA_FILE) | sed  '/FABM_GIT_SHA_KEY/d'  > $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: FABM_GIT_SHA_KEY = "'$(FABM_SHA)'"'	 >> $(SHA_FILE) ; \
	fi

gotm-sha:  git-sha.h
	@-if [  $(shell grep -c $(GOTM_SHA) $(SHA_FILE)) -eq 0 ] ; then \
	  if [ $(shell grep -c GOTM_GIT_SHA_KEY $(SHA_FILE)) -gt 0 ] ; then \
	    cat $(SHA_FILE) | sed  '/GOTM_GIT_SHA_KEY/d'  > $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: GOTM_GIT_SHA_KEY = "'$(GOTM_SHA)'"'	 >> $(SHA_FILE) ; \
	fi

getm-sha:
	@-if [  $(shell grep -c $(GETM_SHA) $(SHA_FILE)) -eq 0 ] ; then \
	  if [ $(shell grep -c GETM_GIT_SHA_KEY $(SHA_FILE)) -gt 0 ] ; then \
	    cat $(SHA_FILE) | sed  '/GETM_GIT_SHA_KEY/d'  > $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: GETM_GIT_SHA_KEY = "'$(GETM_SHA)'"'	 >> $(SHA_FILE) ; \
	fi

erosed-rev:  git-sha.h
	@-if [  $(shell grep -c $(EROSED_REV) $(SHA_FILE)) -eq 0 ] ; then \
	  if [ $(shell grep -c EROSED_SVN_REVISION $(SHA_FILE)) -gt 0 ] ; then \
	    cat $(SHA_FILE) | sed  '/EROSED_SVN_REVISION/d'  > $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: EROSED_SVN_REVISION = "'$(EROSED_REV)'"'	 >> $(SHA_FILE) ; \
	fi

extraclean:
	@- for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done
	@- rm -f $(SHA_FILE) *.swp
