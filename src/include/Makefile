# This Makefile is part of MOSSCO
# 
# @copyright (C) 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/include$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

SUBDIRS=

MOSSCO_SHA=$(shell git log | head -1 | awk '{print $$2}')
MOSSCO_SHA?=none
SHA_FILE=git-sha.h

TARGETS = mossco-sha

ifdef MOSSCO_GOTM
  GOTM_SHA=$(shell (cd $(GOTMDIR) ; git log) | head -1 | awk '{print $$2}')
  GOTM_SHA?=none
  TARGETS += gotm-sha
endif
ifdef MOSSCO_GETM
  GETM_SHA=$(shell (cd $(GETMDIR) ; git log) | head -1 | awk '{print $$2}')
  GETM_SHA?=none
  TARGETS += getm-sha
endif
ifdef MOSSCO_FABM
  FABM_SHA=$(shell (cd $(FABMDIR) ; git log) | head -1 | awk '{print $$2}')
  FABM_SHA?=none
  TARGETS += fabm-sha
endif
ifdef MOSSCO_EROSED
  EROSED_REV=$(shell (cd $(EROSED_DIR) ; svn log) | head -2 | tail -1 | awk '{print $$1}')
  EROSED_REV?=none
  TARGETS += erosed-rev
endif

.PHONY: sha

all: $(TARGETS)

clean: extraclean

git-sha.h:
	@-touch git-sha.h

mossco-sha: git-sha.h
	@-if [  $(shell grep -c $(MOSSCO_SHA) $(SHA_FILE))  -eq 0 ] ; then \
	  if [ $(shell grep -c MOSSCO_GIT_SHA_KEY $(SHA_FILE)) -gt 0 ] ; then \
	    sed -i .tmp '/MOSSCO_GIT_SHA_KEY/d' $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: MOSSCO_GIT_SHA_KEY = "'$(MOSSCO_SHA)'"'	 >> $(SHA_FILE) ; \
	fi  

fabm-sha:  git-sha.h
	@-if [  $(shell grep -c $(FABM_SHA) $(SHA_FILE)) -eq 0 ] ; then \
	  if [ $(shell grep -c FABM_GIT_SHA_KEY $(SHA_FILE)) -gt 0 ] ; then \
	    sed -i .tmp '/FABM_GIT_SHA_KEY/d' $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: FABM_GIT_SHA_KEY = "'$(FABM_SHA)'"'	 >> $(SHA_FILE) ; \
	fi  

gotm-sha:  git-sha.h
	@-if [  $(shell grep -c $(GOTM_SHA) $(SHA_FILE)) -eq 0 ] ; then \
	  if [ $(shell grep -c GOTM_GIT_SHA_KEY $(SHA_FILE)) -gt 0 ] ; then \
	    sed -i .tmp '/GOTM_GIT_SHA_KEY/d' $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: GOTM_GIT_SHA_KEY = "'$(GOTM_SHA)'"'	 >> $(SHA_FILE) ; \
	fi  

getm-sha:
	@-if [  $(shell grep -c $(GETM_SHA) $(SHA_FILE)) -eq 0 ] ; then \
	  if [ $(shell grep -c GETM_GIT_SHA_KEY $(SHA_FILE)) -gt 0 ] ; then \
	    sed -i .tmp '/GETM_GIT_SHA_KEY/d' $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: GETM_GIT_SHA_KEY = "'$(GETM_SHA)'"'	 >> $(SHA_FILE) ; \
	fi  

erosed-rev:  git-sha.h
	@-if [  $(shell grep -c $(EROSED_REV) $(SHA_FILE)) == 0 ] ; then \
	  if [ $(shell grep -c EROSED_SVN_REVISION $(SHA_FILE)) > 0 ] ; then \
	    sed -i .tmp '/EROSED_SVN_REVISION/d' $(SHA_FILE) ; \
	  fi ; \
	  echo 'character(len=40), parameter :: EROSED_SVN_REVISION = "'$(EROSED_REV)'"'	 >> $(SHA_FILE) ; \
	fi  

extraclean:
	@- for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done 
	@- rm -f $(SHA_FILE) *.tmp
