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
export MOSSCO_DIR=$(subst /examples/esmf$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

# Determine examples to build

SUBDIRS += clm empty remtc  benthos simplewave default

ifeq ($(MOSSCO_EROSED),true)
SUBDIRS += erosed
endif
ifeq ($(MOSSCO_FABM),true)
SUBDIRS += sediment
ifeq ($(MOSSCO_GETM),true)
SUBDIRS += getm
endif
ifeq ($(MOSSCO_GOTM),true)
SUBDIRS += gotm fabm0d
SUBDIRS += gotmfabm
SUBDIRS += pelagicfabm1d
endif
endif

.PHONY: all clean extraclean subdirs $(SUBDIRS)

all: subdirs

subdirs:
	@for dir in $(SUBDIRS) ; do make -C $$dir ; done

clean: extraclean

extraclean:
	@for dir in $(SUBDIRS) ; do make -C $$dir clean; done

