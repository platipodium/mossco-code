# This Makefile is part of MOSSCO
# 
# Copyright (C) 2013 Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/standalone$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

# Determine examples to build
SUBDIRS = 
ifeq ($(MOSSCO_FABM),true)
SUBDIRS += omexdia_p
endif

ifeq ($(MOSSCO_EROSED),true)
SUBDIRS += erosed
endif

.PHONY: all clean extraclean subdirs test

default: all

all: subdirs

subdirs:
	@for dir in $(SUBDIRS) ; do make -C $$dir ; done

clean: extraclean

extraclean:
	@for dir in $(SUBDIRS) ; do make -C $$dir clean; done

test:
	for dir in omexdia_p erosed; do (cd $$dir; test -x ./$$dir && ./$$dir) ; done
	
