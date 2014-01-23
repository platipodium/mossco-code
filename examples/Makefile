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
export MOSSCO_DIR=$(subst /examples$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

# Determine examples to build
SUBDIRS = standalone
ifeq ($(MOSSCO_ESMF),true)
SUBDIRS += esmf_remtc
SUBDIRS += esmf_empty
ifeq ($(MOSSCO_EROSED),true)
SUBDIRS += esmf_erosed
endif
ifeq ($(MOSSCO_FABM),true)
ifeq ($(MOSSCO_ESMF),true)
SUBDIRS += esmf_sediment
ifeq ($(MOSSCO_GOTM),true)
SUBDIRS += esmf_gotm esmf_fabm0d
SUBDIRS += fabmsed_fabm0d
SUBDIRS += ocean_fabm0d
SUBDIRS += ocean_fabm0d_fabmsed
SUBDIRS += gotmfabm_fabmsed_constant
SUBDIRS += esmf_gotmfabm
endif
endif
endif
endif

.PHONY: all clean extraclean subdirs $(SUBDIRS) test report

default: all report

all: subdirs

subdirs:
	@for dir in $(SUBDIRS) ; do make -C $$dir ; done

clean: extraclean

extraclean:
	@for dir in $(SUBDIRS) ; do make -C $$dir clean; done

test:
	$(MAKE) -C standalone test
	@for dir in *_* ; do (cd $$dir; test -x ./$$dir && ./$$dir; ) ; done

report:
	@-echo "The following executables have been successfully generated:"
	@-find . -type f -a -perm 755

