# This Makefile snippet is part of MOSSCO.  It sets make infrastructure
# for the external SCHISM model
#
# @copyright (C) 2018 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hereon.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

MOSSCO_SCHISM=false

# Please redefine the SCHISM prefix or its binary dir outside
# of MOSSCO, i.e. in your environment
SCHISM_PREFIX=
SCHISM_BINARY_DIR=

ifdef MOSSCO_SCHISM_BINARY_DIR
  export SCHISM_BINARY_DIR=$(MOSSCO_SCHISM_BINARY_DIR)
  #export SCHISM_PREFIX=$(shell grep CMAKE_INSTALL_PREFIX $(MOSSCO_SCHISM_BINARY_DIR)/CMakeCache.txt | cut -d "=" -f2)
	export SCHISM_PREFIX=$(SCHISM_BINARY_DIR)/..
endif

ifdef MOSSCO_SCHISM_PREFIX
  export SCHISM_PREFIX=$(MOSSCO_SCHISM_PREFIX)
endif

ifneq ($(SCHISM_PREFIX),)
  	MOSSCO_SCHISM=true
endif

export MOSSCO_SCHISM

libschism_external: libschism_build libschism_install

libschism_build:
ifeq ($(MOSSCO_SCHISM),true)
ifndef MOSSCO_SCHISM_BINARY_DIR
		@mkdir -p $(SCHISM_BINARY_DIR)
		(cd $(SCHISM_BINARY_DIR) && cmake $(SCHISM_PREFIX)/src -DTVD_LIM=SB
		(cd $(SCHISM_PREFIX)/schism_esmf; make clean all )
endif
endif

libschism_install:
ifeq ($(MOSSCO_SCHISM),true)
ifdef SCHISM_BINARY_DIR
		@echo Recreating the SCHISM library in $(SCHISM_PREFIX)
		$(MAKE) -sC $(SCHISM_BINARY_DIR)
endif
endif
