# This Makefile is part of MOSSCO
#
# @copyright (C) 2019 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
# @author Knut Klingbeil, Institut für Ostseeforschung Warnemünde
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

MOSSCO_GETM_BUILD_DIR=$(GETMDIR)/../build
ifndef GETM_PREFIX
GETM_PREFIX=$(GETMDIR)/../install
endif

.PHONY: libgetm_external libgetm_build

libgetm_external: libgetm_build libgetm_install

libgetm_build:
	@mkdir -p $(MOSSCO_GETM_BUILD_DIR)
	(cd $(MOSSCO_GETM_BUILD_DIR) && cmake $(GETMDIR)/src -DCMAKE_INSTALL_PREFIX=$(GETM_PREFIX) -DGOTM_BASE=$(GOTMDIR) -DGETM_FLAGS="$(GETM_FFLAGS)" -DCMAKE_Fortran_COMPILER=$(FC))

libgetm_install:
	@mkdir -p $(GETM_PREFIX)/include
	@echo Recreating the GETM library in $(GETM_PREFIX)
	$(MAKE) -sC $(MOSSCO_GETM_BUILD_DIR) install
	cp $(MOSSCO_GETM_BUILD_DIR)/*.mod $(GETM_PREFIX)/include/
