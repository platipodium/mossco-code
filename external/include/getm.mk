# This Makefile is part of MOSSCO
#
# @copyright (C) 2017 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
# @author Knut Klingbeil, Institut für Ostseeforschung Warnemünde
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

.PHONY: getm getm_distclean getm_version

getm:
ifeq ($(wildcard $(external_GETMDIR)/src/getm/main.F90),)
	@$(GIT) clone -b iow --depth 1 https://git.code.sf.net/p/getm/code $(external_GETMDIR)
else
#	@$(GIT) -C $(external_GETMDIR) pull --ff-only
#for old git
	@( cd $(external_GETMDIR) && $(GIT) pull --ff-only )
endif

getm_distclean:
ifneq ($(wildcard $(external_GETMDIR)/src/getm/main.F90),)
	@( unset FABM ; $(MAKE) -C $(external_GETMDIR) distclean )
endif

getm_version:
ifneq ($(wildcard $(external_GETMDIR)/src/getm/main.F90),)
  # git describe --long --tags --dirty --always
#	GETM_VERSION=$(shell $(GIT) -C $(external_GETMDIR) log -1 --format="'%h (%ci)'")
#for old git
	GETM_VERSION=$(shell cd $(external_GETMDIR) && $(GIT) log -1 --format="'%h (%ci)'")

	@#echo "CPPFLAGS+=-DGETM_VERSION="${GETM_VERSION} >> $(MOSSCO_DIR)/src/include/versions.mk
	@#echo "CPPFLAGS+=-DGETM_GIT_SHA="${GETM_GIT_SHA} >> $(MOSSCO_DIR)/src/include/versions.mk
endif

