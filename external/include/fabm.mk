# This Makefile is part of MOSSCO
#
# @copyright (C) 2017, 2018 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
# @author Knut Klingbeil, Institut für Ostseeforschung Warnemünde
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

.PHONY: fabm fabm_distclean fabm_version

fabm:
ifeq ($(wildcard $(external_FABMDIR)/src/fabm.F90),)
	@$(GIT) clone -b master --depth 1 https://git.code.sf.net/p/mossco/fabm $(external_FABMDIR)
else
#	@$(GIT) -C $(external_FABMDIR) pull --ff-only
#for old git
	@( cd $(external_FABMDIR) && $(GIT) pull --ff-only )
endif

fabm_distclean:
ifeq ($(MOSSCO_FABM),true)
	@echo Cleaning the FABM library in $(FABM_PREFIX)
ifndef MOSSCO_FABM_BINARY_DIR
	$(RM) -rf $(FABM_BINARY_DIR)
endif
ifndef MOSSCO_FABM_PREFIX
	$(RM) -rf $(FABM_PREFIX)
endif
endif

fabm_version:
ifneq ($(wildcard $(external_FABMDIR)/src/fabm.F90),)
  # git describe --long --tags --dirty --always
#	FABM_VERSION=$(shell $(GIT) -C $(external_FABMDIR) log -1 --format="'%h (%ci)'")
#for old git
	FABM_VERSION=$(shell cd $(external_FABMDIR) && $(GIT) log -1 --format="'%h (%ci)'")

	@#echo "CPPFLAGS+=-DGETM_VERSION="${GETM_VERSION} >> $(MOSSCO_DIR)/src/include/versions.mk
	@#echo "CPPFLAGS+=-DGETM_GIT_SHA="${GETM_GIT_SHA} >> $(MOSSCO_DIR)/src/include/versions.mk
endif
